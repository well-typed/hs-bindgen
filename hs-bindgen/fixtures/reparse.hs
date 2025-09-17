[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "A",
      newtypeConstr = HsName
        "@NsConstr"
        "A",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_A",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:3:9",
          declId = NamePair {
            nameC = Name "A",
            nameHsIdent = HsIdentifier "A"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "A",
              newtypeField = HsName
                "@NsVar"
                "un_A"},
            macroType = TypePrim
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
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "A",
          commentLocation = Just
            "reparse.h:3:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
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
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
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
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
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
        "A",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "A",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Some_struct",
      structConstr = HsName
        "@NsConstr"
        "Some_struct",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "reparse.h:7:8",
            declId = NamePair {
              nameC = Name "some_struct",
              nameHsIdent = HsIdentifier
                "Some_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["reparse.h"],
                headerInclude = "reparse.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Some_struct"),
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
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "some_struct",
          commentLocation = Just
            "reparse.h:7:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Some_struct",
          structConstr = HsName
            "@NsConstr"
            "Some_struct",
          structFields = [],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "reparse.h:7:8",
                declId = NamePair {
                  nameC = Name "some_struct",
                  nameHsIdent = HsIdentifier
                    "Some_struct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["reparse.h"],
                    headerInclude = "reparse.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Some_struct"),
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
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "some_struct",
              commentLocation = Just
                "reparse.h:7:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}}
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
                    "Some_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "Some_struct",
                  structFields = [],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "reparse.h:7:8",
                        declId = NamePair {
                          nameC = Name "some_struct",
                          nameHsIdent = HsIdentifier
                            "Some_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["reparse.h"],
                            headerInclude = "reparse.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Some_struct"),
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
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "some_struct",
                      commentLocation = Just
                        "reparse.h:7:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["reparse.h"],
                          headerInclude = "reparse.h"},
                      commentChildren = []}})
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
                    "Some_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "Some_struct",
                  structFields = [],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "reparse.h:7:8",
                        declId = NamePair {
                          nameC = Name "some_struct",
                          nameHsIdent = HsIdentifier
                            "Some_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["reparse.h"],
                            headerInclude = "reparse.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Some_struct"),
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
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "some_struct",
                      commentLocation = Just
                        "reparse.h:7:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["reparse.h"],
                          headerInclude = "reparse.h"},
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
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Some_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Some_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Some_union",
      newtypeConstr = HsName
        "@NsConstr"
        "Some_union",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Some_union",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:8:7",
          declId = NamePair {
            nameC = Name "some_union",
            nameHsIdent = HsIdentifier
              "Some_union"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Some_union",
              newtypeField = HsName
                "@NsVar"
                "un_Some_union"},
            unionSizeof = 0,
            unionAlignment = 1,
            unionFields = []},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "some_union",
          commentLocation = Just
            "reparse.h:8:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 0 1),
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Some_union",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Some_enum",
      newtypeConstr = HsName
        "@NsConstr"
        "Some_enum",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Some_enum",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:9:6",
          declId = NamePair {
            nameC = Name "some_enum",
            nameHsIdent = HsIdentifier
              "Some_enum"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Some_enum",
              newtypeField = HsName
                "@NsVar"
                "un_Some_enum"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc = "reparse.h:9:18",
                  fieldName = NamePair {
                    nameC = Name "ENUM_A",
                    nameHsIdent = HsIdentifier
                      "ENUM_A"},
                  fieldComment = Nothing},
                enumConstantValue = 0}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "some_enum",
          commentLocation = Just
            "reparse.h:9:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Some_enum",
          structConstr = HsName
            "@NsConstr"
            "Some_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Some_enum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
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
                    "Some_enum",
                  structConstr = HsName
                    "@NsConstr"
                    "Some_enum",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_Some_enum",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
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
                    "Some_enum",
                  structConstr = HsName
                    "@NsConstr"
                    "Some_enum",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_Some_enum",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
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
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Some_enum",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Some_enum",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Some_enum",
          structConstr = HsName
            "@NsConstr"
            "Some_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Some_enum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsPrimType HsPrimCUInt)
        (Map.fromList
          [
            _×_ 0 (NE.fromList ["ENUM_A"])])
        True,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceSequentialCEnum
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Some_enum",
          structConstr = HsName
            "@NsConstr"
            "Some_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Some_enum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "ENUM_A")
        (HsName "@NsConstr" "ENUM_A"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Some_enum",
          structConstr = HsName
            "@NsConstr"
            "Some_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Some_enum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumRead
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Some_enum",
          structConstr = HsName
            "@NsConstr"
            "Some_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Some_enum",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "ENUM_A",
      patSynType = HsName
        "@NsTypeConstr"
        "Some_enum",
      patSynConstr = HsName
        "@NsConstr"
        "Some_enum",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc = "reparse.h:9:18",
            fieldName = NamePair {
              nameC = Name "ENUM_A",
              nameHsIdent = HsIdentifier
                "ENUM_A"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ENUM_A",
          commentLocation = Just
            "reparse.h:9:18",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_char1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_10171eb304e5cc62",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_10171eb304e5cc62 (A arg1, char arg2) { args_char1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimChar
                  (PrimSignImplicit Nothing)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Function declarations"],
          commentOrigin = Just
            "args_char1",
          commentLocation = Just
            "reparse.h:17:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_016baba9b269ce3e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCChar)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_016baba9b269ce3e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_char1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_016baba9b269ce3e (void)) (A arg1, char arg2) { return &args_char1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Function declarations"],
          commentOrigin = Just
            "args_char1",
          commentLocation = Just
            "reparse.h:17:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_char2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCSChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_0a5b69870f97616a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_0a5b69870f97616a (A arg1, signed char arg2) { args_char2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimChar
                  (PrimSignExplicit Signed)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_char2",
          commentLocation = Just
            "reparse.h:18:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_deda633a655edee7",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCSChar)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_deda633a655edee7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_char2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_deda633a655edee7 (void)) (A arg1, signed char arg2) { return &args_char2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimChar
                (PrimSignExplicit Signed))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_char2",
          commentLocation = Just
            "reparse.h:18:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_char3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCUChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_2152f20110364cbb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_2152f20110364cbb (A arg1, unsigned char arg2) { args_char3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_char3",
          commentLocation = Just
            "reparse.h:19:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_929c40236948e098",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCUChar)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_929c40236948e098",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_char3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_929c40236948e098 (void)) (A arg1, unsigned char arg2) { return &args_char3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_char3",
          commentLocation = Just
            "reparse.h:19:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_short1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCShort,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_bad35762dd252147",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_bad35762dd252147 (A arg1, signed short arg2) { args_short1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimIntegral
                  PrimShort
                  Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_short1",
          commentLocation = Just
            "reparse.h:21:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_6765935911469fba",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCShort)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_6765935911469fba",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_short1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_6765935911469fba (void)) (A arg1, signed short arg2) { return &args_short1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimIntegral PrimShort Signed)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_short1",
          commentLocation = Just
            "reparse.h:21:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_short2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCShort,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_8705b73515e5d5e7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_8705b73515e5d5e7 (A arg1, signed short arg2) { args_short2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimIntegral
                  PrimShort
                  Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_short2",
          commentLocation = Just
            "reparse.h:22:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_945292112fa4a0ee",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCShort)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_945292112fa4a0ee",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_short2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_945292112fa4a0ee (void)) (A arg1, signed short arg2) { return &args_short2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimIntegral PrimShort Signed)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_short2",
          commentLocation = Just
            "reparse.h:22:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_short3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCUShort,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ae877590f2712156",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_ae877590f2712156 (A arg1, unsigned short arg2) { args_short3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimIntegral
                  PrimShort
                  Unsigned))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_short3",
          commentLocation = Just
            "reparse.h:23:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_0a3b985c5f6b183e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCUShort)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_0a3b985c5f6b183e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_short3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_0a3b985c5f6b183e (void)) (A arg1, unsigned short arg2) { return &args_short3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimIntegral
                PrimShort
                Unsigned)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_short3",
          commentLocation = Just
            "reparse.h:23:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_int1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_5bcc52df98b32e80",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_5bcc52df98b32e80 (A arg1, signed int arg2) { args_int1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_int1",
          commentLocation = Just
            "reparse.h:25:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_0b9730298592ed7b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_0b9730298592ed7b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_int1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_0b9730298592ed7b (void)) (A arg1, signed int arg2) { return &args_int1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimIntegral PrimInt Signed)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_int1",
          commentLocation = Just
            "reparse.h:25:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_int2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d5542d284cc6174c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_d5542d284cc6174c (A arg1, signed int arg2) { args_int2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_int2",
          commentLocation = Just
            "reparse.h:26:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_b35d311c59b97357",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_b35d311c59b97357",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_int2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_b35d311c59b97357 (void)) (A arg1, signed int arg2) { return &args_int2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimIntegral PrimInt Signed)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_int2",
          commentLocation = Just
            "reparse.h:26:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_int3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCUInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_940dd7ba49a6be4a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_940dd7ba49a6be4a (A arg1, unsigned int arg2) { args_int3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Unsigned))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_int3",
          commentLocation = Just
            "reparse.h:27:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_deb09915f72c9f94",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCUInt)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_deb09915f72c9f94",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_int3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_deb09915f72c9f94 (void)) (A arg1, unsigned int arg2) { return &args_int3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimIntegral PrimInt Unsigned)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_int3",
          commentLocation = Just
            "reparse.h:27:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_long1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_79b01356e07fd6c5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_79b01356e07fd6c5 (A arg1, signed long arg2) { args_long1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimIntegral
                  PrimLong
                  Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_long1",
          commentLocation = Just
            "reparse.h:29:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_da0f1cd5ae89314a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCLong)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_da0f1cd5ae89314a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_long1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_da0f1cd5ae89314a (void)) (A arg1, signed long arg2) { return &args_long1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimIntegral PrimLong Signed)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_long1",
          commentLocation = Just
            "reparse.h:29:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_long2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCLong,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d5bdd91db9738145",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_d5bdd91db9738145 (A arg1, signed long arg2) { args_long2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimIntegral
                  PrimLong
                  Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_long2",
          commentLocation = Just
            "reparse.h:30:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_758d9779edb07735",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCLong)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_758d9779edb07735",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_long2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_758d9779edb07735 (void)) (A arg1, signed long arg2) { return &args_long2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimIntegral PrimLong Signed)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_long2",
          commentLocation = Just
            "reparse.h:30:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_long3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCULong,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9302b433a1667eb4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_9302b433a1667eb4 (A arg1, unsigned long arg2) { args_long3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimIntegral
                  PrimLong
                  Unsigned))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_long3",
          commentLocation = Just
            "reparse.h:31:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_db9704dc54e0b830",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCULong)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_db9704dc54e0b830",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_long3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_db9704dc54e0b830 (void)) (A arg1, unsigned long arg2) { return &args_long3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimIntegral
                PrimLong
                Unsigned)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_long3",
          commentLocation = Just
            "reparse.h:31:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_float",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_316fd20ab67d2e28",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_316fd20ab67d2e28 (A arg1, float arg2) { args_float(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimFloating PrimFloat))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_float",
          commentLocation = Just
            "reparse.h:33:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_7bd676a3aba9914d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCFloat)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_7bd676a3aba9914d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_float_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_7bd676a3aba9914d (void)) (A arg1, float arg2) { return &args_float; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimFloat)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_float",
          commentLocation = Just
            "reparse.h:33:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_double",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_82242119ea26cfe9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_82242119ea26cfe9 (A arg1, double arg2) { args_double(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_double",
          commentLocation = Just
            "reparse.h:34:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_bc71e4aa8bc9e516",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_bc71e4aa8bc9e516",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_double_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_bc71e4aa8bc9e516 (void)) (A arg1, double arg2) { return &args_double; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_double",
          commentLocation = Just
            "reparse.h:34:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_bool1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCBool,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_8ac94b2768601ba9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_8ac94b2768601ba9 (A arg1, _Bool arg2) { args_bool1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim PrimBool)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_bool1",
          commentLocation = Just
            "reparse.h:35:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_e8148b6b7b0284e3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCBool)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e8148b6b7b0284e3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_bool1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e8148b6b7b0284e3 (void)) (A arg1, _Bool arg2) { return &args_bool1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim PrimBool]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_bool1",
          commentLocation = Just
            "reparse.h:35:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_struct_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Some_struct")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_fc804a81327ef4f8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_fc804a81327ef4f8 (A arg1, struct some_struct *arg2) { args_struct(arg1, *arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeStruct
                NamePair {
                  nameC = Name "some_struct",
                  nameHsIdent = HsIdentifier
                    "Some_struct"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_struct",
          commentLocation = Just
            "reparse.h:37:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_b679dc2bfc141ddd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_struct"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_b679dc2bfc141ddd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_struct_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_b679dc2bfc141ddd (void)) (A arg1, struct some_struct arg2) { return &args_struct; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeStruct
              NamePair {
                nameC = Name "some_struct",
                nameHsIdent = HsIdentifier
                  "Some_struct"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_struct",
          commentLocation = Just
            "reparse.h:37:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_union_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Some_union")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_fd5beb002748e594",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_fd5beb002748e594 (A arg1, union some_union *arg2) { args_union(arg1, *arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeUnion
                NamePair {
                  nameC = Name "some_union",
                  nameHsIdent = HsIdentifier
                    "Some_union"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_union",
          commentLocation = Just
            "reparse.h:38:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_9fc41568c0ef334a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_union"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9fc41568c0ef334a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_union_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_9fc41568c0ef334a (void)) (A arg1, union some_union arg2) { return &args_union; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeUnion
              NamePair {
                nameC = Name "some_union",
                nameHsIdent = HsIdentifier
                  "Some_union"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_union",
          commentLocation = Just
            "reparse.h:38:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_enum",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Some_enum"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_52c129b6f3ed3c8f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_52c129b6f3ed3c8f (A arg1, enum some_enum arg2) { args_enum(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeEnum
                NamePair {
                  nameC = Name "some_enum",
                  nameHsIdent = HsIdentifier
                    "Some_enum"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_enum",
          commentLocation = Just
            "reparse.h:39:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_a38701ffd6bcd8be",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_enum"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_a38701ffd6bcd8be",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_enum_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a38701ffd6bcd8be (void)) (A arg1, enum some_enum arg2) { return &args_enum; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeEnum
              NamePair {
                nameC = Name "some_enum",
                nameHsIdent = HsIdentifier
                  "Some_enum"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_enum",
          commentLocation = Just
            "reparse.h:39:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_pointer1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9a2762d56bf1de6c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_9a2762d56bf1de6c (A arg1, signed int *arg2) { args_pointer1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePointer
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_pointer1",
          commentLocation = Just
            "reparse.h:41:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_a64bc25bdd7e894f",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPtr (HsPrimType HsPrimCInt))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_a64bc25bdd7e894f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_pointer1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a64bc25bdd7e894f (void)) (A arg1, signed int *arg2) { return &args_pointer1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer
              (TypePrim
                (PrimIntegral PrimInt Signed))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_pointer1",
          commentLocation = Just
            "reparse.h:41:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_pointer2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsPtr (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ec864c09e80e67e1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_ec864c09e80e67e1 (A arg1, signed int **arg2) { args_pointer2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePointer
                (TypePointer
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_pointer2",
          commentLocation = Just
            "reparse.h:42:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_9edb8a2162d37629",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPtr
                  (HsPtr (HsPrimType HsPrimCInt)))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9edb8a2162d37629",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_pointer2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_9edb8a2162d37629 (void)) (A arg1, signed int **arg2) { return &args_pointer2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer
              (TypePointer
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_pointer2",
          commentLocation = Just
            "reparse.h:42:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_pointer3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg3"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg3",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_09151e71162f3239",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_09151e71162f3239 (A arg1, void *arg2) { args_pointer3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg3",
                  nameHsIdent = HsIdentifier
                    "arg3"})
              (TypePointer TypeVoid)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_pointer3",
          commentLocation = Just
            "reparse.h:43:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_8a930c71a84a9846",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPtr (HsPrimType HsPrimVoid))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_8a930c71a84a9846",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_pointer3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_8a930c71a84a9846 (void)) (A arg1, void *arg2) { return &args_pointer3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer TypeVoid]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_pointer3",
          commentLocation = Just
            "reparse.h:43:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_A",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName "@NsTypeConstr" "A"))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_721fac62084456f3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "A hs_bindgen_test_reparse_721fac62084456f3 (void) { return ret_A(); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeMacroTypedef
            NamePair {
              nameC = Name "A",
              nameHsIdent = HsIdentifier "A"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ret_A",
          commentLocation = Just
            "reparse.h:47:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_877842edc48bfc6f",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "A"))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_877842edc48bfc6f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_A_ptr */ __attribute__ ((const)) A (*hs_bindgen_test_reparse_877842edc48bfc6f (void)) (void) { return &ret_A; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypeMacroTypedef
            NamePair {
              nameC = Name "A",
              nameHsIdent = HsIdentifier "A"}
            NameOriginInSource)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ret_A",
          commentLocation = Just
            "reparse.h:47:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_char1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCChar)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_82bdf9fd8ea5cec3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char hs_bindgen_test_reparse_82bdf9fd8ea5cec3 (A arg1) { return ret_char1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit Nothing))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_char1",
          commentLocation = Just
            "reparse.h:49:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_adbd59964b60242f",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCChar))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_adbd59964b60242f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_char1_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_reparse_adbd59964b60242f (void)) (A arg1) { return &ret_char1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimChar
              (PrimSignImplicit Nothing)))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_char1",
          commentLocation = Just
            "reparse.h:49:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_char2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCSChar)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_cc0da46ef0c52b0d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed char hs_bindgen_test_reparse_cc0da46ef0c52b0d (A arg1) { return ret_char2(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignExplicit Signed))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_char2",
          commentLocation = Just
            "reparse.h:50:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_59bbea2dfed561af",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCSChar))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_59bbea2dfed561af",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_char2_ptr */ __attribute__ ((const)) signed char (*hs_bindgen_test_reparse_59bbea2dfed561af (void)) (A arg1) { return &ret_char2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimChar
              (PrimSignExplicit Signed)))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_char2",
          commentLocation = Just
            "reparse.h:50:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_char3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCUChar)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_225667bb52779eb7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "unsigned char hs_bindgen_test_reparse_225667bb52779eb7 (A arg1) { return ret_char3(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignExplicit Unsigned))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_char3",
          commentLocation = Just
            "reparse.h:51:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_0801d372d3906773",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCUChar))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_0801d372d3906773",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_char3_ptr */ __attribute__ ((const)) unsigned char (*hs_bindgen_test_reparse_0801d372d3906773 (void)) (A arg1) { return &ret_char3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimChar
              (PrimSignExplicit Unsigned)))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_char3",
          commentLocation = Just
            "reparse.h:51:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_short1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCShort)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9322ef5e958be938",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed short hs_bindgen_test_reparse_9322ef5e958be938 (A arg1) { return ret_short1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral
              PrimShort
              Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_short1",
          commentLocation = Just
            "reparse.h:53:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_ccfcc25e4950bc67",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCShort))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ccfcc25e4950bc67",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_short1_ptr */ __attribute__ ((const)) signed short (*hs_bindgen_test_reparse_ccfcc25e4950bc67 (void)) (A arg1) { return &ret_short1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral
              PrimShort
              Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_short1",
          commentLocation = Just
            "reparse.h:53:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_short2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCShort)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ef12449372a2af8b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed short hs_bindgen_test_reparse_ef12449372a2af8b (A arg1) { return ret_short2(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral
              PrimShort
              Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_short2",
          commentLocation = Just
            "reparse.h:54:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_4bf91229e062d6be",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCShort))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_4bf91229e062d6be",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_short2_ptr */ __attribute__ ((const)) signed short (*hs_bindgen_test_reparse_4bf91229e062d6be (void)) (A arg1) { return &ret_short2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral
              PrimShort
              Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_short2",
          commentLocation = Just
            "reparse.h:54:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_short3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCUShort)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_a7f196b1d51beccf",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "unsigned short hs_bindgen_test_reparse_a7f196b1d51beccf (A arg1) { return ret_short3(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral
              PrimShort
              Unsigned)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_short3",
          commentLocation = Just
            "reparse.h:55:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_d7104088c29df31d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCUShort))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d7104088c29df31d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_short3_ptr */ __attribute__ ((const)) unsigned short (*hs_bindgen_test_reparse_d7104088c29df31d (void)) (A arg1) { return &ret_short3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral
              PrimShort
              Unsigned))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_short3",
          commentLocation = Just
            "reparse.h:55:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_int1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9e2b08760143bbc2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_reparse_9e2b08760143bbc2 (A arg1) { return ret_int1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ret_int1",
          commentLocation = Just
            "reparse.h:57:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_5c37e045e5cc329f",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_5c37e045e5cc329f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_int1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_reparse_5c37e045e5cc329f (void)) (A arg1) { return &ret_int1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ret_int1",
          commentLocation = Just
            "reparse.h:57:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_int2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d7ec0cdbec9f2174",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_reparse_d7ec0cdbec9f2174 (A arg1) { return ret_int2(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ret_int2",
          commentLocation = Just
            "reparse.h:58:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_9e2af05c9cea58ba",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9e2af05c9cea58ba",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_int2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_reparse_9e2af05c9cea58ba (void)) (A arg1) { return &ret_int2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ret_int2",
          commentLocation = Just
            "reparse.h:58:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_int3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCUInt)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_b4415511dbc7b6ab",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "unsigned int hs_bindgen_test_reparse_b4415511dbc7b6ab (A arg1) { return ret_int3(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral
              PrimInt
              Unsigned)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ret_int3",
          commentLocation = Just
            "reparse.h:59:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_b04741094c75560d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCUInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_b04741094c75560d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_int3_ptr */ __attribute__ ((const)) unsigned int (*hs_bindgen_test_reparse_b04741094c75560d (void)) (A arg1) { return &ret_int3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral
              PrimInt
              Unsigned))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ret_int3",
          commentLocation = Just
            "reparse.h:59:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_long1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCLong)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_62ed9e11769772fe",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed long hs_bindgen_test_reparse_62ed9e11769772fe (A arg1) { return ret_long1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimLong Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_long1",
          commentLocation = Just
            "reparse.h:61:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_8f79f8c1226c822b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCLong))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_8f79f8c1226c822b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_long1_ptr */ __attribute__ ((const)) signed long (*hs_bindgen_test_reparse_8f79f8c1226c822b (void)) (A arg1) { return &ret_long1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral
              PrimLong
              Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_long1",
          commentLocation = Just
            "reparse.h:61:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_long2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCLong)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_16ef6708c7b0d6a3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed long hs_bindgen_test_reparse_16ef6708c7b0d6a3 (A arg1) { return ret_long2(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimLong Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_long2",
          commentLocation = Just
            "reparse.h:62:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_2ba044632bcfc084",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCLong))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_2ba044632bcfc084",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_long2_ptr */ __attribute__ ((const)) signed long (*hs_bindgen_test_reparse_2ba044632bcfc084 (void)) (A arg1) { return &ret_long2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral
              PrimLong
              Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_long2",
          commentLocation = Just
            "reparse.h:62:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_long3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCULong)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_097510d70490cc54",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "unsigned long hs_bindgen_test_reparse_097510d70490cc54 (A arg1) { return ret_long3(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral
              PrimLong
              Unsigned)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_long3",
          commentLocation = Just
            "reparse.h:63:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_1129f4b482d7d20c",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCULong))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_1129f4b482d7d20c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_long3_ptr */ __attribute__ ((const)) unsigned long (*hs_bindgen_test_reparse_1129f4b482d7d20c (void)) (A arg1) { return &ret_long3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral
              PrimLong
              Unsigned))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_long3",
          commentLocation = Just
            "reparse.h:63:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_float",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCFloat)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_5777c7f34d2aabaf",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "float hs_bindgen_test_reparse_5777c7f34d2aabaf (A arg1) { return ret_float(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimFloating PrimFloat)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_float",
          commentLocation = Just
            "reparse.h:65:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_eed7871d97729b4d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCFloat))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_eed7871d97729b4d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_float_ptr */ __attribute__ ((const)) float (*hs_bindgen_test_reparse_eed7871d97729b4d (void)) (A arg1) { return &ret_float; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimFloating PrimFloat))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_float",
          commentLocation = Just
            "reparse.h:65:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_double",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCDouble)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_61e16d3cc5a31728",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "double hs_bindgen_test_reparse_61e16d3cc5a31728 (A arg1) { return ret_double(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimFloating PrimDouble)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_double",
          commentLocation = Just
            "reparse.h:66:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_c29a0524e261854e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCDouble))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c29a0524e261854e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_double_ptr */ __attribute__ ((const)) double (*hs_bindgen_test_reparse_c29a0524e261854e (void)) (A arg1) { return &ret_double; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimFloating PrimDouble))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_double",
          commentLocation = Just
            "reparse.h:66:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_bool1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_da83bbf3ce08a187",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "_Bool hs_bindgen_test_reparse_da83bbf3ce08a187 (A arg1) { return ret_bool1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            PrimBool},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_bool1",
          commentLocation = Just
            "reparse.h:67:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_4c0272348355fd7b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCBool))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_4c0272348355fd7b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_bool1_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_reparse_4c0272348355fd7b (void)) (A arg1) { return &ret_bool1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim PrimBool)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_bool1",
          commentLocation = Just
            "reparse.h:67:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_struct_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Some_struct"))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_98263067da352564",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_98263067da352564 (A arg1, struct some_struct *arg2) { *arg2 = ret_struct(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "some_struct",
              nameHsIdent = HsIdentifier
                "Some_struct"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_struct",
          commentLocation = Just
            "reparse.h:69:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_aed8a54ad301c1ed",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_struct")))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_aed8a54ad301c1ed",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_struct_ptr */ __attribute__ ((const)) struct some_struct (*hs_bindgen_test_reparse_aed8a54ad301c1ed (void)) (A arg1) { return &ret_struct; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypeStruct
            NamePair {
              nameC = Name "some_struct",
              nameHsIdent = HsIdentifier
                "Some_struct"}
            NameOriginInSource)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_struct",
          commentLocation = Just
            "reparse.h:69:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_union_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Some_union"))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_3daeab025170869e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_3daeab025170869e (A arg1, union some_union *arg2) { *arg2 = ret_union(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeUnion
            NamePair {
              nameC = Name "some_union",
              nameHsIdent = HsIdentifier
                "Some_union"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_union",
          commentLocation = Just
            "reparse.h:70:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_682e09e159d012ae",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_union")))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_682e09e159d012ae",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_union_ptr */ __attribute__ ((const)) union some_union (*hs_bindgen_test_reparse_682e09e159d012ae (void)) (A arg1) { return &ret_union; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypeUnion
            NamePair {
              nameC = Name "some_union",
              nameHsIdent = HsIdentifier
                "Some_union"}
            NameOriginInSource)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_union",
          commentLocation = Just
            "reparse.h:70:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_enum",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Some_enum"))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_1afc70c95eeaaf09",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "enum some_enum hs_bindgen_test_reparse_1afc70c95eeaaf09 (A arg1) { return ret_enum(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeEnum
            NamePair {
              nameC = Name "some_enum",
              nameHsIdent = HsIdentifier
                "Some_enum"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ret_enum",
          commentLocation = Just
            "reparse.h:71:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_1eb9e1744c110315",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_enum")))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_1eb9e1744c110315",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_enum_ptr */ __attribute__ ((const)) enum some_enum (*hs_bindgen_test_reparse_1eb9e1744c110315 (void)) (A arg1) { return &ret_enum; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypeEnum
            NamePair {
              nameC = Name "some_enum",
              nameHsIdent = HsIdentifier
                "Some_enum"}
            NameOriginInSource)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "ret_enum",
          commentLocation = Just
            "reparse.h:71:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_pointer1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_031974ad82756999",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int *hs_bindgen_test_reparse_031974ad82756999 (A arg1) { return ret_pointer1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypePrim
              (PrimIntegral PrimInt Signed))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_pointer1",
          commentLocation = Just
            "reparse.h:73:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_e0b897b0d664dd33",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e0b897b0d664dd33",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_pointer1_ptr */ __attribute__ ((const)) signed int *(*hs_bindgen_test_reparse_e0b897b0d664dd33 (void)) (A arg1) { return &ret_pointer1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_pointer1",
          commentLocation = Just
            "reparse.h:73:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_pointer2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_3a4b95d14fe97f80",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int **hs_bindgen_test_reparse_3a4b95d14fe97f80 (A arg1) { return ret_pointer2(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypePointer
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_pointer2",
          commentLocation = Just
            "reparse.h:74:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_82fad80fb47b7236",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPtr
                  (HsPtr
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_82fad80fb47b7236",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_pointer2_ptr */ __attribute__ ((const)) signed int **(*hs_bindgen_test_reparse_82fad80fb47b7236 (void)) (A arg1) { return &ret_pointer2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypePointer
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_pointer2",
          commentLocation = Just
            "reparse.h:74:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_pointer3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_96fb446aa96eafd2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_reparse_96fb446aa96eafd2 (A arg1) { return ret_pointer3(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_pointer3",
          commentLocation = Just
            "reparse.h:75:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_efd348436d86573f",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimVoid)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_efd348436d86573f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_pointer3_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_reparse_efd348436d86573f (void)) (A arg1) { return &ret_pointer3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer TypeVoid)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_pointer3",
          commentLocation = Just
            "reparse.h:75:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "body1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_49dcc244bd02f90d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_reparse_49dcc244bd02f90d (A arg1) { return body1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "body1",
          commentLocation = Just
            "reparse.h:79:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_997e04203154d0e9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_997e04203154d0e9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_body1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_reparse_997e04203154d0e9 (void)) (A arg1) { return &body1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "body1",
          commentLocation = Just
            "reparse.h:79:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "body2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName "@NsTypeConstr" "A"))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_4712ad809c27d477",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "A hs_bindgen_test_reparse_4712ad809c27d477 (void) { return body2(); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeMacroTypedef
            NamePair {
              nameC = Name "A",
              nameHsIdent = HsIdentifier "A"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "body2",
          commentLocation = Just
            "reparse.h:80:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_37bd37a0a14228bd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "A"))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_37bd37a0a14228bd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_body2_ptr */ __attribute__ ((const)) A (*hs_bindgen_test_reparse_37bd37a0a14228bd (void)) (void) { return &body2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypeMacroTypedef
            NamePair {
              nameC = Name "A",
              nameHsIdent = HsIdentifier "A"}
            NameOriginInSource)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "body2",
          commentLocation = Just
            "reparse.h:80:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_complex_float_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsComplexType HsPrimCFloat),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e390229cde8bd386",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_e390229cde8bd386 (A arg1, float _Complex *arg2) { args_complex_float(arg1, *arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeComplex
                (PrimFloating PrimFloat))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_complex_float",
          commentLocation = Just
            "reparse.h:84:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_bbd43e0b7f262423",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsComplexType HsPrimCFloat)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_bbd43e0b7f262423",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_complex_float_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_bbd43e0b7f262423 (void)) (A arg1, float _Complex arg2) { return &args_complex_float; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeComplex
              (PrimFloating PrimFloat)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_complex_float",
          commentLocation = Just
            "reparse.h:84:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "args_complex_double_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsComplexType HsPrimCDouble),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_230de832b9c63bed",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_230de832b9c63bed (A arg1, double _Complex *arg2) { args_complex_double(arg1, *arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeComplex
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_complex_double",
          commentLocation = Just
            "reparse.h:85:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_d654095e32e85ea0",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsComplexType HsPrimCDouble)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d654095e32e85ea0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_args_complex_double_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_d654095e32e85ea0 (void)) (A arg1, double _Complex arg2) { return &args_complex_double; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeComplex
              (PrimFloating PrimDouble)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "args_complex_double",
          commentLocation = Just
            "reparse.h:85:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_complex_float_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsComplexType HsPrimCFloat)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_79af5d3f50e20e35",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_79af5d3f50e20e35 (A arg1, float _Complex *arg2) { *arg2 = ret_complex_float(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeComplex
            (PrimFloating PrimFloat)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_complex_float",
          commentLocation = Just
            "reparse.h:86:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_c2911fd8888e3b7d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsComplexType
                  HsPrimCFloat))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c2911fd8888e3b7d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_complex_float_ptr */ __attribute__ ((const)) float _Complex (*hs_bindgen_test_reparse_c2911fd8888e3b7d (void)) (A arg1) { return &ret_complex_float; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypeComplex
            (PrimFloating PrimFloat))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_complex_float",
          commentLocation = Just
            "reparse.h:86:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "ret_complex_double_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsComplexType HsPrimCDouble)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ed71779b9e9b1c7a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_ed71779b9e9b1c7a (A arg1, double _Complex *arg2) { *arg2 = ret_complex_double(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeComplex
            (PrimFloating PrimDouble)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_complex_double",
          commentLocation = Just
            "reparse.h:87:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_368c2a0e22b110dc",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsComplexType
                  HsPrimCDouble))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_368c2a0e22b110dc",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_ret_complex_double_ptr */ __attribute__ ((const)) double _Complex (*hs_bindgen_test_reparse_368c2a0e22b110dc (void)) (A arg1) { return &ret_complex_double; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypeComplex
            (PrimFloating PrimDouble))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "ret_complex_double",
          commentLocation = Just
            "reparse.h:87:17",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bespoke_args1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCBool,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_6118e8e73dd68f55",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_6118e8e73dd68f55 (A arg1, _Bool arg2) { bespoke_args1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim PrimBool)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "bespoke_args1",
          commentLocation = Just
            "reparse.h:94:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_93a5557905f490f6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCBool)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_93a5557905f490f6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_bespoke_args1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_93a5557905f490f6 (void)) (A arg1, _Bool arg2) { return &bespoke_args1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim PrimBool]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "bespoke_args1",
          commentLocation = Just
            "reparse.h:94:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bespoke_args2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCSize,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_0968e3a6efd957cb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_0968e3a6efd957cb (A arg1, size_t arg2) { bespoke_args2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePrim PrimSize)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "bespoke_args2",
          commentLocation = Just
            "reparse.h:95:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_daf6570af7b7ecc5",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCSize)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_daf6570af7b7ecc5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_bespoke_args2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_daf6570af7b7ecc5 (void)) (A arg1, size_t arg2) { return &bespoke_args2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePrim PrimSize]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "bespoke_args2",
          commentLocation = Just
            "reparse.h:95:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bespoke_ret1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c6e601a29d4614cd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "_Bool hs_bindgen_test_reparse_c6e601a29d4614cd (A arg1) { return bespoke_ret1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            PrimBool},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "bespoke_ret1",
          commentLocation = Just
            "reparse.h:97:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_146864821fa56426",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCBool))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_146864821fa56426",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_bespoke_ret1_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_reparse_146864821fa56426 (void)) (A arg1) { return &bespoke_ret1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim PrimBool)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "bespoke_ret1",
          commentLocation = Just
            "reparse.h:97:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bespoke_ret2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCSize)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_4dd147df19305554",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "size_t hs_bindgen_test_reparse_4dd147df19305554 (A arg1) { return bespoke_ret2(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            PrimSize},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "bespoke_ret2",
          commentLocation = Just
            "reparse.h:98:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_9d181c086663dcbe",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimCSize))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9d181c086663dcbe",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_bespoke_ret2_ptr */ __attribute__ ((const)) size_t (*hs_bindgen_test_reparse_9d181c086663dcbe (void)) (A arg1) { return &bespoke_ret2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePrim PrimSize)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "bespoke_ret2",
          commentLocation = Just
            "reparse.h:98:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_args1_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9b04f6f1111852bd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_9b04f6f1111852bd (A *arg1) { arr_args1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeIncompleteArray
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [TextContent "Arrays"],
          commentOrigin = Just
            "arr_args1",
          commentLocation = Just
            "reparse.h:104:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_8a6079727ebf0474",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsIncompleteArray
                (HsTypRef
                  (HsName "@NsTypeConstr" "A")))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_8a6079727ebf0474",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_args1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_8a6079727ebf0474 (void)) (A arg1[]) { return &arr_args1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeIncompleteArray
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [TextContent "Arrays"],
          commentOrigin = Just
            "arr_args1",
          commentLocation = Just
            "reparse.h:104:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_args2_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsPtr
            (HsPtr
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_0434a82b86093487",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_0434a82b86093487 (A **arg1) { arr_args2(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeIncompleteArray
                (TypePointer
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "arr_args2",
          commentLocation = Just
            "reparse.h:105:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_babe3a26240cd0c7",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsIncompleteArray
                (HsPtr
                  (HsTypRef
                    (HsName "@NsTypeConstr" "A"))))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_babe3a26240cd0c7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_args2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_babe3a26240cd0c7 (void)) (A *arg1[]) { return &arr_args2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeIncompleteArray
              (TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "arr_args2",
          commentLocation = Just
            "reparse.h:105:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_args3_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_91ebb7f6b5169d37",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_91ebb7f6b5169d37 (A *arg1) { arr_args3(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeConstArray
                5
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "arr_args3",
          commentLocation = Just
            "reparse.h:106:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_9dd1b6a92f93c7dd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsConstArray
                5
                (HsTypRef
                  (HsName "@NsTypeConstr" "A")))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9dd1b6a92f93c7dd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_args3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_9dd1b6a92f93c7dd (void)) (A arg1[5]) { return &arr_args3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeConstArray
              5
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "arr_args3",
          commentLocation = Just
            "reparse.h:106:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "arr_args4_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsPtr
            (HsPtr
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_6707badb9b46b3ba",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_6707badb9b46b3ba (A **arg1) { arr_args4(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeConstArray
                5
                (TypePointer
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "arr_args4",
          commentLocation = Just
            "reparse.h:107:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_e1f7f09307f08420",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsConstArray
                5
                (HsPtr
                  (HsTypRef
                    (HsName "@NsTypeConstr" "A"))))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e1f7f09307f08420",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_arr_args4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e1f7f09307f08420 (void)) (A *arg1[5]) { return &arr_args4; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeConstArray
              5
              (TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "arr_args4",
          commentLocation = Just
            "reparse.h:107:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr_typedef1",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr_typedef1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr_typedef1",
        fieldType = HsIncompleteArray
          (HsTypRef
            (HsName "@NsTypeConstr" "A")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:109:13",
          declId = NamePair {
            nameC = Name "arr_typedef1",
            nameHsIdent = HsIdentifier
              "Arr_typedef1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr_typedef1",
              newtypeField = HsName
                "@NsVar"
                "un_Arr_typedef1"},
            typedefType =
            TypeIncompleteArray
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "arr_typedef1",
          commentLocation = Just
            "reparse.h:109:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr_typedef2",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr_typedef2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr_typedef2",
        fieldType = HsIncompleteArray
          (HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A"))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:110:13",
          declId = NamePair {
            nameC = Name "arr_typedef2",
            nameHsIdent = HsIdentifier
              "Arr_typedef2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr_typedef2",
              newtypeField = HsName
                "@NsVar"
                "un_Arr_typedef2"},
            typedefType =
            TypeIncompleteArray
              (TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "arr_typedef2",
          commentLocation = Just
            "reparse.h:110:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr_typedef3",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr_typedef3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr_typedef3",
        fieldType = HsConstArray
          5
          (HsTypRef
            (HsName "@NsTypeConstr" "A")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:111:13",
          declId = NamePair {
            nameC = Name "arr_typedef3",
            nameHsIdent = HsIdentifier
              "Arr_typedef3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr_typedef3",
              newtypeField = HsName
                "@NsVar"
                "un_Arr_typedef3"},
            typedefType = TypeConstArray
              5
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)},
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
            "arr_typedef3",
          commentLocation = Just
            "reparse.h:111:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr_typedef4",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr_typedef4",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr_typedef4",
        fieldType = HsConstArray
          5
          (HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A"))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:112:13",
          declId = NamePair {
            nameC = Name "arr_typedef4",
            nameHsIdent = HsIdentifier
              "Arr_typedef4"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Arr_typedef4",
              newtypeField = HsName
                "@NsVar"
                "un_Arr_typedef4"},
            typedefType = TypeConstArray
              5
              (TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))},
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
            "arr_typedef4",
          commentLocation = Just
            "reparse.h:112:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Arr_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Typedef1",
      newtypeConstr = HsName
        "@NsConstr"
        "Typedef1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Typedef1",
        fieldType = HsTypRef
          (HsName "@NsTypeConstr" "A"),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:118:14",
          declId = NamePair {
            nameC = Name "typedef1",
            nameHsIdent = HsIdentifier
              "Typedef1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Just
            (Comment
              [
                Paragraph
                  [TextContent "Typedefs"]])},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Typedef1",
              newtypeField = HsName
                "@NsVar"
                "un_Typedef1"},
            typedefType = TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource},
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
          commentTitle = Just
            [TextContent "Typedefs"],
          commentOrigin = Just "typedef1",
          commentLocation = Just
            "reparse.h:118:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
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
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
      deriveInstanceComment =
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
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
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
        "Typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Typedef2",
      newtypeConstr = HsName
        "@NsConstr"
        "Typedef2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Typedef2",
        fieldType = HsPtr
          (HsTypRef
            (HsName "@NsTypeConstr" "A")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:119:14",
          declId = NamePair {
            nameC = Name "typedef2",
            nameHsIdent = HsIdentifier
              "Typedef2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Typedef2",
              newtypeField = HsName
                "@NsVar"
                "un_Typedef2"},
            typedefType = TypePointer
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "typedef2",
          commentLocation = Just
            "reparse.h:119:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
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
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef2",
      deriveInstanceComment =
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
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Typedef3",
      newtypeConstr = HsName
        "@NsConstr"
        "Typedef3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Typedef3",
        fieldType = HsPtr
          (HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A"))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:120:14",
          declId = NamePair {
            nameC = Name "typedef3",
            nameHsIdent = HsIdentifier
              "Typedef3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Typedef3",
              newtypeField = HsName
                "@NsVar"
                "un_Typedef3"},
            typedefType = TypePointer
              (TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "typedef3",
          commentLocation = Just
            "reparse.h:120:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
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
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Typedef3",
      deriveInstanceComment =
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
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_args1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsFunPtr
            (HsIO (HsPrimType HsPrimUnit)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c31bf8a78aa68a4d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_c31bf8a78aa68a4d (A arg1, void (*arg2) (void)) { funptr_args1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePointer
                (TypeFun [] TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Function pointers"],
          commentOrigin = Just
            "funptr_args1",
          commentLocation = Just
            "reparse.h:126:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_1c0e4678c987c05d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsFunPtr
                  (HsIO (HsPrimType HsPrimUnit)))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_1c0e4678c987c05d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_args1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_1c0e4678c987c05d (void)) (A arg1, void (*arg2) (void)) { return &funptr_args1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer
              (TypeFun [] TypeVoid)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Function pointers"],
          commentOrigin = Just
            "funptr_args1",
          commentLocation = Just
            "reparse.h:126:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_args2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsFunPtr
            (HsIO (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_98fbd01a7d3daae5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_98fbd01a7d3daae5 (A arg1, signed int (*arg2) (void)) { funptr_args2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePointer
                (TypeFun
                  []
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_args2",
          commentLocation = Just
            "reparse.h:127:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_7b060727d5fd9def",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsFunPtr
                  (HsIO (HsPrimType HsPrimCInt)))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_7b060727d5fd9def",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_args2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_7b060727d5fd9def (void)) (A arg1, signed int (*arg2) (void)) { return &funptr_args2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer
              (TypeFun
                []
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_args2",
          commentLocation = Just
            "reparse.h:127:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_args3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimUnit))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_1c1752238030d660",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_1c1752238030d660 (A arg1, void (*arg2) (signed int arg1)) { funptr_args3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  TypeVoid))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_args3",
          commentLocation = Just
            "reparse.h:128:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_daeb6d5a2f5d54e9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO (HsPrimType HsPrimUnit))))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_daeb6d5a2f5d54e9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_args3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_daeb6d5a2f5d54e9 (void)) (A arg1, void (*arg2) (signed int arg1)) { return &funptr_args3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                TypeVoid)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_args3",
          commentLocation = Just
            "reparse.h:128:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_args4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPrimType HsPrimCChar)))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_8c6106c42b1ae638",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_8c6106c42b1ae638 (A arg1, char (*arg2) (signed int arg1, double arg2)) { funptr_args4(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed),
                    TypePrim
                      (PrimFloating PrimDouble)]
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit Nothing)))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_args4",
          commentLocation = Just
            "reparse.h:129:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_a43eec054dfe7fd4",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsFun
                      (HsPrimType HsPrimCDouble)
                      (HsIO
                        (HsPrimType HsPrimCChar)))))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_a43eec054dfe7fd4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_args4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a43eec054dfe7fd4 (void)) (A arg1, char (*arg2) (signed int arg1, double arg2)) { return &funptr_args4; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit Nothing))))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_args4",
          commentLocation = Just
            "reparse.h:129:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_args5",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCInt))))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_367f3da78499ae2e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_367f3da78499ae2e (A arg1, signed int *(*arg2) (signed int arg1, double arg2)) { funptr_args5(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePointer
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed),
                    TypePrim
                      (PrimFloating PrimDouble)]
                  (TypePointer
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_args5",
          commentLocation = Just
            "reparse.h:130:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_464f5f738234f93e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsFun
                      (HsPrimType HsPrimCDouble)
                      (HsIO
                        (HsPtr
                          (HsPrimType HsPrimCInt))))))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_464f5f738234f93e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_args5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_464f5f738234f93e (void)) (A arg1, signed int *(*arg2) (signed int arg1, double arg2)) { return &funptr_args5; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypePointer
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_args5",
          commentLocation = Just
            "reparse.h:130:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Funptr_typedef1",
      newtypeConstr = HsName
        "@NsConstr"
        "Funptr_typedef1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Funptr_typedef1",
        fieldType = HsFunPtr
          (HsIO
            (HsTypRef
              (HsName "@NsTypeConstr" "A"))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:132:16",
          declId = NamePair {
            nameC = Name "funptr_typedef1",
            nameHsIdent = HsIdentifier
              "Funptr_typedef1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Funptr_typedef1",
              newtypeField = HsName
                "@NsVar"
                "un_Funptr_typedef1"},
            typedefType = TypePointer
              (TypeFun
                []
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_typedef1",
          commentLocation = Just
            "reparse.h:132:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Funptr_typedef2",
      newtypeConstr = HsName
        "@NsConstr"
        "Funptr_typedef2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Funptr_typedef2",
        fieldType = HsFunPtr
          (HsIO
            (HsPtr
              (HsTypRef
                (HsName "@NsTypeConstr" "A")))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:133:16",
          declId = NamePair {
            nameC = Name "funptr_typedef2",
            nameHsIdent = HsIdentifier
              "Funptr_typedef2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Funptr_typedef2",
              newtypeField = HsName
                "@NsVar"
                "un_Funptr_typedef2"},
            typedefType = TypePointer
              (TypeFun
                []
                (TypePointer
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_typedef2",
          commentLocation = Just
            "reparse.h:133:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Funptr_typedef3",
      newtypeConstr = HsName
        "@NsConstr"
        "Funptr_typedef3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Funptr_typedef3",
        fieldType = HsFunPtr
          (HsIO
            (HsPtr
              (HsPtr
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "A"))))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:134:16",
          declId = NamePair {
            nameC = Name "funptr_typedef3",
            nameHsIdent = HsIdentifier
              "Funptr_typedef3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Funptr_typedef3",
              newtypeField = HsName
                "@NsVar"
                "un_Funptr_typedef3"},
            typedefType = TypePointer
              (TypeFun
                []
                (TypePointer
                  (TypePointer
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_typedef3",
          commentLocation = Just
            "reparse.h:134:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Funptr_typedef4",
      newtypeConstr = HsName
        "@NsConstr"
        "Funptr_typedef4",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Funptr_typedef4",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "A"))))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:135:16",
          declId = NamePair {
            nameC = Name "funptr_typedef4",
            nameHsIdent = HsIdentifier
              "Funptr_typedef4"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Funptr_typedef4",
              newtypeField = HsName
                "@NsVar"
                "un_Funptr_typedef4"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_typedef4",
          commentLocation = Just
            "reparse.h:135:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Funptr_typedef5",
      newtypeConstr = HsName
        "@NsConstr"
        "Funptr_typedef5",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Funptr_typedef5",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsPtr
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "A")))))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:136:16",
          declId = NamePair {
            nameC = Name "funptr_typedef5",
            nameHsIdent = HsIdentifier
              "Funptr_typedef5"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Funptr_typedef5",
              newtypeField = HsName
                "@NsVar"
                "un_Funptr_typedef5"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypePointer
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_typedef5",
          commentLocation = Just
            "reparse.h:136:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Funptr_typedef5",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "comments1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c5d7b3da7f176eba",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_c5d7b3da7f176eba (A arg1) { comments1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Comments in awkward places"],
          commentOrigin = Just
            "comments1",
          commentLocation = Just
            "reparse.h:144:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = [
            Paragraph
              [
                TextContent
                  "(Prior to language-c we failed to parse there.)"]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_c8f28692a6df33a4",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c8f28692a6df33a4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_comments1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_c8f28692a6df33a4 (void)) (A arg1) { return &comments1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Comments in awkward places"],
          commentOrigin = Just
            "comments1",
          commentLocation = Just
            "reparse.h:144:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = [
            Paragraph
              [
                TextContent
                  "(Prior to language-c we failed to parse there.)"]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Comments2",
      newtypeConstr = HsName
        "@NsConstr"
        "Comments2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Comments2",
        fieldType = HsTypRef
          (HsName "@NsTypeConstr" "A"),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:145:30",
          declId = NamePair {
            nameC = Name "comments2",
            nameHsIdent = HsIdentifier
              "Comments2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Comments2",
              newtypeField = HsName
                "@NsVar"
                "un_Comments2"},
            typedefType = TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource},
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
          commentOrigin = Just
            "comments2",
          commentLocation = Just
            "reparse.h:145:30",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
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
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Comments2",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Example_struct",
      structConstr = HsName
        "@NsConstr"
        "Example_struct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "example_struct_field1",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "reparse.h:152:8",
                fieldName = NamePair {
                  nameC = Name "field1",
                  nameHsIdent = HsIdentifier
                    "example_struct_field1"},
                fieldComment = Nothing},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field1",
              commentLocation = Just
                "reparse.h:152:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_struct_field2",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "reparse.h:153:8",
                fieldName = NamePair {
                  nameC = Name "field2",
                  nameHsIdent = HsIdentifier
                    "example_struct_field2"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field2",
              commentLocation = Just
                "reparse.h:153:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_struct_field3",
          fieldType = HsPtr
            (HsPtr
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "reparse.h:154:8",
                fieldName = NamePair {
                  nameC = Name "field3",
                  nameHsIdent = HsIdentifier
                    "example_struct_field3"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypePointer
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)),
              structFieldOffset = 128,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "field3",
              commentLocation = Just
                "reparse.h:154:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "reparse.h:151:8",
            declId = NamePair {
              nameC = Name "example_struct",
              nameHsIdent = HsIdentifier
                "Example_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["reparse.h"],
                headerInclude = "reparse.h"},
            declComment = Just
              (Comment
                [
                  Paragraph
                    [
                      TextContent "Struct fields"]])},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Example_struct"),
              structSizeof = 24,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:152:8",
                    fieldName = NamePair {
                      nameC = Name "field1",
                      nameHsIdent = HsIdentifier
                        "example_struct_field1"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:153:8",
                    fieldName = NamePair {
                      nameC = Name "field2",
                      nameHsIdent = HsIdentifier
                        "example_struct_field2"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:154:8",
                    fieldName = NamePair {
                      nameC = Name "field3",
                      nameHsIdent = HsIdentifier
                        "example_struct_field3"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypePointer
                      (TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource)),
                  structFieldOffset = 128,
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
          commentTitle = Just
            [TextContent "Struct fields"],
          commentOrigin = Just
            "example_struct",
          commentLocation = Just
            "reparse.h:151:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Example_struct",
          structConstr = HsName
            "@NsConstr"
            "Example_struct",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "example_struct_field1",
              fieldType = HsTypRef
                (HsName "@NsTypeConstr" "A"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:152:8",
                    fieldName = NamePair {
                      nameC = Name "field1",
                      nameHsIdent = HsIdentifier
                        "example_struct_field1"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field1",
                  commentLocation = Just
                    "reparse.h:152:8",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["reparse.h"],
                      headerInclude = "reparse.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "example_struct_field2",
              fieldType = HsPtr
                (HsTypRef
                  (HsName "@NsTypeConstr" "A")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:153:8",
                    fieldName = NamePair {
                      nameC = Name "field2",
                      nameHsIdent = HsIdentifier
                        "example_struct_field2"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field2",
                  commentLocation = Just
                    "reparse.h:153:8",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["reparse.h"],
                      headerInclude = "reparse.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "example_struct_field3",
              fieldType = HsPtr
                (HsPtr
                  (HsTypRef
                    (HsName "@NsTypeConstr" "A"))),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:154:8",
                    fieldName = NamePair {
                      nameC = Name "field3",
                      nameHsIdent = HsIdentifier
                        "example_struct_field3"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypePointer
                      (TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource)),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "field3",
                  commentLocation = Just
                    "reparse.h:154:8",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["reparse.h"],
                      headerInclude = "reparse.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "reparse.h:151:8",
                declId = NamePair {
                  nameC = Name "example_struct",
                  nameHsIdent = HsIdentifier
                    "Example_struct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["reparse.h"],
                    headerInclude = "reparse.h"},
                declComment = Just
                  (Comment
                    [
                      Paragraph
                        [
                          TextContent "Struct fields"]])},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Example_struct"),
                  structSizeof = 24,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "reparse.h:152:8",
                        fieldName = NamePair {
                          nameC = Name "field1",
                          nameHsIdent = HsIdentifier
                            "example_struct_field1"},
                        fieldComment = Nothing},
                      structFieldType =
                      TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "reparse.h:153:8",
                        fieldName = NamePair {
                          nameC = Name "field2",
                          nameHsIdent = HsIdentifier
                            "example_struct_field2"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeMacroTypedef
                          NamePair {
                            nameC = Name "A",
                            nameHsIdent = HsIdentifier "A"}
                          NameOriginInSource),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "reparse.h:154:8",
                        fieldName = NamePair {
                          nameC = Name "field3",
                          nameHsIdent = HsIdentifier
                            "example_struct_field3"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypePointer
                          (TypeMacroTypedef
                            NamePair {
                              nameC = Name "A",
                              nameHsIdent = HsIdentifier "A"}
                            NameOriginInSource)),
                      structFieldOffset = 128,
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
              commentTitle = Just
                [TextContent "Struct fields"],
              commentOrigin = Just
                "example_struct",
              commentLocation = Just
                "reparse.h:151:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 24,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Example_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "Example_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_field1",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "A"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:152:8",
                            fieldName = NamePair {
                              nameC = Name "field1",
                              nameHsIdent = HsIdentifier
                                "example_struct_field1"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "A",
                              nameHsIdent = HsIdentifier "A"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field1",
                          commentLocation = Just
                            "reparse.h:152:8",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_field2",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:153:8",
                            fieldName = NamePair {
                              nameC = Name "field2",
                              nameHsIdent = HsIdentifier
                                "example_struct_field2"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "A",
                                nameHsIdent = HsIdentifier "A"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field2",
                          commentLocation = Just
                            "reparse.h:153:8",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_field3",
                      fieldType = HsPtr
                        (HsPtr
                          (HsTypRef
                            (HsName "@NsTypeConstr" "A"))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:154:8",
                            fieldName = NamePair {
                              nameC = Name "field3",
                              nameHsIdent = HsIdentifier
                                "example_struct_field3"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypePointer
                              (TypeMacroTypedef
                                NamePair {
                                  nameC = Name "A",
                                  nameHsIdent = HsIdentifier "A"}
                                NameOriginInSource)),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field3",
                          commentLocation = Just
                            "reparse.h:154:8",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "reparse.h:151:8",
                        declId = NamePair {
                          nameC = Name "example_struct",
                          nameHsIdent = HsIdentifier
                            "Example_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["reparse.h"],
                            headerInclude = "reparse.h"},
                        declComment = Just
                          (Comment
                            [
                              Paragraph
                                [
                                  TextContent "Struct fields"]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Example_struct"),
                          structSizeof = 24,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:152:8",
                                fieldName = NamePair {
                                  nameC = Name "field1",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_field1"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "A",
                                  nameHsIdent = HsIdentifier "A"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:153:8",
                                fieldName = NamePair {
                                  nameC = Name "field2",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_field2"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "A",
                                    nameHsIdent = HsIdentifier "A"}
                                  NameOriginInSource),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:154:8",
                                fieldName = NamePair {
                                  nameC = Name "field3",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_field3"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypePointer
                                  (TypeMacroTypedef
                                    NamePair {
                                      nameC = Name "A",
                                      nameHsIdent = HsIdentifier "A"}
                                    NameOriginInSource)),
                              structFieldOffset = 128,
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
                      commentTitle = Just
                        [TextContent "Struct fields"],
                      commentOrigin = Just
                        "example_struct",
                      commentLocation = Just
                        "reparse.h:151:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["reparse.h"],
                          headerInclude = "reparse.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 8,
                PeekByteOff (Idx 0) 16]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Example_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "Example_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_field1",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "A"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:152:8",
                            fieldName = NamePair {
                              nameC = Name "field1",
                              nameHsIdent = HsIdentifier
                                "example_struct_field1"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "A",
                              nameHsIdent = HsIdentifier "A"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field1",
                          commentLocation = Just
                            "reparse.h:152:8",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_field2",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:153:8",
                            fieldName = NamePair {
                              nameC = Name "field2",
                              nameHsIdent = HsIdentifier
                                "example_struct_field2"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "A",
                                nameHsIdent = HsIdentifier "A"}
                              NameOriginInSource),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field2",
                          commentLocation = Just
                            "reparse.h:153:8",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_field3",
                      fieldType = HsPtr
                        (HsPtr
                          (HsTypRef
                            (HsName "@NsTypeConstr" "A"))),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:154:8",
                            fieldName = NamePair {
                              nameC = Name "field3",
                              nameHsIdent = HsIdentifier
                                "example_struct_field3"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypePointer
                              (TypeMacroTypedef
                                NamePair {
                                  nameC = Name "A",
                                  nameHsIdent = HsIdentifier "A"}
                                NameOriginInSource)),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "field3",
                          commentLocation = Just
                            "reparse.h:154:8",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "reparse.h:151:8",
                        declId = NamePair {
                          nameC = Name "example_struct",
                          nameHsIdent = HsIdentifier
                            "Example_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["reparse.h"],
                            headerInclude = "reparse.h"},
                        declComment = Just
                          (Comment
                            [
                              Paragraph
                                [
                                  TextContent "Struct fields"]])},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Example_struct"),
                          structSizeof = 24,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:152:8",
                                fieldName = NamePair {
                                  nameC = Name "field1",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_field1"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "A",
                                  nameHsIdent = HsIdentifier "A"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:153:8",
                                fieldName = NamePair {
                                  nameC = Name "field2",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_field2"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "A",
                                    nameHsIdent = HsIdentifier "A"}
                                  NameOriginInSource),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:154:8",
                                fieldName = NamePair {
                                  nameC = Name "field3",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_field3"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypePointer
                                  (TypeMacroTypedef
                                    NamePair {
                                      nameC = Name "A",
                                      nameHsIdent = HsIdentifier "A"}
                                    NameOriginInSource)),
                              structFieldOffset = 128,
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
                      commentTitle = Just
                        [TextContent "Struct fields"],
                      commentOrigin = Just
                        "example_struct",
                      commentLocation = Just
                        "reparse.h:151:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["reparse.h"],
                          headerInclude = "reparse.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 8 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      16
                      (Idx 2)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Example_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Example_struct",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_prim_before1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c33e21f21d3c0189",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_c33e21f21d3c0189 (A arg1, char const arg2) { const_prim_before1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit Nothing))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment =
      Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "`const` qualifier"],
          commentOrigin = Just
            "const_prim_before1",
          commentLocation = Just
            "reparse.h:179:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "NOTE: These were not parsed correctly prior to the switch to language-c."]]},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_e11e4b5f44bc145c",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCChar)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e11e4b5f44bc145c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_prim_before1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e11e4b5f44bc145c (void)) (A arg1, char const arg2) { return &const_prim_before1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePrim
                (PrimChar
                  (PrimSignImplicit Nothing)))]
          TypeVoid),
      foreignImportComment =
      Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "`const` qualifier"],
          commentOrigin = Just
            "const_prim_before1",
          commentLocation = Just
            "reparse.h:179:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "NOTE: These were not parsed correctly prior to the switch to language-c."]]},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_prim_before2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCSChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_96a22f4d00d1024e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_96a22f4d00d1024e (A arg1, signed char const arg2) { const_prim_before2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim
                  (PrimChar
                    (PrimSignExplicit Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_prim_before2",
          commentLocation = Just
            "reparse.h:180:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_2735f43b5743f988",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCSChar)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_2735f43b5743f988",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_prim_before2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_2735f43b5743f988 (void)) (A arg1, signed char const arg2) { return &const_prim_before2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePrim
                (PrimChar
                  (PrimSignExplicit Signed)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_prim_before2",
          commentLocation = Just
            "reparse.h:180:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_prim_before3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCUChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_0a38436fbc3be248",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_0a38436fbc3be248 (A arg1, unsigned char const arg2) { const_prim_before3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim
                  (PrimChar
                    (PrimSignExplicit Unsigned))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_prim_before3",
          commentLocation = Just
            "reparse.h:181:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_e4cb72d2f1df53fd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCUChar)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e4cb72d2f1df53fd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_prim_before3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e4cb72d2f1df53fd (void)) (A arg1, unsigned char const arg2) { return &const_prim_before3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_prim_before3",
          commentLocation = Just
            "reparse.h:181:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_prim_after1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_718696d7ee1d51a0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_718696d7ee1d51a0 (A arg1, char const arg2) { const_prim_after1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit Nothing))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_prim_after1",
          commentLocation = Just
            "reparse.h:182:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_c17bd5a020927e9e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCChar)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c17bd5a020927e9e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_prim_after1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_c17bd5a020927e9e (void)) (A arg1, char const arg2) { return &const_prim_after1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePrim
                (PrimChar
                  (PrimSignImplicit Nothing)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_prim_after1",
          commentLocation = Just
            "reparse.h:182:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_prim_after2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCSChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9c95677f7c61a6f5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_9c95677f7c61a6f5 (A arg1, signed char const arg2) { const_prim_after2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim
                  (PrimChar
                    (PrimSignExplicit Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_prim_after2",
          commentLocation = Just
            "reparse.h:183:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_aaadc77459f026a4",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCSChar)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_aaadc77459f026a4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_prim_after2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_aaadc77459f026a4 (void)) (A arg1, signed char const arg2) { return &const_prim_after2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePrim
                (PrimChar
                  (PrimSignExplicit Signed)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_prim_after2",
          commentLocation = Just
            "reparse.h:183:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_prim_after3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCUChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_2b5b3d1fc6859ba3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_2b5b3d1fc6859ba3 (A arg1, unsigned char const arg2) { const_prim_after3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim
                  (PrimChar
                    (PrimSignExplicit Unsigned))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_prim_after3",
          commentLocation = Just
            "reparse.h:184:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_d08efe58f7551f9c",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCUChar)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d08efe58f7551f9c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_prim_after3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_d08efe58f7551f9c (void)) (A arg1, unsigned char const arg2) { return &const_prim_after3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_prim_after3",
          commentLocation = Just
            "reparse.h:184:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_before1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9d57b2fa38d5f7bc",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_9d57b2fa38d5f7bc (A arg1, float const arg2) { const_withoutSign_before1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim
                  (PrimFloating PrimFloat)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before1",
          commentLocation = Just
            "reparse.h:188:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_7327c6cd454253cf",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCFloat)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_7327c6cd454253cf",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_before1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_7327c6cd454253cf (void)) (A arg1, float const arg2) { return &const_withoutSign_before1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePrim
                (PrimFloating PrimFloat))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before1",
          commentLocation = Just
            "reparse.h:188:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_before2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_52eba7cca7385d7d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_52eba7cca7385d7d (A arg1, double const arg2) { const_withoutSign_before2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim
                  (PrimFloating PrimDouble)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before2",
          commentLocation = Just
            "reparse.h:189:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_1ff1052f6d3279e6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_1ff1052f6d3279e6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_before2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_1ff1052f6d3279e6 (void)) (A arg1, double const arg2) { return &const_withoutSign_before2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePrim
                (PrimFloating PrimDouble))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before2",
          commentLocation = Just
            "reparse.h:189:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_before3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCBool,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_96b5756c596ae9a2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_96b5756c596ae9a2 (A arg1, _Bool const arg2) { const_withoutSign_before3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim PrimBool))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before3",
          commentLocation = Just
            "reparse.h:190:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_72fd083ea0be94e7",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCBool)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_72fd083ea0be94e7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_before3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_72fd083ea0be94e7 (void)) (A arg1, _Bool const arg2) { return &const_withoutSign_before3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst (TypePrim PrimBool)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before3",
          commentLocation = Just
            "reparse.h:190:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_before4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Some_struct"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_4dee4a646d2da9ed",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_4dee4a646d2da9ed (A arg1, struct some_struct const arg2) { const_withoutSign_before4(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypeStruct
                  NamePair {
                    nameC = Name "some_struct",
                    nameHsIdent = HsIdentifier
                      "Some_struct"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before4",
          commentLocation = Just
            "reparse.h:191:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_9087b446588f2208",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_struct"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9087b446588f2208",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_before4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_9087b446588f2208 (void)) (A arg1, struct some_struct const arg2) { return &const_withoutSign_before4; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypeStruct
                NamePair {
                  nameC = Name "some_struct",
                  nameHsIdent = HsIdentifier
                    "Some_struct"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before4",
          commentLocation = Just
            "reparse.h:191:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_before5",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Some_union"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c944f84b146c925d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_c944f84b146c925d (A arg1, union some_union const arg2) { const_withoutSign_before5(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypeUnion
                  NamePair {
                    nameC = Name "some_union",
                    nameHsIdent = HsIdentifier
                      "Some_union"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before5",
          commentLocation = Just
            "reparse.h:192:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_c8da645320ac5f3d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_union"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c8da645320ac5f3d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_before5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_c8da645320ac5f3d (void)) (A arg1, union some_union const arg2) { return &const_withoutSign_before5; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypeUnion
                NamePair {
                  nameC = Name "some_union",
                  nameHsIdent = HsIdentifier
                    "Some_union"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before5",
          commentLocation = Just
            "reparse.h:192:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_before6",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Some_enum"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_5fc3275406c102f7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_5fc3275406c102f7 (A arg1, enum some_enum const arg2) { const_withoutSign_before6(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypeEnum
                  NamePair {
                    nameC = Name "some_enum",
                    nameHsIdent = HsIdentifier
                      "Some_enum"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before6",
          commentLocation = Just
            "reparse.h:193:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_e87b52f038c210ec",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_enum"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e87b52f038c210ec",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_before6_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e87b52f038c210ec (void)) (A arg1, enum some_enum const arg2) { return &const_withoutSign_before6; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypeEnum
                NamePair {
                  nameC = Name "some_enum",
                  nameHsIdent = HsIdentifier
                    "Some_enum"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before6",
          commentLocation = Just
            "reparse.h:193:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_before7",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCBool,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ea6deb36fcb8d482",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_ea6deb36fcb8d482 (A arg1, _Bool const arg2) { const_withoutSign_before7(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim PrimBool))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before7",
          commentLocation = Just
            "reparse.h:194:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_e6e725a648481bf4",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCBool)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e6e725a648481bf4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_before7_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e6e725a648481bf4 (void)) (A arg1, _Bool const arg2) { return &const_withoutSign_before7; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst (TypePrim PrimBool)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before7",
          commentLocation = Just
            "reparse.h:194:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_before8",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCSize,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_7940b984b4ebe7f5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_7940b984b4ebe7f5 (A arg1, size_t const arg2) { const_withoutSign_before8(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim PrimSize))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before8",
          commentLocation = Just
            "reparse.h:195:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_cf5b220806fc5356",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCSize)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_cf5b220806fc5356",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_before8_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_cf5b220806fc5356 (void)) (A arg1, size_t const arg2) { return &const_withoutSign_before8; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst (TypePrim PrimSize)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_before8",
          commentLocation = Just
            "reparse.h:195:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_after1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCFloat,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_0aa519784a037e2f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_0aa519784a037e2f (A arg1, float const arg2) { const_withoutSign_after1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim
                  (PrimFloating PrimFloat)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after1",
          commentLocation = Just
            "reparse.h:197:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_92eb08cf64fe2e87",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCFloat)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_92eb08cf64fe2e87",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_after1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_92eb08cf64fe2e87 (void)) (A arg1, float const arg2) { return &const_withoutSign_after1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePrim
                (PrimFloating PrimFloat))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after1",
          commentLocation = Just
            "reparse.h:197:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_after2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ee60f796acb178b7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_ee60f796acb178b7 (A arg1, double const arg2) { const_withoutSign_after2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim
                  (PrimFloating PrimDouble)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after2",
          commentLocation = Just
            "reparse.h:198:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_8c0f96fa4264c653",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_8c0f96fa4264c653",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_after2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_8c0f96fa4264c653 (void)) (A arg1, double const arg2) { return &const_withoutSign_after2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePrim
                (PrimFloating PrimDouble))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after2",
          commentLocation = Just
            "reparse.h:198:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_after3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCBool,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_56c75f3b604d0b43",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_56c75f3b604d0b43 (A arg1, _Bool const arg2) { const_withoutSign_after3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim PrimBool))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after3",
          commentLocation = Just
            "reparse.h:199:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_0eef09188651c221",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCBool)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_0eef09188651c221",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_after3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_0eef09188651c221 (void)) (A arg1, _Bool const arg2) { return &const_withoutSign_after3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst (TypePrim PrimBool)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after3",
          commentLocation = Just
            "reparse.h:199:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_after4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Some_struct"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d3022bf1fee59add",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_d3022bf1fee59add (A arg1, struct some_struct const arg2) { const_withoutSign_after4(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypeStruct
                  NamePair {
                    nameC = Name "some_struct",
                    nameHsIdent = HsIdentifier
                      "Some_struct"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after4",
          commentLocation = Just
            "reparse.h:200:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_a383627604fb7e55",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_struct"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_a383627604fb7e55",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_after4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a383627604fb7e55 (void)) (A arg1, struct some_struct const arg2) { return &const_withoutSign_after4; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypeStruct
                NamePair {
                  nameC = Name "some_struct",
                  nameHsIdent = HsIdentifier
                    "Some_struct"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after4",
          commentLocation = Just
            "reparse.h:200:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_after5",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Some_union"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_79a3d4ac2b99caa8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_79a3d4ac2b99caa8 (A arg1, union some_union const arg2) { const_withoutSign_after5(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypeUnion
                  NamePair {
                    nameC = Name "some_union",
                    nameHsIdent = HsIdentifier
                      "Some_union"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after5",
          commentLocation = Just
            "reparse.h:201:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_8de35e5f14909162",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_union"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_8de35e5f14909162",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_after5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_8de35e5f14909162 (void)) (A arg1, union some_union const arg2) { return &const_withoutSign_after5; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypeUnion
                NamePair {
                  nameC = Name "some_union",
                  nameHsIdent = HsIdentifier
                    "Some_union"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after5",
          commentLocation = Just
            "reparse.h:201:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_after6",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Some_enum"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_53ec664f2511ac04",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_53ec664f2511ac04 (A arg1, enum some_enum const arg2) { const_withoutSign_after6(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypeEnum
                  NamePair {
                    nameC = Name "some_enum",
                    nameHsIdent = HsIdentifier
                      "Some_enum"}
                  NameOriginInSource))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after6",
          commentLocation = Just
            "reparse.h:202:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_66230a5bf8ef3e40",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Some_enum"))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_66230a5bf8ef3e40",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_after6_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_66230a5bf8ef3e40 (void)) (A arg1, enum some_enum const arg2) { return &const_withoutSign_after6; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypeEnum
                NamePair {
                  nameC = Name "some_enum",
                  nameHsIdent = HsIdentifier
                    "Some_enum"}
                NameOriginInSource)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after6",
          commentLocation = Just
            "reparse.h:202:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_after7",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCBool,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d07aafa2007b7654",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_d07aafa2007b7654 (A arg1, _Bool const arg2) { const_withoutSign_after7(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim PrimBool))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after7",
          commentLocation = Just
            "reparse.h:203:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_a2bb46ca9f36cc46",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCBool)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_a2bb46ca9f36cc46",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_after7_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a2bb46ca9f36cc46 (void)) (A arg1, _Bool const arg2) { return &const_withoutSign_after7; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst (TypePrim PrimBool)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after7",
          commentLocation = Just
            "reparse.h:203:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_withoutSign_after8",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType =
          HsPrimType HsPrimCSize,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_5f3b8ee6bb9b0dfb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_5f3b8ee6bb9b0dfb (A arg1, size_t const arg2) { const_withoutSign_after8(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePrim PrimSize))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after8",
          commentLocation = Just
            "reparse.h:204:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_4777111d7551332e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPrimType HsPrimCSize)
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_4777111d7551332e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_withoutSign_after8_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_4777111d7551332e (void)) (A arg1, size_t const arg2) { return &const_withoutSign_after8; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst (TypePrim PrimSize)]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_withoutSign_after8",
          commentLocation = Just
            "reparse.h:204:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_pointers_args1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_a79f56af6e9ef2f7",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_a79f56af6e9ef2f7 (A arg1, signed int const *arg2) { const_pointers_args1(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_args1",
          commentLocation = Just
            "reparse.h:208:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_ad5aa005051441d6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPtr (HsPrimType HsPrimCInt))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ad5aa005051441d6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_pointers_args1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_ad5aa005051441d6 (void)) (A arg1, signed int const *arg2) { return &const_pointers_args1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer
              (TypeConst
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_args1",
          commentLocation = Just
            "reparse.h:208:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_pointers_args2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_022f5aa1f6eb033a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_022f5aa1f6eb033a (A arg1, signed int const *arg2) { const_pointers_args2(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_args2",
          commentLocation = Just
            "reparse.h:209:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_205f50942ba9b45e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPtr (HsPrimType HsPrimCInt))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_205f50942ba9b45e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_pointers_args2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_205f50942ba9b45e (void)) (A arg1, signed int const *arg2) { return &const_pointers_args2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer
              (TypeConst
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_args2",
          commentLocation = Just
            "reparse.h:209:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_pointers_args3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_39ad759f70b99e37",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_39ad759f70b99e37 (A arg1, signed int *const arg2) { const_pointers_args3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePointer
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_args3",
          commentLocation = Just
            "reparse.h:210:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_c2ddece5d773a650",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPtr (HsPrimType HsPrimCInt))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c2ddece5d773a650",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_pointers_args3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_c2ddece5d773a650 (void)) (A arg1, signed int *const arg2) { return &const_pointers_args3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePointer
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_args3",
          commentLocation = Just
            "reparse.h:210:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_pointers_args4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d4a5973dc0abcd97",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_d4a5973dc0abcd97 (A arg1, signed int const *const arg2) { const_pointers_args4(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePointer
                  (TypeConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_args4",
          commentLocation = Just
            "reparse.h:211:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_ed9daaf3a67948dd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPtr (HsPrimType HsPrimCInt))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ed9daaf3a67948dd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_pointers_args4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_ed9daaf3a67948dd (void)) (A arg1, signed int const *const arg2) { return &const_pointers_args4; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_args4",
          commentLocation = Just
            "reparse.h:211:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_pointers_args5",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCInt),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_1ed4abb42ab52f46",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_1ed4abb42ab52f46 (A arg1, signed int const *const arg2) { const_pointers_args5(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypeConst
                (TypePointer
                  (TypeConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_args5",
          commentLocation = Just
            "reparse.h:212:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_4b329c596dbf249e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsPtr (HsPrimType HsPrimCInt))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_4b329c596dbf249e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_pointers_args5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_4b329c596dbf249e (void)) (A arg1, signed int const *const arg2) { return &const_pointers_args5; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypeConst
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_args5",
          commentLocation = Just
            "reparse.h:212:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_pointers_ret1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_7d43de2d59af106e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int const *hs_bindgen_test_reparse_7d43de2d59af106e (A arg1) { return const_pointers_ret1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeConst
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_ret1",
          commentLocation = Just
            "reparse.h:214:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_fb3f484345aeb2d2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_fb3f484345aeb2d2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_pointers_ret1_ptr */ __attribute__ ((const)) signed int const *(*hs_bindgen_test_reparse_fb3f484345aeb2d2 (void)) (A arg1) { return &const_pointers_ret1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeConst
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_ret1",
          commentLocation = Just
            "reparse.h:214:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_pointers_ret2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_b7a5da1fec42008c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int const *hs_bindgen_test_reparse_b7a5da1fec42008c (A arg1) { return const_pointers_ret2(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeConst
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_ret2",
          commentLocation = Just
            "reparse.h:215:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_4242fece463aa185",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_4242fece463aa185",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_pointers_ret2_ptr */ __attribute__ ((const)) signed int const *(*hs_bindgen_test_reparse_4242fece463aa185 (void)) (A arg1) { return &const_pointers_ret2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeConst
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_ret2",
          commentLocation = Just
            "reparse.h:215:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_pointers_ret3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ea139108aca83634",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int *const hs_bindgen_test_reparse_ea139108aca83634 (A arg1) { return const_pointers_ret3(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeConst
            (TypePointer
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_ret3",
          commentLocation = Just
            "reparse.h:216:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_d88b6e752a8081c3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d88b6e752a8081c3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_pointers_ret3_ptr */ __attribute__ ((const)) signed int *const (*hs_bindgen_test_reparse_d88b6e752a8081c3 (void)) (A arg1) { return &const_pointers_ret3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypeConst
            (TypePointer
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_ret3",
          commentLocation = Just
            "reparse.h:216:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_pointers_ret4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ab3c3cf6c30a91a4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int const *const hs_bindgen_test_reparse_ab3c3cf6c30a91a4 (A arg1) { return const_pointers_ret4(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeConst
            (TypePointer
              (TypeConst
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_ret4",
          commentLocation = Just
            "reparse.h:217:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_ae606f6ad11597ce",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ae606f6ad11597ce",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_pointers_ret4_ptr */ __attribute__ ((const)) signed int const *const (*hs_bindgen_test_reparse_ae606f6ad11597ce (void)) (A arg1) { return &const_pointers_ret4; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypeConst
            (TypePointer
              (TypeConst
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_ret4",
          commentLocation = Just
            "reparse.h:217:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_pointers_ret5",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e32a2dec62135c7a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int const *const hs_bindgen_test_reparse_e32a2dec62135c7a (A arg1) { return const_pointers_ret5(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeConst
            (TypePointer
              (TypeConst
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_ret5",
          commentLocation = Just
            "reparse.h:218:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_c86348709b464f9b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_c86348709b464f9b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_pointers_ret5_ptr */ __attribute__ ((const)) signed int const *const (*hs_bindgen_test_reparse_c86348709b464f9b (void)) (A arg1) { return &const_pointers_ret5; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypeConst
            (TypePointer
              (TypeConst
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_pointers_ret5",
          commentLocation = Just
            "reparse.h:218:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_typedef1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_typedef1",
        fieldType = HsTypRef
          (HsName "@NsTypeConstr" "A"),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:220:25",
          declId = NamePair {
            nameC = Name "const_typedef1",
            nameHsIdent = HsIdentifier
              "Const_typedef1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_typedef1",
              newtypeField = HsName
                "@NsVar"
                "un_Const_typedef1"},
            typedefType = TypeConst
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)},
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
          commentOrigin = Just
            "const_typedef1",
          commentLocation = Just
            "reparse.h:220:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
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
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_typedef2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_typedef2",
        fieldType = HsTypRef
          (HsName "@NsTypeConstr" "A"),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:221:25",
          declId = NamePair {
            nameC = Name "const_typedef2",
            nameHsIdent = HsIdentifier
              "Const_typedef2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_typedef2",
              newtypeField = HsName
                "@NsVar"
                "un_Const_typedef2"},
            typedefType = TypeConst
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)},
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
          commentOrigin = Just
            "const_typedef2",
          commentLocation = Just
            "reparse.h:221:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
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
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_typedef3",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_typedef3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_typedef3",
        fieldType = HsPtr
          (HsTypRef
            (HsName "@NsTypeConstr" "A")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:222:25",
          declId = NamePair {
            nameC = Name "const_typedef3",
            nameHsIdent = HsIdentifier
              "Const_typedef3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_typedef3",
              newtypeField = HsName
                "@NsVar"
                "un_Const_typedef3"},
            typedefType = TypePointer
              (TypeConst
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_typedef3",
          commentLocation = Just
            "reparse.h:222:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef3",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_typedef4",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_typedef4",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_typedef4",
        fieldType = HsPtr
          (HsTypRef
            (HsName "@NsTypeConstr" "A")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:223:25",
          declId = NamePair {
            nameC = Name "const_typedef4",
            nameHsIdent = HsIdentifier
              "Const_typedef4"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_typedef4",
              newtypeField = HsName
                "@NsVar"
                "un_Const_typedef4"},
            typedefType = TypePointer
              (TypeConst
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_typedef4",
          commentLocation = Just
            "reparse.h:223:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef4",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_typedef5",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_typedef5",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_typedef5",
        fieldType = HsPtr
          (HsTypRef
            (HsName "@NsTypeConstr" "A")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:224:25",
          declId = NamePair {
            nameC = Name "const_typedef5",
            nameHsIdent = HsIdentifier
              "Const_typedef5"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_typedef5",
              newtypeField = HsName
                "@NsVar"
                "un_Const_typedef5"},
            typedefType = TypeConst
              (TypePointer
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_typedef5",
          commentLocation = Just
            "reparse.h:224:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef5",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_typedef6",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_typedef6",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_typedef6",
        fieldType = HsPtr
          (HsTypRef
            (HsName "@NsTypeConstr" "A")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:225:25",
          declId = NamePair {
            nameC = Name "const_typedef6",
            nameHsIdent = HsIdentifier
              "Const_typedef6"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_typedef6",
              newtypeField = HsName
                "@NsVar"
                "un_Const_typedef6"},
            typedefType = TypeConst
              (TypePointer
                (TypeConst
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_typedef6",
          commentLocation = Just
            "reparse.h:225:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef6",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef6",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef6",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef6",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_typedef7",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_typedef7",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_typedef7",
        fieldType = HsPtr
          (HsTypRef
            (HsName "@NsTypeConstr" "A")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:226:25",
          declId = NamePair {
            nameC = Name "const_typedef7",
            nameHsIdent = HsIdentifier
              "Const_typedef7"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_typedef7",
              newtypeField = HsName
                "@NsVar"
                "un_Const_typedef7"},
            typedefType = TypeConst
              (TypePointer
                (TypeConst
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_typedef7",
          commentLocation = Just
            "reparse.h:226:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef7",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef7",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef7",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_typedef7",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Example_struct_with_const",
      structConstr = HsName
        "@NsConstr"
        "Example_struct_with_const",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "example_struct_with_const_const_field1",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "reparse.h:229:19",
                fieldName = NamePair {
                  nameC = Name "const_field1",
                  nameHsIdent = HsIdentifier
                    "example_struct_with_const_const_field1"},
                fieldComment = Nothing},
              structFieldType = TypeConst
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "const_field1",
              commentLocation = Just
                "reparse.h:229:19",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_struct_with_const_const_field2",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "reparse.h:230:19",
                fieldName = NamePair {
                  nameC = Name "const_field2",
                  nameHsIdent = HsIdentifier
                    "example_struct_with_const_const_field2"},
                fieldComment = Nothing},
              structFieldType = TypeConst
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "const_field2",
              commentLocation = Just
                "reparse.h:230:19",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_struct_with_const_const_field3",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "reparse.h:231:19",
                fieldName = NamePair {
                  nameC = Name "const_field3",
                  nameHsIdent = HsIdentifier
                    "example_struct_with_const_const_field3"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeConst
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "const_field3",
              commentLocation = Just
                "reparse.h:231:19",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_struct_with_const_const_field4",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "reparse.h:232:19",
                fieldName = NamePair {
                  nameC = Name "const_field4",
                  nameHsIdent = HsIdentifier
                    "example_struct_with_const_const_field4"},
                fieldComment = Nothing},
              structFieldType = TypePointer
                (TypeConst
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)),
              structFieldOffset = 128,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "const_field4",
              commentLocation = Just
                "reparse.h:232:19",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_struct_with_const_const_field5",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "reparse.h:233:19",
                fieldName = NamePair {
                  nameC = Name "const_field5",
                  nameHsIdent = HsIdentifier
                    "example_struct_with_const_const_field5"},
                fieldComment = Nothing},
              structFieldType = TypeConst
                (TypePointer
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)),
              structFieldOffset = 192,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "const_field5",
              commentLocation = Just
                "reparse.h:233:19",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_struct_with_const_const_field6",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "reparse.h:234:19",
                fieldName = NamePair {
                  nameC = Name "const_field6",
                  nameHsIdent = HsIdentifier
                    "example_struct_with_const_const_field6"},
                fieldComment = Nothing},
              structFieldType = TypeConst
                (TypePointer
                  (TypeConst
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource))),
              structFieldOffset = 256,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "const_field6",
              commentLocation = Just
                "reparse.h:234:19",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_struct_with_const_const_field7",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A")),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "reparse.h:235:19",
                fieldName = NamePair {
                  nameC = Name "const_field7",
                  nameHsIdent = HsIdentifier
                    "example_struct_with_const_const_field7"},
                fieldComment = Nothing},
              structFieldType = TypeConst
                (TypePointer
                  (TypeConst
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource))),
              structFieldOffset = 320,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "const_field7",
              commentLocation = Just
                "reparse.h:235:19",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "reparse.h:228:8",
            declId = NamePair {
              nameC = Name
                "example_struct_with_const",
              nameHsIdent = HsIdentifier
                "Example_struct_with_const"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["reparse.h"],
                headerInclude = "reparse.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Example_struct_with_const"),
              structSizeof = 48,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:229:19",
                    fieldName = NamePair {
                      nameC = Name "const_field1",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field1"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:230:19",
                    fieldName = NamePair {
                      nameC = Name "const_field2",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field2"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:231:19",
                    fieldName = NamePair {
                      nameC = Name "const_field3",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field3"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeConst
                      (TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource)),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:232:19",
                    fieldName = NamePair {
                      nameC = Name "const_field4",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field4"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeConst
                      (TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource)),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:233:19",
                    fieldName = NamePair {
                      nameC = Name "const_field5",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field5"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypePointer
                      (TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource)),
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:234:19",
                    fieldName = NamePair {
                      nameC = Name "const_field6",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field6"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypePointer
                      (TypeConst
                        (TypeMacroTypedef
                          NamePair {
                            nameC = Name "A",
                            nameHsIdent = HsIdentifier "A"}
                          NameOriginInSource))),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:235:19",
                    fieldName = NamePair {
                      nameC = Name "const_field7",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field7"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypePointer
                      (TypeConst
                        (TypeMacroTypedef
                          NamePair {
                            nameC = Name "A",
                            nameHsIdent = HsIdentifier "A"}
                          NameOriginInSource))),
                  structFieldOffset = 320,
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
          commentOrigin = Just
            "example_struct_with_const",
          commentLocation = Just
            "reparse.h:228:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Example_struct_with_const",
          structConstr = HsName
            "@NsConstr"
            "Example_struct_with_const",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "example_struct_with_const_const_field1",
              fieldType = HsTypRef
                (HsName "@NsTypeConstr" "A"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:229:19",
                    fieldName = NamePair {
                      nameC = Name "const_field1",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field1"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "const_field1",
                  commentLocation = Just
                    "reparse.h:229:19",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["reparse.h"],
                      headerInclude = "reparse.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "example_struct_with_const_const_field2",
              fieldType = HsTypRef
                (HsName "@NsTypeConstr" "A"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:230:19",
                    fieldName = NamePair {
                      nameC = Name "const_field2",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field2"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "const_field2",
                  commentLocation = Just
                    "reparse.h:230:19",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["reparse.h"],
                      headerInclude = "reparse.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "example_struct_with_const_const_field3",
              fieldType = HsPtr
                (HsTypRef
                  (HsName "@NsTypeConstr" "A")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:231:19",
                    fieldName = NamePair {
                      nameC = Name "const_field3",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field3"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeConst
                      (TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource)),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "const_field3",
                  commentLocation = Just
                    "reparse.h:231:19",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["reparse.h"],
                      headerInclude = "reparse.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "example_struct_with_const_const_field4",
              fieldType = HsPtr
                (HsTypRef
                  (HsName "@NsTypeConstr" "A")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:232:19",
                    fieldName = NamePair {
                      nameC = Name "const_field4",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field4"},
                    fieldComment = Nothing},
                  structFieldType = TypePointer
                    (TypeConst
                      (TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource)),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "const_field4",
                  commentLocation = Just
                    "reparse.h:232:19",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["reparse.h"],
                      headerInclude = "reparse.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "example_struct_with_const_const_field5",
              fieldType = HsPtr
                (HsTypRef
                  (HsName "@NsTypeConstr" "A")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:233:19",
                    fieldName = NamePair {
                      nameC = Name "const_field5",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field5"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypePointer
                      (TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource)),
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "const_field5",
                  commentLocation = Just
                    "reparse.h:233:19",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["reparse.h"],
                      headerInclude = "reparse.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "example_struct_with_const_const_field6",
              fieldType = HsPtr
                (HsTypRef
                  (HsName "@NsTypeConstr" "A")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:234:19",
                    fieldName = NamePair {
                      nameC = Name "const_field6",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field6"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypePointer
                      (TypeConst
                        (TypeMacroTypedef
                          NamePair {
                            nameC = Name "A",
                            nameHsIdent = HsIdentifier "A"}
                          NameOriginInSource))),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "const_field6",
                  commentLocation = Just
                    "reparse.h:234:19",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["reparse.h"],
                      headerInclude = "reparse.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "example_struct_with_const_const_field7",
              fieldType = HsPtr
                (HsTypRef
                  (HsName "@NsTypeConstr" "A")),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "reparse.h:235:19",
                    fieldName = NamePair {
                      nameC = Name "const_field7",
                      nameHsIdent = HsIdentifier
                        "example_struct_with_const_const_field7"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypePointer
                      (TypeConst
                        (TypeMacroTypedef
                          NamePair {
                            nameC = Name "A",
                            nameHsIdent = HsIdentifier "A"}
                          NameOriginInSource))),
                  structFieldOffset = 320,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "const_field7",
                  commentLocation = Just
                    "reparse.h:235:19",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["reparse.h"],
                      headerInclude = "reparse.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "reparse.h:228:8",
                declId = NamePair {
                  nameC = Name
                    "example_struct_with_const",
                  nameHsIdent = HsIdentifier
                    "Example_struct_with_const"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["reparse.h"],
                    headerInclude = "reparse.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Example_struct_with_const"),
                  structSizeof = 48,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "reparse.h:229:19",
                        fieldName = NamePair {
                          nameC = Name "const_field1",
                          nameHsIdent = HsIdentifier
                            "example_struct_with_const_const_field1"},
                        fieldComment = Nothing},
                      structFieldType = TypeConst
                        (TypeMacroTypedef
                          NamePair {
                            nameC = Name "A",
                            nameHsIdent = HsIdentifier "A"}
                          NameOriginInSource),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "reparse.h:230:19",
                        fieldName = NamePair {
                          nameC = Name "const_field2",
                          nameHsIdent = HsIdentifier
                            "example_struct_with_const_const_field2"},
                        fieldComment = Nothing},
                      structFieldType = TypeConst
                        (TypeMacroTypedef
                          NamePair {
                            nameC = Name "A",
                            nameHsIdent = HsIdentifier "A"}
                          NameOriginInSource),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "reparse.h:231:19",
                        fieldName = NamePair {
                          nameC = Name "const_field3",
                          nameHsIdent = HsIdentifier
                            "example_struct_with_const_const_field3"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeConst
                          (TypeMacroTypedef
                            NamePair {
                              nameC = Name "A",
                              nameHsIdent = HsIdentifier "A"}
                            NameOriginInSource)),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "reparse.h:232:19",
                        fieldName = NamePair {
                          nameC = Name "const_field4",
                          nameHsIdent = HsIdentifier
                            "example_struct_with_const_const_field4"},
                        fieldComment = Nothing},
                      structFieldType = TypePointer
                        (TypeConst
                          (TypeMacroTypedef
                            NamePair {
                              nameC = Name "A",
                              nameHsIdent = HsIdentifier "A"}
                            NameOriginInSource)),
                      structFieldOffset = 128,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "reparse.h:233:19",
                        fieldName = NamePair {
                          nameC = Name "const_field5",
                          nameHsIdent = HsIdentifier
                            "example_struct_with_const_const_field5"},
                        fieldComment = Nothing},
                      structFieldType = TypeConst
                        (TypePointer
                          (TypeMacroTypedef
                            NamePair {
                              nameC = Name "A",
                              nameHsIdent = HsIdentifier "A"}
                            NameOriginInSource)),
                      structFieldOffset = 192,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "reparse.h:234:19",
                        fieldName = NamePair {
                          nameC = Name "const_field6",
                          nameHsIdent = HsIdentifier
                            "example_struct_with_const_const_field6"},
                        fieldComment = Nothing},
                      structFieldType = TypeConst
                        (TypePointer
                          (TypeConst
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "A",
                                nameHsIdent = HsIdentifier "A"}
                              NameOriginInSource))),
                      structFieldOffset = 256,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "reparse.h:235:19",
                        fieldName = NamePair {
                          nameC = Name "const_field7",
                          nameHsIdent = HsIdentifier
                            "example_struct_with_const_const_field7"},
                        fieldComment = Nothing},
                      structFieldType = TypeConst
                        (TypePointer
                          (TypeConst
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "A",
                                nameHsIdent = HsIdentifier "A"}
                              NameOriginInSource))),
                      structFieldOffset = 320,
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
              commentOrigin = Just
                "example_struct_with_const",
              commentLocation = Just
                "reparse.h:228:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["reparse.h"],
                  headerInclude = "reparse.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 48,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Example_struct_with_const",
                  structConstr = HsName
                    "@NsConstr"
                    "Example_struct_with_const",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field1",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "A"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:229:19",
                            fieldName = NamePair {
                              nameC = Name "const_field1",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field1"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "A",
                                nameHsIdent = HsIdentifier "A"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field1",
                          commentLocation = Just
                            "reparse.h:229:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field2",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "A"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:230:19",
                            fieldName = NamePair {
                              nameC = Name "const_field2",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field2"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "A",
                                nameHsIdent = HsIdentifier "A"}
                              NameOriginInSource),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field2",
                          commentLocation = Just
                            "reparse.h:230:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field3",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:231:19",
                            fieldName = NamePair {
                              nameC = Name "const_field3",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field3"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeConst
                              (TypeMacroTypedef
                                NamePair {
                                  nameC = Name "A",
                                  nameHsIdent = HsIdentifier "A"}
                                NameOriginInSource)),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field3",
                          commentLocation = Just
                            "reparse.h:231:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field4",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:232:19",
                            fieldName = NamePair {
                              nameC = Name "const_field4",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field4"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeConst
                              (TypeMacroTypedef
                                NamePair {
                                  nameC = Name "A",
                                  nameHsIdent = HsIdentifier "A"}
                                NameOriginInSource)),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field4",
                          commentLocation = Just
                            "reparse.h:232:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field5",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:233:19",
                            fieldName = NamePair {
                              nameC = Name "const_field5",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field5"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypePointer
                              (TypeMacroTypedef
                                NamePair {
                                  nameC = Name "A",
                                  nameHsIdent = HsIdentifier "A"}
                                NameOriginInSource)),
                          structFieldOffset = 192,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field5",
                          commentLocation = Just
                            "reparse.h:233:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field6",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:234:19",
                            fieldName = NamePair {
                              nameC = Name "const_field6",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field6"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypePointer
                              (TypeConst
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "A",
                                    nameHsIdent = HsIdentifier "A"}
                                  NameOriginInSource))),
                          structFieldOffset = 256,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field6",
                          commentLocation = Just
                            "reparse.h:234:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field7",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:235:19",
                            fieldName = NamePair {
                              nameC = Name "const_field7",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field7"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypePointer
                              (TypeConst
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "A",
                                    nameHsIdent = HsIdentifier "A"}
                                  NameOriginInSource))),
                          structFieldOffset = 320,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field7",
                          commentLocation = Just
                            "reparse.h:235:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "reparse.h:228:8",
                        declId = NamePair {
                          nameC = Name
                            "example_struct_with_const",
                          nameHsIdent = HsIdentifier
                            "Example_struct_with_const"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["reparse.h"],
                            headerInclude = "reparse.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Example_struct_with_const"),
                          structSizeof = 48,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:229:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field1",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field1"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "A",
                                    nameHsIdent = HsIdentifier "A"}
                                  NameOriginInSource),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:230:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field2",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field2"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "A",
                                    nameHsIdent = HsIdentifier "A"}
                                  NameOriginInSource),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:231:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field3",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field3"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeConst
                                  (TypeMacroTypedef
                                    NamePair {
                                      nameC = Name "A",
                                      nameHsIdent = HsIdentifier "A"}
                                    NameOriginInSource)),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:232:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field4",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field4"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeConst
                                  (TypeMacroTypedef
                                    NamePair {
                                      nameC = Name "A",
                                      nameHsIdent = HsIdentifier "A"}
                                    NameOriginInSource)),
                              structFieldOffset = 128,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:233:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field5",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field5"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypePointer
                                  (TypeMacroTypedef
                                    NamePair {
                                      nameC = Name "A",
                                      nameHsIdent = HsIdentifier "A"}
                                    NameOriginInSource)),
                              structFieldOffset = 192,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:234:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field6",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field6"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypePointer
                                  (TypeConst
                                    (TypeMacroTypedef
                                      NamePair {
                                        nameC = Name "A",
                                        nameHsIdent = HsIdentifier "A"}
                                      NameOriginInSource))),
                              structFieldOffset = 256,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:235:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field7",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field7"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypePointer
                                  (TypeConst
                                    (TypeMacroTypedef
                                      NamePair {
                                        nameC = Name "A",
                                        nameHsIdent = HsIdentifier "A"}
                                      NameOriginInSource))),
                              structFieldOffset = 320,
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
                      commentOrigin = Just
                        "example_struct_with_const",
                      commentLocation = Just
                        "reparse.h:228:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["reparse.h"],
                          headerInclude = "reparse.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4,
                PeekByteOff (Idx 0) 8,
                PeekByteOff (Idx 0) 16,
                PeekByteOff (Idx 0) 24,
                PeekByteOff (Idx 0) 32,
                PeekByteOff (Idx 0) 40]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Example_struct_with_const",
                  structConstr = HsName
                    "@NsConstr"
                    "Example_struct_with_const",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field1",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "A"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:229:19",
                            fieldName = NamePair {
                              nameC = Name "const_field1",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field1"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "A",
                                nameHsIdent = HsIdentifier "A"}
                              NameOriginInSource),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field1",
                          commentLocation = Just
                            "reparse.h:229:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field2",
                      fieldType = HsTypRef
                        (HsName "@NsTypeConstr" "A"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:230:19",
                            fieldName = NamePair {
                              nameC = Name "const_field2",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field2"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypeMacroTypedef
                              NamePair {
                                nameC = Name "A",
                                nameHsIdent = HsIdentifier "A"}
                              NameOriginInSource),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field2",
                          commentLocation = Just
                            "reparse.h:230:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field3",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:231:19",
                            fieldName = NamePair {
                              nameC = Name "const_field3",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field3"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeConst
                              (TypeMacroTypedef
                                NamePair {
                                  nameC = Name "A",
                                  nameHsIdent = HsIdentifier "A"}
                                NameOriginInSource)),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field3",
                          commentLocation = Just
                            "reparse.h:231:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field4",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:232:19",
                            fieldName = NamePair {
                              nameC = Name "const_field4",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field4"},
                            fieldComment = Nothing},
                          structFieldType = TypePointer
                            (TypeConst
                              (TypeMacroTypedef
                                NamePair {
                                  nameC = Name "A",
                                  nameHsIdent = HsIdentifier "A"}
                                NameOriginInSource)),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field4",
                          commentLocation = Just
                            "reparse.h:232:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field5",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:233:19",
                            fieldName = NamePair {
                              nameC = Name "const_field5",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field5"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypePointer
                              (TypeMacroTypedef
                                NamePair {
                                  nameC = Name "A",
                                  nameHsIdent = HsIdentifier "A"}
                                NameOriginInSource)),
                          structFieldOffset = 192,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field5",
                          commentLocation = Just
                            "reparse.h:233:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field6",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:234:19",
                            fieldName = NamePair {
                              nameC = Name "const_field6",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field6"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypePointer
                              (TypeConst
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "A",
                                    nameHsIdent = HsIdentifier "A"}
                                  NameOriginInSource))),
                          structFieldOffset = 256,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field6",
                          commentLocation = Just
                            "reparse.h:234:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_struct_with_const_const_field7",
                      fieldType = HsPtr
                        (HsTypRef
                          (HsName "@NsTypeConstr" "A")),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "reparse.h:235:19",
                            fieldName = NamePair {
                              nameC = Name "const_field7",
                              nameHsIdent = HsIdentifier
                                "example_struct_with_const_const_field7"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypePointer
                              (TypeConst
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "A",
                                    nameHsIdent = HsIdentifier "A"}
                                  NameOriginInSource))),
                          structFieldOffset = 320,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "const_field7",
                          commentLocation = Just
                            "reparse.h:235:19",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["reparse.h"],
                              headerInclude = "reparse.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "reparse.h:228:8",
                        declId = NamePair {
                          nameC = Name
                            "example_struct_with_const",
                          nameHsIdent = HsIdentifier
                            "Example_struct_with_const"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["reparse.h"],
                            headerInclude = "reparse.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Example_struct_with_const"),
                          structSizeof = 48,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:229:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field1",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field1"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "A",
                                    nameHsIdent = HsIdentifier "A"}
                                  NameOriginInSource),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:230:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field2",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field2"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypeMacroTypedef
                                  NamePair {
                                    nameC = Name "A",
                                    nameHsIdent = HsIdentifier "A"}
                                  NameOriginInSource),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:231:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field3",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field3"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeConst
                                  (TypeMacroTypedef
                                    NamePair {
                                      nameC = Name "A",
                                      nameHsIdent = HsIdentifier "A"}
                                    NameOriginInSource)),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:232:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field4",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field4"},
                                fieldComment = Nothing},
                              structFieldType = TypePointer
                                (TypeConst
                                  (TypeMacroTypedef
                                    NamePair {
                                      nameC = Name "A",
                                      nameHsIdent = HsIdentifier "A"}
                                    NameOriginInSource)),
                              structFieldOffset = 128,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:233:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field5",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field5"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypePointer
                                  (TypeMacroTypedef
                                    NamePair {
                                      nameC = Name "A",
                                      nameHsIdent = HsIdentifier "A"}
                                    NameOriginInSource)),
                              structFieldOffset = 192,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:234:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field6",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field6"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypePointer
                                  (TypeConst
                                    (TypeMacroTypedef
                                      NamePair {
                                        nameC = Name "A",
                                        nameHsIdent = HsIdentifier "A"}
                                      NameOriginInSource))),
                              structFieldOffset = 256,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "reparse.h:235:19",
                                fieldName = NamePair {
                                  nameC = Name "const_field7",
                                  nameHsIdent = HsIdentifier
                                    "example_struct_with_const_const_field7"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypePointer
                                  (TypeConst
                                    (TypeMacroTypedef
                                      NamePair {
                                        nameC = Name "A",
                                        nameHsIdent = HsIdentifier "A"}
                                      NameOriginInSource))),
                              structFieldOffset = 320,
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
                      commentOrigin = Just
                        "example_struct_with_const",
                      commentLocation = Just
                        "reparse.h:228:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["reparse.h"],
                          headerInclude = "reparse.h"},
                      commentChildren = []}}
                (Add 7)
                (Seq
                  [
                    PokeByteOff (Idx 8) 0 (Idx 0),
                    PokeByteOff (Idx 8) 4 (Idx 1),
                    PokeByteOff (Idx 8) 8 (Idx 2),
                    PokeByteOff (Idx 8) 16 (Idx 3),
                    PokeByteOff (Idx 8) 24 (Idx 4),
                    PokeByteOff (Idx 8) 32 (Idx 5),
                    PokeByteOff
                      (Idx 8)
                      40
                      (Idx 6)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Example_struct_with_const",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Example_struct_with_const",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_funptr1",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_funptr1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_funptr1",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "A"))))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:238:27",
          declId = NamePair {
            nameC = Name "const_funptr1",
            nameHsIdent = HsIdentifier
              "Const_funptr1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_funptr1",
              newtypeField = HsName
                "@NsVar"
                "un_Const_funptr1"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypeConst
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_funptr1",
          commentLocation = Just
            "reparse.h:238:27",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_funptr2",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_funptr2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_funptr2",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "A"))))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:239:27",
          declId = NamePair {
            nameC = Name "const_funptr2",
            nameHsIdent = HsIdentifier
              "Const_funptr2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_funptr2",
              newtypeField = HsName
                "@NsVar"
                "un_Const_funptr2"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypeConst
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_funptr2",
          commentLocation = Just
            "reparse.h:239:27",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_funptr3",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_funptr3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_funptr3",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsPtr
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "A")))))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:240:27",
          declId = NamePair {
            nameC = Name "const_funptr3",
            nameHsIdent = HsIdentifier
              "Const_funptr3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_funptr3",
              newtypeField = HsName
                "@NsVar"
                "un_Const_funptr3"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypePointer
                  (TypeConst
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_funptr3",
          commentLocation = Just
            "reparse.h:240:27",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr3",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_funptr4",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_funptr4",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_funptr4",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsPtr
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "A")))))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:241:27",
          declId = NamePair {
            nameC = Name "const_funptr4",
            nameHsIdent = HsIdentifier
              "Const_funptr4"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_funptr4",
              newtypeField = HsName
                "@NsVar"
                "un_Const_funptr4"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypePointer
                  (TypeConst
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_funptr4",
          commentLocation = Just
            "reparse.h:241:27",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr4",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr4",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_funptr5",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_funptr5",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_funptr5",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsPtr
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "A")))))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:242:27",
          declId = NamePair {
            nameC = Name "const_funptr5",
            nameHsIdent = HsIdentifier
              "Const_funptr5"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_funptr5",
              newtypeField = HsName
                "@NsVar"
                "un_Const_funptr5"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypeConst
                  (TypePointer
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_funptr5",
          commentLocation = Just
            "reparse.h:242:27",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr5",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr5",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_funptr6",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_funptr6",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_funptr6",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsPtr
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "A")))))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:243:27",
          declId = NamePair {
            nameC = Name "const_funptr6",
            nameHsIdent = HsIdentifier
              "Const_funptr6"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_funptr6",
              newtypeField = HsName
                "@NsVar"
                "un_Const_funptr6"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypeConst
                  (TypePointer
                    (TypeConst
                      (TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource)))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_funptr6",
          commentLocation = Just
            "reparse.h:243:27",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr6",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr6",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr6",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr6",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Const_funptr7",
      newtypeConstr = HsName
        "@NsConstr"
        "Const_funptr7",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Const_funptr7",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsPtr
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "A")))))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:244:27",
          declId = NamePair {
            nameC = Name "const_funptr7",
            nameHsIdent = HsIdentifier
              "Const_funptr7"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Const_funptr7",
              newtypeField = HsName
                "@NsVar"
                "un_Const_funptr7"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed),
                  TypePrim
                    (PrimFloating PrimDouble)]
                (TypeConst
                  (TypePointer
                    (TypeConst
                      (TypeMacroTypedef
                        NamePair {
                          nameC = Name "A",
                          nameHsIdent = HsIdentifier "A"}
                        NameOriginInSource)))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_funptr7",
          commentLocation = Just
            "reparse.h:244:27",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr7",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr7",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr7",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Const_funptr7",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_array_elem1_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "A")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_02f253d2b51e801b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_02f253d2b51e801b (A const *arg1) { const_array_elem1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeIncompleteArray
                (TypeConst
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_array_elem1",
          commentLocation = Just
            "reparse.h:246:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_74accfb2c8dbad9b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsIncompleteArray
                (HsTypRef
                  (HsName "@NsTypeConstr" "A")))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_74accfb2c8dbad9b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_array_elem1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_74accfb2c8dbad9b (void)) (A const arg1[]) { return &const_array_elem1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeIncompleteArray
              (TypeConst
                (TypeMacroTypedef
                  NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"}
                  NameOriginInSource))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_array_elem1",
          commentLocation = Just
            "reparse.h:246:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_array_elem2_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsPtr
            (HsPtr
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ae594e2b9ac5b1c3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_ae594e2b9ac5b1c3 (A const **arg1) { const_array_elem2(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeIncompleteArray
                (TypePointer
                  (TypeConst
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_array_elem2",
          commentLocation = Just
            "reparse.h:247:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_e545f64064b17b40",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsIncompleteArray
                (HsPtr
                  (HsTypRef
                    (HsName "@NsTypeConstr" "A"))))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e545f64064b17b40",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_array_elem2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e545f64064b17b40 (void)) (A const *arg1[]) { return &const_array_elem2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeIncompleteArray
              (TypePointer
                (TypeConst
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_array_elem2",
          commentLocation = Just
            "reparse.h:247:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "const_array_elem3_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsPtr
            (HsPtr
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_2149e2b9eadf2ded",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_2149e2b9eadf2ded (A *const *arg1) { const_array_elem3(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeIncompleteArray
                (TypeConst
                  (TypePointer
                    (TypeMacroTypedef
                      NamePair {
                        nameC = Name "A",
                        nameHsIdent = HsIdentifier "A"}
                      NameOriginInSource))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_array_elem3",
          commentLocation = Just
            "reparse.h:248:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_9467829a46b5dfc3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsIncompleteArray
                (HsPtr
                  (HsTypRef
                    (HsName "@NsTypeConstr" "A"))))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_9467829a46b5dfc3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_const_array_elem3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_9467829a46b5dfc3 (void)) (A *const arg1[]) { return &const_array_elem3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeIncompleteArray
              (TypeConst
                (TypePointer
                  (TypeMacroTypedef
                    NamePair {
                      nameC = Name "A",
                      nameHsIdent = HsIdentifier "A"}
                    NameOriginInSource)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "const_array_elem3",
          commentLocation = Just
            "reparse.h:248:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "noParams1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName "@NsTypeConstr" "A"))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_14dbb45095c3c138",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "A hs_bindgen_test_reparse_14dbb45095c3c138 (void) { return noParams1(); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeMacroTypedef
            NamePair {
              nameC = Name "A",
              nameHsIdent = HsIdentifier "A"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Other examples we reparsed /incorrectly/ before language-c"],
          commentOrigin = Just
            "noParams1",
          commentLocation = Just
            "reparse.h:256:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_af69ae434bc46f7f",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "A"))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_af69ae434bc46f7f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_noParams1_ptr */ __attribute__ ((const)) A (*hs_bindgen_test_reparse_af69ae434bc46f7f (void)) (void) { return &noParams1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypeMacroTypedef
            NamePair {
              nameC = Name "A",
              nameHsIdent = HsIdentifier "A"}
            NameOriginInSource)),
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Other examples we reparsed /incorrectly/ before language-c"],
          commentOrigin = Just
            "noParams1",
          commentLocation = Just
            "reparse.h:256:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "noParams2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName "@NsTypeConstr" "A"))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_667f19a2bcb7ada6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "A hs_bindgen_test_reparse_667f19a2bcb7ada6 (void) { return noParams2(); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeMacroTypedef
            NamePair {
              nameC = Name "A",
              nameHsIdent = HsIdentifier "A"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "noParams2",
          commentLocation = Just
            "reparse.h:257:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_2f061e057ad050d2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "A"))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_2f061e057ad050d2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_noParams2_ptr */ __attribute__ ((const)) A (*hs_bindgen_test_reparse_2f061e057ad050d2 (void)) (void) { return &noParams2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypeMacroTypedef
            NamePair {
              nameC = Name "A",
              nameHsIdent = HsIdentifier "A"}
            NameOriginInSource)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "noParams2",
          commentLocation = Just
            "reparse.h:257:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "noParams3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg2"),
          functionParameterType = HsFunPtr
            (HsIO (HsPrimType HsPrimCInt)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg2",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_34332de6ec849f2a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_reparse_34332de6ec849f2a (A arg1, signed int (*arg2) (void)) { noParams3(arg1, arg2); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "arg2",
                  nameHsIdent = HsIdentifier
                    "arg2"})
              (TypePointer
                (TypeFun
                  []
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "noParams3",
          commentLocation = Just
            "reparse.h:258:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_a179b59c001d5529",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsFun
                (HsFunPtr
                  (HsIO (HsPrimType HsPrimCInt)))
                (HsIO
                  (HsPrimType HsPrimUnit)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_a179b59c001d5529",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_noParams3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a179b59c001d5529 (void)) (A arg1, signed int (*arg2) (void)) { return &noParams3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource,
            TypePointer
              (TypeFun
                []
                (TypePrim
                  (PrimIntegral PrimInt Signed)))]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "noParams3",
          commentLocation = Just
            "reparse.h:258:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_ret1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_7c42b6acbe3f8845",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void (*hs_bindgen_test_reparse_7c42b6acbe3f8845 (A arg1)) (void) { return funptr_ret1(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun [] TypeVoid)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret1",
          commentLocation = Just
            "reparse.h:262:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_ae380911ef8bd65a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsFunPtr
                  (HsIO
                    (HsPrimType HsPrimUnit))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ae380911ef8bd65a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_ret1_ptr */ __attribute__ ((const)) void (*(*hs_bindgen_test_reparse_ae380911ef8bd65a (void)) (A arg1)) (void) { return &funptr_ret1; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeFun [] TypeVoid))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret1",
          commentLocation = Just
            "reparse.h:262:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_ret2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_de4479feaa4c1b63",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int (*hs_bindgen_test_reparse_de4479feaa4c1b63 (A arg1)) (void) { return funptr_ret2(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              []
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret2",
          commentLocation = Just
            "reparse.h:263:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_0e60d042fc021fa0",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsFunPtr
                  (HsIO
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_0e60d042fc021fa0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_ret2_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_reparse_0e60d042fc021fa0 (void)) (A arg1)) (void) { return &funptr_ret2; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeFun
              []
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret2",
          commentLocation = Just
            "reparse.h:263:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_ret3",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_8ca1c491149e7a82",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void (*hs_bindgen_test_reparse_8ca1c491149e7a82 (A arg1)) (signed int arg1) { return funptr_ret3(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed)]
              TypeVoid)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret3",
          commentLocation = Just
            "reparse.h:264:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_ee8815fca40a6e75",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsIO
                      (HsPrimType HsPrimUnit)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ee8815fca40a6e75",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_ret3_ptr */ __attribute__ ((const)) void (*(*hs_bindgen_test_reparse_ee8815fca40a6e75 (void)) (A arg1)) (signed int arg1) { return &funptr_ret3; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed)]
              TypeVoid))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret3",
          commentLocation = Just
            "reparse.h:264:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_ret4",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPrimType HsPrimCChar)))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_28a86a376fb47aef",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char (*hs_bindgen_test_reparse_28a86a376fb47aef (A arg1)) (signed int arg1, double arg2) { return funptr_ret4(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypePrim
                (PrimChar
                  (PrimSignImplicit Nothing))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret4",
          commentLocation = Just
            "reparse.h:265:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_5d6841bf202a780e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsFun
                      (HsPrimType HsPrimCDouble)
                      (HsIO
                        (HsPrimType
                          HsPrimCChar))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_5d6841bf202a780e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_ret4_ptr */ __attribute__ ((const)) char (*(*hs_bindgen_test_reparse_5d6841bf202a780e (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret4; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypePrim
                (PrimChar
                  (PrimSignImplicit Nothing)))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret4",
          commentLocation = Just
            "reparse.h:265:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_ret5",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_677dad7824f8f926",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int *(*hs_bindgen_test_reparse_677dad7824f8f926 (A arg1)) (signed int arg1, double arg2) { return funptr_ret5(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypePointer
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret5",
          commentLocation = Just
            "reparse.h:269:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_43e62fcd431c5993",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsFun
                      (HsPrimType HsPrimCDouble)
                      (HsIO
                        (HsPtr
                          (HsPrimType
                            HsPrimCInt)))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_43e62fcd431c5993",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_ret5_ptr */ __attribute__ ((const)) signed int *(*(*hs_bindgen_test_reparse_43e62fcd431c5993 (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret5; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypePointer
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret5",
          commentLocation = Just
            "reparse.h:269:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_ret6",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_32eeedbdca340214",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int const *(*hs_bindgen_test_reparse_32eeedbdca340214 (A arg1)) (signed int arg1, double arg2) { return funptr_ret6(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret6",
          commentLocation = Just
            "reparse.h:270:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_ae01ac1f8293cbcd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsFun
                      (HsPrimType HsPrimCDouble)
                      (HsIO
                        (HsPtr
                          (HsPrimType
                            HsPrimCInt)))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_ae01ac1f8293cbcd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_ret6_ptr */ __attribute__ ((const)) signed int const *(*(*hs_bindgen_test_reparse_ae01ac1f8293cbcd (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret6; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret6",
          commentLocation = Just
            "reparse.h:270:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_ret7",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_6c9848f47db6c013",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int const *(*hs_bindgen_test_reparse_6c9848f47db6c013 (A arg1)) (signed int arg1, double arg2) { return funptr_ret7(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret7",
          commentLocation = Just
            "reparse.h:271:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_f6edab58ac911c39",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsFun
                      (HsPrimType HsPrimCDouble)
                      (HsIO
                        (HsPtr
                          (HsPrimType
                            HsPrimCInt)))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_f6edab58ac911c39",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_ret7_ptr */ __attribute__ ((const)) signed int const *(*(*hs_bindgen_test_reparse_f6edab58ac911c39 (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret7; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret7",
          commentLocation = Just
            "reparse.h:271:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_ret8",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_685ed1da05a496ec",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int *const (*hs_bindgen_test_reparse_685ed1da05a496ec (A arg1)) (signed int arg1, double arg2) { return funptr_ret8(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypeConst
                (TypePointer
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret8",
          commentLocation = Just
            "reparse.h:272:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_e16f7f0071bb9009",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsFun
                      (HsPrimType HsPrimCDouble)
                      (HsIO
                        (HsPtr
                          (HsPrimType
                            HsPrimCInt)))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e16f7f0071bb9009",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_ret8_ptr */ __attribute__ ((const)) signed int *const (*(*hs_bindgen_test_reparse_e16f7f0071bb9009 (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret8; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypeConst
                (TypePointer
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret8",
          commentLocation = Just
            "reparse.h:272:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_ret9",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d838f03e8f8b0e9a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int const *const (*hs_bindgen_test_reparse_d838f03e8f8b0e9a (A arg1)) (signed int arg1, double arg2) { return funptr_ret9(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypeConst
                (TypePointer
                  (TypeConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret9",
          commentLocation = Just
            "reparse.h:273:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_d7e96ea609f2a0d5",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsFun
                      (HsPrimType HsPrimCDouble)
                      (HsIO
                        (HsPtr
                          (HsPrimType
                            HsPrimCInt)))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_d7e96ea609f2a0d5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_ret9_ptr */ __attribute__ ((const)) signed int const *const (*(*hs_bindgen_test_reparse_d7e96ea609f2a0d5 (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret9; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypeConst
                (TypePointer
                  (TypeConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret9",
          commentLocation = Just
            "reparse.h:273:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "funptr_ret10",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg1"),
          functionParameterType = HsTypRef
            (HsName "@NsTypeConstr" "A"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg1",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCInt))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_e44ba9f0163468d8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int const *const (*hs_bindgen_test_reparse_e44ba9f0163468d8 (A arg1)) (signed int arg1, double arg2) { return funptr_ret10(arg1); }",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg1",
                  nameHsIdent = HsIdentifier
                    "arg1"})
              (TypeMacroTypedef
                NamePair {
                  nameC = Name "A",
                  nameHsIdent = HsIdentifier "A"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypeConst
                (TypePointer
                  (TypeConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret10",
          commentLocation = Just
            "reparse.h:274:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_reparse_163513e7413372dd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "A"))
              (HsIO
                (HsFunPtr
                  (HsFun
                    (HsPrimType HsPrimCInt)
                    (HsFun
                      (HsPrimType HsPrimCDouble)
                      (HsIO
                        (HsPtr
                          (HsPrimType
                            HsPrimCInt)))))))))),
      foreignImportOrigName =
      "hs_bindgen_test_reparse_163513e7413372dd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_funptr_ret10_ptr */ __attribute__ ((const)) signed int const *const (*(*hs_bindgen_test_reparse_163513e7413372dd (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret10; } ",
          capiWrapperImport =
          "reparse.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeMacroTypedef
              NamePair {
                nameC = Name "A",
                nameHsIdent = HsIdentifier "A"}
              NameOriginInSource]
          (TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed),
                TypePrim
                  (PrimFloating PrimDouble)]
              (TypeConst
                (TypePointer
                  (TypeConst
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "funptr_ret10",
          commentLocation = Just
            "reparse.h:274:20",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "BOOL",
      newtypeConstr = HsName
        "@NsConstr"
        "BOOL",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_BOOL",
        fieldType = HsPrimType
          HsPrimCBool,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:280:9",
          declId = NamePair {
            nameC = Name "BOOL",
            nameHsIdent = HsIdentifier
              "BOOL"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "BOOL",
              newtypeField = HsName
                "@NsVar"
                "un_BOOL"},
            macroType = TypePrim PrimBool},
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
          commentOrigin = Just "BOOL",
          commentLocation = Just
            "reparse.h:280:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
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
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "BOOL",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "INT",
      newtypeConstr = HsName
        "@NsConstr"
        "INT",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_INT",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:281:9",
          declId = NamePair {
            nameC = Name "INT",
            nameHsIdent = HsIdentifier
              "INT"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "INT",
              newtypeField = HsName
                "@NsVar"
                "un_INT"},
            macroType = TypePrim
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
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "INT",
          commentLocation = Just
            "reparse.h:281:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
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
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INT",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "INTP",
      newtypeConstr = HsName
        "@NsConstr"
        "INTP",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_INTP",
        fieldType = HsPtr
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:282:9",
          declId = NamePair {
            nameC = Name "INTP",
            nameHsIdent = HsIdentifier
              "INTP"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "INTP",
              newtypeField = HsName
                "@NsVar"
                "un_INTP"},
            macroType = TypePointer
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "INTP",
          commentLocation = Just
            "reparse.h:282:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INTP",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INTP",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INTP",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INTP",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "INTCP",
      newtypeConstr = HsName
        "@NsConstr"
        "INTCP",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_INTCP",
        fieldType = HsPtr
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "reparse.h:283:9",
          declId = NamePair {
            nameC = Name "INTCP",
            nameHsIdent = HsIdentifier
              "INTCP"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "INTCP",
              newtypeField = HsName
                "@NsVar"
                "un_INTCP"},
            macroType = TypeConst
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "INTCP",
          commentLocation = Just
            "reparse.h:283:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["reparse.h"],
              headerInclude = "reparse.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INTCP",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INTCP",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INTCP",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "INTCP",
      deriveInstanceComment =
      Nothing}]
