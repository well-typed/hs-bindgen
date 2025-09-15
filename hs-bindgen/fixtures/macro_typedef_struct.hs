[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      newtypeConstr = HsName
        "@NsConstr"
        "MY_TYPE",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_MY_TYPE",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_typedef_struct.h:1:9",
          declId = NamePair {
            nameC = Name "MY_TYPE",
            nameHsIdent = HsIdentifier
              "MY_TYPE"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_typedef_struct.h"],
              headerInclude =
              "macro_typedef_struct.h"},
          declComment = Nothing},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "MY_TYPE",
              newtypeField = HsName
                "@NsVar"
                "un_MY_TYPE"},
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
          commentOrigin = Just "MY_TYPE",
          commentLocation = Just
            "macro_typedef_struct.h:1:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_typedef_struct.h"],
              headerInclude =
              "macro_typedef_struct.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
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
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "MY_TYPE",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Bar",
      structConstr = HsName
        "@NsConstr"
        "Bar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "macro_typedef_struct.h:4:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "bar_x"},
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
                "macro_typedef_struct.h:4:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["macro_typedef_struct.h"],
                  headerInclude =
                  "macro_typedef_struct.h"},
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_y",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "MY_TYPE"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "macro_typedef_struct.h:5:11",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "bar_y"},
                fieldComment = Nothing},
              structFieldType =
              TypeMacroTypedef
                NamePair {
                  nameC = Name "MY_TYPE",
                  nameHsIdent = HsIdentifier
                    "MY_TYPE"}
                NameOriginInSource,
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "macro_typedef_struct.h:5:11",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["macro_typedef_struct.h"],
                  headerInclude =
                  "macro_typedef_struct.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "macro_typedef_struct.h:3:9",
            declId = NamePair {
              nameC = Name "bar",
              nameHsIdent = HsIdentifier
                "Bar"},
            declOrigin = NameOriginGenerated
              (AnonId
                "macro_typedef_struct.h:3:9"),
            declAliases = [Name "bar"],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["macro_typedef_struct.h"],
                headerInclude =
                "macro_typedef_struct.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Bar"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_typedef_struct.h:4:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "bar_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_typedef_struct.h:5:11",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "bar_y"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "MY_TYPE",
                      nameHsIdent = HsIdentifier
                        "MY_TYPE"}
                    NameOriginInSource,
                  structFieldOffset = 32,
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
          commentOrigin = Just "bar",
          commentLocation = Just
            "macro_typedef_struct.h:3:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["macro_typedef_struct.h"],
              headerInclude =
              "macro_typedef_struct.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Bar",
          structConstr = HsName
            "@NsConstr"
            "Bar",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "bar_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_typedef_struct.h:4:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "bar_x"},
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
                    "macro_typedef_struct.h:4:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["macro_typedef_struct.h"],
                      headerInclude =
                      "macro_typedef_struct.h"},
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "bar_y",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "MY_TYPE"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "macro_typedef_struct.h:5:11",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "bar_y"},
                    fieldComment = Nothing},
                  structFieldType =
                  TypeMacroTypedef
                    NamePair {
                      nameC = Name "MY_TYPE",
                      nameHsIdent = HsIdentifier
                        "MY_TYPE"}
                    NameOriginInSource,
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "macro_typedef_struct.h:5:11",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["macro_typedef_struct.h"],
                      headerInclude =
                      "macro_typedef_struct.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "macro_typedef_struct.h:3:9",
                declId = NamePair {
                  nameC = Name "bar",
                  nameHsIdent = HsIdentifier
                    "Bar"},
                declOrigin = NameOriginGenerated
                  (AnonId
                    "macro_typedef_struct.h:3:9"),
                declAliases = [Name "bar"],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["macro_typedef_struct.h"],
                    headerInclude =
                    "macro_typedef_struct.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Bar"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "macro_typedef_struct.h:4:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "bar_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "macro_typedef_struct.h:5:11",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "bar_y"},
                        fieldComment = Nothing},
                      structFieldType =
                      TypeMacroTypedef
                        NamePair {
                          nameC = Name "MY_TYPE",
                          nameHsIdent = HsIdentifier
                            "MY_TYPE"}
                        NameOriginInSource,
                      structFieldOffset = 32,
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
              commentOrigin = Just "bar",
              commentLocation = Just
                "macro_typedef_struct.h:3:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["macro_typedef_struct.h"],
                  headerInclude =
                  "macro_typedef_struct.h"},
              commentChildren = []}}
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
                    "Bar",
                  structConstr = HsName
                    "@NsConstr"
                    "Bar",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bar_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_typedef_struct.h:4:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "bar_x"},
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
                            "macro_typedef_struct.h:4:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["macro_typedef_struct.h"],
                              headerInclude =
                              "macro_typedef_struct.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bar_y",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "MY_TYPE"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_typedef_struct.h:5:11",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "bar_y"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "MY_TYPE",
                              nameHsIdent = HsIdentifier
                                "MY_TYPE"}
                            NameOriginInSource,
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "macro_typedef_struct.h:5:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["macro_typedef_struct.h"],
                              headerInclude =
                              "macro_typedef_struct.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "macro_typedef_struct.h:3:9",
                        declId = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = HsIdentifier
                            "Bar"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "macro_typedef_struct.h:3:9"),
                        declAliases = [Name "bar"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["macro_typedef_struct.h"],
                            headerInclude =
                            "macro_typedef_struct.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Bar"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_typedef_struct.h:4:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "bar_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_typedef_struct.h:5:11",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "bar_y"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "MY_TYPE",
                                  nameHsIdent = HsIdentifier
                                    "MY_TYPE"}
                                NameOriginInSource,
                              structFieldOffset = 32,
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
                      commentOrigin = Just "bar",
                      commentLocation = Just
                        "macro_typedef_struct.h:3:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["macro_typedef_struct.h"],
                          headerInclude =
                          "macro_typedef_struct.h"},
                      commentChildren = []}})
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
                    "Bar",
                  structConstr = HsName
                    "@NsConstr"
                    "Bar",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bar_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_typedef_struct.h:4:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "bar_x"},
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
                            "macro_typedef_struct.h:4:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["macro_typedef_struct.h"],
                              headerInclude =
                              "macro_typedef_struct.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "bar_y",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "MY_TYPE"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "macro_typedef_struct.h:5:11",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "bar_y"},
                            fieldComment = Nothing},
                          structFieldType =
                          TypeMacroTypedef
                            NamePair {
                              nameC = Name "MY_TYPE",
                              nameHsIdent = HsIdentifier
                                "MY_TYPE"}
                            NameOriginInSource,
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "macro_typedef_struct.h:5:11",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["macro_typedef_struct.h"],
                              headerInclude =
                              "macro_typedef_struct.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "macro_typedef_struct.h:3:9",
                        declId = NamePair {
                          nameC = Name "bar",
                          nameHsIdent = HsIdentifier
                            "Bar"},
                        declOrigin = NameOriginGenerated
                          (AnonId
                            "macro_typedef_struct.h:3:9"),
                        declAliases = [Name "bar"],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["macro_typedef_struct.h"],
                            headerInclude =
                            "macro_typedef_struct.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Bar"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_typedef_struct.h:4:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "bar_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "macro_typedef_struct.h:5:11",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "bar_y"},
                                fieldComment = Nothing},
                              structFieldType =
                              TypeMacroTypedef
                                NamePair {
                                  nameC = Name "MY_TYPE",
                                  nameHsIdent = HsIdentifier
                                    "MY_TYPE"}
                                NameOriginInSource,
                              structFieldOffset = 32,
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
                      commentOrigin = Just "bar",
                      commentLocation = Just
                        "macro_typedef_struct.h:3:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["macro_typedef_struct.h"],
                          headerInclude =
                          "macro_typedef_struct.h"},
                      commentChildren = []}}
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
        "Bar",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Bar",
      deriveInstanceComment =
      Nothing}]
