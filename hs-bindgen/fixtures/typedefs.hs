[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Myint",
      newtypeConstr = HsName
        "@NsConstr"
        "Myint",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Myint",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:1:13",
          declId = NamePair {
            nameC = Name "myint",
            nameHsIdent = HsIdentifier
              "Myint"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Myint",
              newtypeField = HsName
                "@NsVar"
                "un_Myint"},
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
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "myint",
          commentLocation = Just
            "typedefs.h:1:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
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
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Myint",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Intptr",
      newtypeConstr = HsName
        "@NsConstr"
        "Intptr",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Intptr",
        fieldType = HsPtr
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:2:15",
          declId = NamePair {
            nameC = Name "intptr",
            nameHsIdent = HsIdentifier
              "Intptr"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Intptr",
              newtypeField = HsName
                "@NsVar"
                "un_Intptr"},
            typedefType = TypePointer
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
          commentOrigin = Just "intptr",
          commentLocation = Just
            "typedefs.h:2:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Intptr",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Intptr",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Intptr",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Intptr",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "FunctionPointer_Function_Deref",
      newtypeConstr = HsName
        "@NsConstr"
        "FunctionPointer_Function_Deref",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_FunctionPointer_Function_Deref",
        fieldType = HsIO
          (HsPrimType HsPrimUnit),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:4:16",
          declId = NamePair {
            nameC = Name
              "FunctionPointer_Function_Deref",
            nameHsIdent = HsIdentifier
              "FunctionPointer_Function_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "typedefs.h:4:16"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "FunctionPointer_Function_Deref",
              newtypeField = HsName
                "@NsVar"
                "un_FunctionPointer_Function_Deref"},
            typedefType = TypeFun
              []
              TypeVoid},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "FunctionPointer_Function",
      newtypeConstr = HsName
        "@NsConstr"
        "FunctionPointer_Function",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_FunctionPointer_Function",
        fieldType = HsFunPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "FunctionPointer_Function_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:4:16",
          declId = NamePair {
            nameC = Name
              "FunctionPointer_Function",
            nameHsIdent = HsIdentifier
              "FunctionPointer_Function"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "FunctionPointer_Function",
              newtypeField = HsName
                "@NsVar"
                "un_FunctionPointer_Function"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name
                      "FunctionPointer_Function_Deref",
                    nameHsIdent = HsIdentifier
                      "FunctionPointer_Function_Deref"}))},
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
            "FunctionPointer_Function",
          commentLocation = Just
            "typedefs.h:4:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "FunctionPointer_Function",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "FunctionPointer_Function",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "FunctionPointer_Function",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "FunctionPointer_Function",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "NonFunctionPointer_Function",
      newtypeConstr = HsName
        "@NsConstr"
        "NonFunctionPointer_Function",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_NonFunctionPointer_Function",
        fieldType = HsFun
          (HsPrimType HsPrimCInt)
          (HsIO (HsPrimType HsPrimCInt)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:5:14",
          declId = NamePair {
            nameC = Name
              "NonFunctionPointer_Function",
            nameHsIdent = HsIdentifier
              "NonFunctionPointer_Function"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "NonFunctionPointer_Function",
              newtypeField = HsName
                "@NsVar"
                "un_NonFunctionPointer_Function"},
            typedefType = TypeFun
              [
                TypePrim
                  (PrimIntegral PrimInt Signed)]
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "NonFunctionPointer_Function",
          commentLocation = Just
            "typedefs.h:5:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "F1_Deref",
      newtypeConstr = HsName
        "@NsConstr"
        "F1_Deref",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_F1_Deref",
        fieldType = HsIO
          (HsPrimType HsPrimUnit),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:7:16",
          declId = NamePair {
            nameC = Name "f1_Deref",
            nameHsIdent = HsIdentifier
              "F1_Deref"},
          declOrigin = NameOriginGenerated
            (AnonId "typedefs.h:7:16"),
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "F1_Deref",
              newtypeField = HsName
                "@NsVar"
                "un_F1_Deref"},
            typedefType = TypeFun
              []
              TypeVoid},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "F1",
      newtypeConstr = HsName
        "@NsConstr"
        "F1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_F1",
        fieldType = HsFunPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "F1_Deref")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:7:16",
          declId = NamePair {
            nameC = Name "f1",
            nameHsIdent = HsIdentifier
              "F1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "F1",
              newtypeField = HsName
                "@NsVar"
                "un_F1"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "f1_Deref",
                    nameHsIdent = HsIdentifier
                      "F1_Deref"}))},
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
          commentOrigin = Just "f1",
          commentLocation = Just
            "typedefs.h:7:16",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "F1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "G1",
      newtypeConstr = HsName
        "@NsConstr"
        "G1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_G1",
        fieldType = HsIO
          (HsPrimType HsPrimUnit),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:9:14",
          declId = NamePair {
            nameC = Name "g1",
            nameHsIdent = HsIdentifier
              "G1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "G1",
              newtypeField = HsName
                "@NsVar"
                "un_G1"},
            typedefType = TypeFun
              []
              TypeVoid},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "g1",
          commentLocation = Just
            "typedefs.h:9:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "G2",
      newtypeConstr = HsName
        "@NsConstr"
        "G2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_G2",
        fieldType = HsFunPtr
          (HsTypRef
            (HsName "@NsTypeConstr" "G1")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:10:14",
          declId = NamePair {
            nameC = Name "g2",
            nameHsIdent = HsIdentifier
              "G2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "G2",
              newtypeField = HsName
                "@NsVar"
                "un_G2"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "g1",
                    nameHsIdent = HsIdentifier
                      "G1"}))},
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
          commentOrigin = Just "g2",
          commentLocation = Just
            "typedefs.h:10:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "G2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "G2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "G2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "G2",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "H1",
      newtypeConstr = HsName
        "@NsConstr"
        "H1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_H1",
        fieldType = HsIO
          (HsPrimType HsPrimUnit),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:12:14",
          declId = NamePair {
            nameC = Name "h1",
            nameHsIdent = HsIdentifier
              "H1"},
          declOrigin = NameOriginInSource,
          declAliases = [Name "h2"],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "H1",
              newtypeField = HsName
                "@NsVar"
                "un_H1"},
            typedefType = TypeFun
              []
              TypeVoid},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "h1",
          commentLocation = Just
            "typedefs.h:12:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "H2",
      newtypeConstr = HsName
        "@NsConstr"
        "H2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_H2",
        fieldType = HsTypRef
          (HsName "@NsTypeConstr" "H1"),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:13:12",
          declId = NamePair {
            nameC = Name "h2",
            nameHsIdent = HsIdentifier
              "H2"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "H2",
              newtypeField = HsName
                "@NsVar"
                "un_H2"},
            typedefType = TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "h1",
                  nameHsIdent = HsIdentifier
                    "H1"})},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "h2",
          commentLocation = Just
            "typedefs.h:13:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "H3",
      newtypeConstr = HsName
        "@NsConstr"
        "H3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_H3",
        fieldType = HsFunPtr
          (HsTypRef
            (HsName "@NsTypeConstr" "H2")),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "typedefs.h:14:14",
          declId = NamePair {
            nameC = Name "h3",
            nameHsIdent = HsIdentifier
              "H3"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "H3",
              newtypeField = HsName
                "@NsVar"
                "un_H3"},
            typedefType = TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "h2",
                    nameHsIdent = HsIdentifier
                      "H2"}))},
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
          commentOrigin = Just "h3",
          commentLocation = Just
            "typedefs.h:14:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["typedefs.h"],
              headerInclude = "typedefs.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "H3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "H3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "H3",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "H3",
      deriveInstanceComment =
      Nothing}]
