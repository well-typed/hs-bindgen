[
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "fIELD_OFFSET",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        4
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "ePSILON",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon FloatLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (FloatLikeTyCon DoubleType))))
                []]}},
      varDeclBody = VarDeclDouble
        0.1},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "pTR_TO_FIELD",
      varDeclType = ForallTy {
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              (AClass
                (GenerativeTyCon
                  (ClassTyCon AddTyCon)))
              [
                TyVarTy (Idx 0),
                TyConAppTy
                  (ATyCon
                    (GenerativeTyCon
                      (DataTyCon IntLikeTyCon)))
                  [
                    TyConAppTy
                      (ATyCon
                        (GenerativeTyCon
                          (DataTyCon
                            (IntLikeTyCon
                              (CIntegralType
                                (IntLike (Int Signed)))))))
                      []]]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (TyConAppTy
              (ATyCon
                (FamilyTyCon AddResTyCon))
              [
                TyVarTy (Idx 0),
                TyConAppTy
                  (ATyCon
                    (GenerativeTyCon
                      (DataTyCon IntLikeTyCon)))
                  [
                    TyConAppTy
                      (ATyCon
                        (GenerativeTyCon
                          (DataTyCon
                            (IntLikeTyCon
                              (CIntegralType
                                (IntLike (Int Signed)))))))
                      []]])}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "ptr")
          (VarDeclApp
            (InfixAppHead MAdd)
            [
              VarDeclVar (Idx 0),
              VarDeclIntegral
                4
                HsPrimCInt]))},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "YEAR",
      newtypeConstr = HsName
        "@NsConstr"
        "YEAR",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_YEAR",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "manual_examples.h:53:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "YEAR",
          macroArgs = [],
          macroBody = MTerm
            (MType
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "YEAR"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "MONTH",
      newtypeConstr = HsName
        "@NsConstr"
        "MONTH",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_MONTH",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "manual_examples.h:54:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "MONTH",
          macroArgs = [],
          macroBody = MTerm
            (MType
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "MONTH"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "DAY",
      newtypeConstr = HsName
        "@NsConstr"
        "DAY",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_DAY",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "manual_examples.h:55:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "DAY",
          macroArgs = [],
          macroBody = MTerm
            (MType
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "DAY"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "DAY"),
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "mk_triple",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPrimType HsPrimCInt)
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsFun
              (HsPtr
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Triple")))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportCRes = TypeVoid,
      foreignImportCArgs = [
        TypePrim
          (PrimIntegral PrimInt Signed),
        TypePrim
          (PrimIntegral PrimInt Signed),
        TypePrim
          (PrimIntegral PrimInt Signed),
        TypePointer
          (TypeStruct
            (DeclPathName
              (CName "triple")
              DeclPathCtxtTop))],
      foreignImportOrigName =
      "mk_triple",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "mk_triple",
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePointer
              (TypeStruct
                (DeclPathName
                  (CName "triple")
                  DeclPathCtxtTop))],
          functionRes = TypeVoid,
          functionHeader =
          "manual_examples.h",
          functionSourceLoc =
          "manual_examples.h:20:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "index_triple",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Triple")))
        (HsFun
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Index"))
          (HsIO (HsPrimType HsPrimCInt))),
      foreignImportCRes = TypePrim
        (PrimIntegral PrimInt Signed),
      foreignImportCArgs = [
        TypePointer
          (TypeStruct
            (DeclPathName
              (CName "triple")
              DeclPathCtxtTop)),
        TypeEnum
          (DeclPathName
            (CName "index")
            DeclPathCtxtTop)],
      foreignImportOrigName =
      "index_triple",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "index_triple",
          functionArgs = [
            TypePointer
              (TypeStruct
                (DeclPathName
                  (CName "triple")
                  DeclPathCtxtTop)),
            TypeEnum
              (DeclPathName
                (CName "index")
                DeclPathCtxtTop)],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed),
          functionHeader =
          "manual_examples.h",
          functionSourceLoc =
          "manual_examples.h:32:5"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "sum_triple",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Triple")))
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Sum"))),
      foreignImportCRes = TypeTypedef
        (CName "sum"),
      foreignImportCArgs = [
        TypePointer
          (TypeStruct
            (DeclPathName
              (CName "triple")
              DeclPathCtxtTop))],
      foreignImportOrigName =
      "sum_triple",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "sum_triple",
          functionArgs = [
            TypePointer
              (TypeStruct
                (DeclPathName
                  (CName "triple")
                  DeclPathCtxtTop))],
          functionRes = TypeTypedef
            (CName "sum"),
          functionHeader =
          "manual_examples.h",
          functionSourceLoc =
          "manual_examples.h:41:5"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "average_triple",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Triple")))
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Average"))),
      foreignImportCRes = TypeTypedef
        (CName "average"),
      foreignImportCArgs = [
        TypePointer
          (TypeStruct
            (DeclPathName
              (CName "triple")
              DeclPathCtxtTop))],
      foreignImportOrigName =
      "average_triple",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "average_triple",
          functionArgs = [
            TypePointer
              (TypeStruct
                (DeclPathName
                  (CName "triple")
                  DeclPathCtxtTop))],
          functionRes = TypeTypedef
            (CName "average"),
          functionHeader =
          "manual_examples.h",
          functionSourceLoc =
          "manual_examples.h:42:9"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "getYear",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Date")))
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "YEAR"))),
      foreignImportCRes = TypeTypedef
        (CName "YEAR"),
      foreignImportCArgs = [
        TypePointer
          (TypeTypedef (CName "date"))],
      foreignImportOrigName =
      "getYear",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "getYear",
          functionArgs = [
            TypePointer
              (TypeTypedef (CName "date"))],
          functionRes = TypeTypedef
            (CName "YEAR"),
          functionHeader =
          "manual_examples.h",
          functionSourceLoc =
          "manual_examples.h:63:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "print_occupation",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Occupation")))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportCRes = TypeVoid,
      foreignImportCArgs = [
        TypePrim
          (PrimIntegral PrimInt Signed),
        TypePointer
          (TypeUnion
            (DeclPathName
              (CName "occupation")
              DeclPathCtxtTop))],
      foreignImportOrigName =
      "print_occupation",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "print_occupation",
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePointer
              (TypeUnion
                (DeclPathName
                  (CName "occupation")
                  DeclPathCtxtTop))],
          functionRes = TypeVoid,
          functionHeader =
          "manual_examples.h",
          functionSourceLoc =
          "manual_examples.h:82:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "\25308\25308",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportCRes = TypeVoid,
      foreignImportCArgs = [],
      foreignImportOrigName =
      "\25308\25308",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "\25308\25308",
          functionArgs = [],
          functionRes = TypeVoid,
          functionHeader =
          "manual_examples.h",
          functionSourceLoc =
          "manual_examples.h:110:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "c\978",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportCRes = TypeVoid,
      foreignImportCArgs = [],
      foreignImportOrigName = "\978",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "\978",
          functionArgs = [],
          functionRes = TypeVoid,
          functionHeader =
          "manual_examples.h",
          functionSourceLoc =
          "manual_examples.h:112:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "import'",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportCRes = TypeVoid,
      foreignImportCArgs = [],
      foreignImportOrigName =
      "import",
      foreignImportHeader =
      "manual_examples.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "import",
          functionArgs = [],
          functionRes = TypeVoid,
          functionHeader =
          "manual_examples.h",
          functionSourceLoc =
          "manual_examples.h:114:6"}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Triple",
      structConstr = HsName
        "@NsConstr"
        "Triple",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "triple_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:15:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "triple_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:16:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "triple_c",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "c",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:17:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "triple")
            DeclPathCtxtTop,
          structAliases = [
            CName "triple"],
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:15:9"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:16:9"},
            StructField {
              fieldName = CName "c",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:17:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "manual_examples.h:14:16"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Triple",
        structConstr = HsName
          "@NsConstr"
          "Triple",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "triple_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:15:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "triple_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:16:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "triple_c",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "c",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:17:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "triple")
              DeclPathCtxtTop,
            structAliases = [
              CName "triple"],
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:15:9"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:16:9"},
              StructField {
                fieldName = CName "c",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:17:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "manual_examples.h:14:16"}}
      StorableInstance {
        storableSizeOf = 12,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Triple",
                structConstr = HsName
                  "@NsConstr"
                  "Triple",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:15:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:16:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_c",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:17:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "triple")
                      DeclPathCtxtTop,
                    structAliases = [
                      CName "triple"],
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:15:9"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:16:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:17:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:14:16"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4,
              PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Triple",
                structConstr = HsName
                  "@NsConstr"
                  "Triple",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:15:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:16:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "triple_c",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:17:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "triple")
                      DeclPathCtxtTop,
                    structAliases = [
                      CName "triple"],
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:15:9"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:16:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:17:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:14:16"}}
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Index",
      newtypeConstr = HsName
        "@NsConstr"
        "Index",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Index",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "index")
            DeclPathCtxtTop,
          enumAliases = [CName "index"],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "A",
              valueValue = 0,
              valueSourceLoc =
              "manual_examples.h:27:5"},
            EnumValue {
              valueName = CName "B",
              valueValue = 1,
              valueSourceLoc =
              "manual_examples.h:28:5"},
            EnumValue {
              valueName = CName "C",
              valueValue = 2,
              valueSourceLoc =
              "manual_examples.h:29:5"}],
          enumSourceLoc =
          "manual_examples.h:26:14"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Index",
        structConstr = HsName
          "@NsConstr"
          "Index",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Index",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "index")
              DeclPathCtxtTop,
            enumAliases = [CName "index"],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "A",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:27:5"},
              EnumValue {
                valueName = CName "B",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:28:5"},
              EnumValue {
                valueName = CName "C",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:29:5"}],
            enumSourceLoc =
            "manual_examples.h:26:14"}}
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
                  "Index",
                structConstr = HsName
                  "@NsConstr"
                  "Index",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Index",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "index")
                      DeclPathCtxtTop,
                    enumAliases = [CName "index"],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "A",
                        valueValue = 0,
                        valueSourceLoc =
                        "manual_examples.h:27:5"},
                      EnumValue {
                        valueName = CName "B",
                        valueValue = 1,
                        valueSourceLoc =
                        "manual_examples.h:28:5"},
                      EnumValue {
                        valueName = CName "C",
                        valueValue = 2,
                        valueSourceLoc =
                        "manual_examples.h:29:5"}],
                    enumSourceLoc =
                    "manual_examples.h:26:14"}})
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
                  "Index",
                structConstr = HsName
                  "@NsConstr"
                  "Index",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Index",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "index")
                      DeclPathCtxtTop,
                    enumAliases = [CName "index"],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "A",
                        valueValue = 0,
                        valueSourceLoc =
                        "manual_examples.h:27:5"},
                      EnumValue {
                        valueName = CName "B",
                        valueValue = 1,
                        valueSourceLoc =
                        "manual_examples.h:28:5"},
                      EnumValue {
                        valueName = CName "C",
                        valueValue = 2,
                        valueSourceLoc =
                        "manual_examples.h:29:5"}],
                    enumSourceLoc =
                    "manual_examples.h:26:14"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Index"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Index"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Index"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Index",
        structConstr = HsName
          "@NsConstr"
          "Index",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Index",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "index")
              DeclPathCtxtTop,
            enumAliases = [CName "index"],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "A",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:27:5"},
              EnumValue {
                valueName = CName "B",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:28:5"},
              EnumValue {
                valueName = CName "C",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:29:5"}],
            enumSourceLoc =
            "manual_examples.h:26:14"}}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["A"]),
          _×_ 1 (NE.fromList ["B"]),
          _×_ 2 (NE.fromList ["C"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Index",
        structConstr = HsName
          "@NsConstr"
          "Index",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Index",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "index")
              DeclPathCtxtTop,
            enumAliases = [CName "index"],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "A",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:27:5"},
              EnumValue {
                valueName = CName "B",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:28:5"},
              EnumValue {
                valueName = CName "C",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:29:5"}],
            enumSourceLoc =
            "manual_examples.h:26:14"}}
      (HsName "@NsConstr" "A")
      (HsName "@NsConstr" "C")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Index",
        structConstr = HsName
          "@NsConstr"
          "Index",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Index",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "index")
              DeclPathCtxtTop,
            enumAliases = [CName "index"],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "A",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:27:5"},
              EnumValue {
                valueName = CName "B",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:28:5"},
              EnumValue {
                valueName = CName "C",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:29:5"}],
            enumSourceLoc =
            "manual_examples.h:26:14"}}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "A",
      patSynType = HsName
        "@NsTypeConstr"
        "Index",
      patSynConstr = HsName
        "@NsConstr"
        "Index",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "A",
          valueValue = 0,
          valueSourceLoc =
          "manual_examples.h:27:5"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "B",
      patSynType = HsName
        "@NsTypeConstr"
        "Index",
      patSynConstr = HsName
        "@NsConstr"
        "Index",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "B",
          valueValue = 1,
          valueSourceLoc =
          "manual_examples.h:28:5"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "C",
      patSynType = HsName
        "@NsTypeConstr"
        "Index",
      patSynConstr = HsName
        "@NsConstr"
        "Index",
      patSynValue = 2,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "C",
          valueValue = 2,
          valueSourceLoc =
          "manual_examples.h:29:5"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Sum",
      newtypeConstr = HsName
        "@NsConstr"
        "Sum",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Sum",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "sum",
          typedefType = TypePrim
            (PrimIntegral PrimInt Signed),
          typedefSourceLoc =
          "manual_examples.h:38:13"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "Sum"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Average",
      newtypeConstr = HsName
        "@NsConstr"
        "Average",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Average",
        fieldType = HsPrimType
          HsPrimCDouble,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "average",
          typedefType = TypePrim
            (PrimFloating PrimDouble),
          typedefSourceLoc =
          "manual_examples.h:39:16"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    Floating
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    Fractional
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFloat
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFrac
    (HsName
      "@NsTypeConstr"
      "Average"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Date",
      structConstr = HsName
        "@NsConstr"
        "Date",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "date_year",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "YEAR"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "year",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "YEAR"),
              fieldSourceLoc =
              "manual_examples.h:58:11"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "date_month",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "MONTH"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "month",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "MONTH"),
              fieldSourceLoc =
              "manual_examples.h:59:11"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "date_day",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "DAY"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "day",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "DAY"),
              fieldSourceLoc =
              "manual_examples.h:60:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "date")
            DeclPathCtxtTop,
          structAliases = [CName "date"],
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "year",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "YEAR"),
              fieldSourceLoc =
              "manual_examples.h:58:11"},
            StructField {
              fieldName = CName "month",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "MONTH"),
              fieldSourceLoc =
              "manual_examples.h:59:11"},
            StructField {
              fieldName = CName "day",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "DAY"),
              fieldSourceLoc =
              "manual_examples.h:60:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "manual_examples.h:57:16"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Date",
        structConstr = HsName
          "@NsConstr"
          "Date",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "date_year",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "YEAR"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "year",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "YEAR"),
                fieldSourceLoc =
                "manual_examples.h:58:11"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "date_month",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "MONTH"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "month",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "MONTH"),
                fieldSourceLoc =
                "manual_examples.h:59:11"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "date_day",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "DAY"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "day",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "DAY"),
                fieldSourceLoc =
                "manual_examples.h:60:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "date")
              DeclPathCtxtTop,
            structAliases = [CName "date"],
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "year",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "YEAR"),
                fieldSourceLoc =
                "manual_examples.h:58:11"},
              StructField {
                fieldName = CName "month",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "MONTH"),
                fieldSourceLoc =
                "manual_examples.h:59:11"},
              StructField {
                fieldName = CName "day",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "DAY"),
                fieldSourceLoc =
                "manual_examples.h:60:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "manual_examples.h:57:16"}}
      StorableInstance {
        storableSizeOf = 12,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Date",
                structConstr = HsName
                  "@NsConstr"
                  "Date",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_year",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "YEAR"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "year",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "YEAR"),
                        fieldSourceLoc =
                        "manual_examples.h:58:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_month",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "MONTH"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "month",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "MONTH"),
                        fieldSourceLoc =
                        "manual_examples.h:59:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_day",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "DAY"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "day",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "DAY"),
                        fieldSourceLoc =
                        "manual_examples.h:60:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "date")
                      DeclPathCtxtTop,
                    structAliases = [CName "date"],
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "year",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "YEAR"),
                        fieldSourceLoc =
                        "manual_examples.h:58:11"},
                      StructField {
                        fieldName = CName "month",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "MONTH"),
                        fieldSourceLoc =
                        "manual_examples.h:59:11"},
                      StructField {
                        fieldName = CName "day",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "DAY"),
                        fieldSourceLoc =
                        "manual_examples.h:60:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:57:16"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4,
              PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Date",
                structConstr = HsName
                  "@NsConstr"
                  "Date",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_year",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "YEAR"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "year",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "YEAR"),
                        fieldSourceLoc =
                        "manual_examples.h:58:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_month",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "MONTH"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "month",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "MONTH"),
                        fieldSourceLoc =
                        "manual_examples.h:59:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "date_day",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "DAY"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "day",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "DAY"),
                        fieldSourceLoc =
                        "manual_examples.h:60:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "date")
                      DeclPathCtxtTop,
                    structAliases = [CName "date"],
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "year",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "YEAR"),
                        fieldSourceLoc =
                        "manual_examples.h:58:11"},
                      StructField {
                        fieldName = CName "month",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "MONTH"),
                        fieldSourceLoc =
                        "manual_examples.h:59:11"},
                      StructField {
                        fieldName = CName "day",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "DAY"),
                        fieldSourceLoc =
                        "manual_examples.h:60:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:57:16"}}
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Date"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Date"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Student",
      structConstr = HsName
        "@NsConstr"
        "Student",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "student_university",
          fieldType = HsPtr
            (HsPrimType HsPrimCChar),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "university",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))),
              fieldSourceLoc =
              "manual_examples.h:71:11"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "student_year",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "year",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:72:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "student")
            (DeclPathCtxtField
              (Just (CName "occupation"))
              (CName "student")
              DeclPathCtxtTop),
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "university",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))),
              fieldSourceLoc =
              "manual_examples.h:71:11"},
            StructField {
              fieldName = CName "year",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:72:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "manual_examples.h:70:10"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Student",
        structConstr = HsName
          "@NsConstr"
          "Student",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "student_university",
            fieldType = HsPtr
              (HsPrimType HsPrimCChar),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "university",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed)))),
                fieldSourceLoc =
                "manual_examples.h:71:11"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "student_year",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "year",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:72:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "student")
              (DeclPathCtxtField
                (Just (CName "occupation"))
                (CName "student")
                DeclPathCtxtTop),
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "university",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed)))),
                fieldSourceLoc =
                "manual_examples.h:71:11"},
              StructField {
                fieldName = CName "year",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:72:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "manual_examples.h:70:10"}}
      StorableInstance {
        storableSizeOf = 16,
        storableAlignment = 8,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Student",
                structConstr = HsName
                  "@NsConstr"
                  "Student",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "student_university",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCChar),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "university",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        fieldSourceLoc =
                        "manual_examples.h:71:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "student_year",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "year",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:72:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "student")
                      (DeclPathCtxtField
                        (Just (CName "occupation"))
                        (CName "student")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "university",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        fieldSourceLoc =
                        "manual_examples.h:71:11"},
                      StructField {
                        fieldName = CName "year",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:72:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:70:10"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Student",
                structConstr = HsName
                  "@NsConstr"
                  "Student",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "student_university",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCChar),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "university",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        fieldSourceLoc =
                        "manual_examples.h:71:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "student_year",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "year",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:72:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "student")
                      (DeclPathCtxtField
                        (Just (CName "occupation"))
                        (CName "student")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "university",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        fieldSourceLoc =
                        "manual_examples.h:71:11"},
                      StructField {
                        fieldName = CName "year",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:72:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:70:10"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Student"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Student"),
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Person",
      emptyDataOrigin =
      EmptyDataOriginOpaqueStruct
        OpaqueStruct {
          opaqueStructDeclPath =
          DeclPathName
            (CName "person")
            (DeclPathCtxtPtr
              (DeclPathCtxtField
                (Just (CName "employee"))
                (CName "supervisor")
                (DeclPathCtxtField
                  (Just (CName "occupation"))
                  (CName "employee")
                  DeclPathCtxtTop))),
          opaqueStructAliases = [],
          opaqueStructSourceLoc =
          "manual_examples.h:77:12"}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Employee",
      structConstr = HsName
        "@NsConstr"
        "Employee",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "employee_company",
          fieldType = HsPtr
            (HsPrimType HsPrimCChar),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "company",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))),
              fieldSourceLoc =
              "manual_examples.h:76:11"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "employee_supervisor",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Person")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "supervisor",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathName
                    (CName "person")
                    (DeclPathCtxtPtr
                      (DeclPathCtxtField
                        (Just (CName "employee"))
                        (CName "supervisor")
                        (DeclPathCtxtField
                          (Just (CName "occupation"))
                          (CName "employee")
                          DeclPathCtxtTop))))),
              fieldSourceLoc =
              "manual_examples.h:77:20"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "employee_salary",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "salary",
              fieldOffset = 128,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:78:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "employee")
            (DeclPathCtxtField
              (Just (CName "occupation"))
              (CName "employee")
              DeclPathCtxtTop),
          structAliases = [],
          structSizeof = 24,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "company",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))),
              fieldSourceLoc =
              "manual_examples.h:76:11"},
            StructField {
              fieldName = CName "supervisor",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathName
                    (CName "person")
                    (DeclPathCtxtPtr
                      (DeclPathCtxtField
                        (Just (CName "employee"))
                        (CName "supervisor")
                        (DeclPathCtxtField
                          (Just (CName "occupation"))
                          (CName "employee")
                          DeclPathCtxtTop))))),
              fieldSourceLoc =
              "manual_examples.h:77:20"},
            StructField {
              fieldName = CName "salary",
              fieldOffset = 128,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:78:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "manual_examples.h:75:10"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Employee",
        structConstr = HsName
          "@NsConstr"
          "Employee",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "employee_company",
            fieldType = HsPtr
              (HsPrimType HsPrimCChar),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "company",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed)))),
                fieldSourceLoc =
                "manual_examples.h:76:11"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "employee_supervisor",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Person")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "supervisor",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathName
                      (CName "person")
                      (DeclPathCtxtPtr
                        (DeclPathCtxtField
                          (Just (CName "employee"))
                          (CName "supervisor")
                          (DeclPathCtxtField
                            (Just (CName "occupation"))
                            (CName "employee")
                            DeclPathCtxtTop))))),
                fieldSourceLoc =
                "manual_examples.h:77:20"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "employee_salary",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "salary",
                fieldOffset = 128,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:78:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "employee")
              (DeclPathCtxtField
                (Just (CName "occupation"))
                (CName "employee")
                DeclPathCtxtTop),
            structAliases = [],
            structSizeof = 24,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "company",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed)))),
                fieldSourceLoc =
                "manual_examples.h:76:11"},
              StructField {
                fieldName = CName "supervisor",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathName
                      (CName "person")
                      (DeclPathCtxtPtr
                        (DeclPathCtxtField
                          (Just (CName "employee"))
                          (CName "supervisor")
                          (DeclPathCtxtField
                            (Just (CName "occupation"))
                            (CName "employee")
                            DeclPathCtxtTop))))),
                fieldSourceLoc =
                "manual_examples.h:77:20"},
              StructField {
                fieldName = CName "salary",
                fieldOffset = 128,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:78:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "manual_examples.h:75:10"}}
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
                  "Employee",
                structConstr = HsName
                  "@NsConstr"
                  "Employee",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_company",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCChar),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "company",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        fieldSourceLoc =
                        "manual_examples.h:76:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_supervisor",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Person")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "supervisor",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "person")
                              (DeclPathCtxtPtr
                                (DeclPathCtxtField
                                  (Just (CName "employee"))
                                  (CName "supervisor")
                                  (DeclPathCtxtField
                                    (Just (CName "occupation"))
                                    (CName "employee")
                                    DeclPathCtxtTop))))),
                        fieldSourceLoc =
                        "manual_examples.h:77:20"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_salary",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "salary",
                        fieldOffset = 128,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:78:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "employee")
                      (DeclPathCtxtField
                        (Just (CName "occupation"))
                        (CName "employee")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 24,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "company",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        fieldSourceLoc =
                        "manual_examples.h:76:11"},
                      StructField {
                        fieldName = CName "supervisor",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "person")
                              (DeclPathCtxtPtr
                                (DeclPathCtxtField
                                  (Just (CName "employee"))
                                  (CName "supervisor")
                                  (DeclPathCtxtField
                                    (Just (CName "occupation"))
                                    (CName "employee")
                                    DeclPathCtxtTop))))),
                        fieldSourceLoc =
                        "manual_examples.h:77:20"},
                      StructField {
                        fieldName = CName "salary",
                        fieldOffset = 128,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:78:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:75:10"}})
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
                  "Employee",
                structConstr = HsName
                  "@NsConstr"
                  "Employee",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_company",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCChar),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "company",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        fieldSourceLoc =
                        "manual_examples.h:76:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_supervisor",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Person")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "supervisor",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "person")
                              (DeclPathCtxtPtr
                                (DeclPathCtxtField
                                  (Just (CName "employee"))
                                  (CName "supervisor")
                                  (DeclPathCtxtField
                                    (Just (CName "occupation"))
                                    (CName "employee")
                                    DeclPathCtxtTop))))),
                        fieldSourceLoc =
                        "manual_examples.h:77:20"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "employee_salary",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "salary",
                        fieldOffset = 128,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:78:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "employee")
                      (DeclPathCtxtField
                        (Just (CName "occupation"))
                        (CName "employee")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 24,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "company",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed)))),
                        fieldSourceLoc =
                        "manual_examples.h:76:11"},
                      StructField {
                        fieldName = CName "supervisor",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "person")
                              (DeclPathCtxtPtr
                                (DeclPathCtxtField
                                  (Just (CName "employee"))
                                  (CName "supervisor")
                                  (DeclPathCtxtField
                                    (Just (CName "occupation"))
                                    (CName "employee")
                                    DeclPathCtxtTop))))),
                        fieldSourceLoc =
                        "manual_examples.h:77:20"},
                      StructField {
                        fieldName = CName "salary",
                        fieldOffset = 128,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:78:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:75:10"}}
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 8 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    16
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Employee"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Employee"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Occupation",
      newtypeConstr = HsName
        "@NsConstr"
        "Occupation",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Occupation",
        fieldType = HsByteArray,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginUnion
        Union {
          unionDeclPath = DeclPathName
            (CName "occupation")
            DeclPathCtxtTop,
          unionAliases = [
            CName "occupation"],
          unionSizeof = 24,
          unionAlignment = 8,
          unionFields = [
            UnionField {
              ufieldName = CName "student",
              ufieldType = TypeStruct
                (DeclPathName
                  (CName "student")
                  (DeclPathCtxtField
                    (Just (CName "occupation"))
                    (CName "student")
                    DeclPathCtxtTop)),
              ufieldSourceLoc =
              "manual_examples.h:73:5"},
            UnionField {
              ufieldName = CName "employee",
              ufieldType = TypeStruct
                (DeclPathName
                  (CName "employee")
                  (DeclPathCtxtField
                    (Just (CName "occupation"))
                    (CName "employee")
                    DeclPathCtxtTop)),
              ufieldSourceLoc =
              "manual_examples.h:79:5"}],
          unionSourceLoc =
          "manual_examples.h:69:15"}},
  DeclNewtypeInstance
    (DeriveVia
      (HsSizedByteArray 24 8))
    Storable
    (HsName
      "@NsTypeConstr"
      "Occupation"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "Occupation")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "Student"))
    (HsName
      "@NsVar"
      "get_occupation_student"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "Occupation")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "Student"))
    (HsName
      "@NsVar"
      "set_occupation_student"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "Occupation")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "Employee"))
    (HsName
      "@NsVar"
      "get_occupation_employee"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "Occupation")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "Employee"))
    (HsName
      "@NsVar"
      "set_occupation_employee"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Rect_lower_left",
      structConstr = HsName
        "@NsConstr"
        "Rect_lower_left",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_lower_left_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:90:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_lower_left_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:91:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathAnon
            (DeclPathCtxtField
              (Just (CName "rect"))
              (CName "lower_left")
              DeclPathCtxtTop),
          structAliases = [],
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:90:9"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:91:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "manual_examples.h:89:3"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Rect_lower_left",
        structConstr = HsName
          "@NsConstr"
          "Rect_lower_left",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_lower_left_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:90:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_lower_left_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:91:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathAnon
              (DeclPathCtxtField
                (Just (CName "rect"))
                (CName "lower_left")
                DeclPathCtxtTop),
            structAliases = [],
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:90:9"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:91:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "manual_examples.h:89:3"}}
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
                  "Rect_lower_left",
                structConstr = HsName
                  "@NsConstr"
                  "Rect_lower_left",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:90:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:91:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtField
                        (Just (CName "rect"))
                        (CName "lower_left")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:90:9"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:91:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:89:3"}})
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
                  "Rect_lower_left",
                structConstr = HsName
                  "@NsConstr"
                  "Rect_lower_left",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:90:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:91:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtField
                        (Just (CName "rect"))
                        (CName "lower_left")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:90:9"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:91:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:89:3"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Rect_lower_left"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Rect_lower_left"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Rect_upper_right",
      structConstr = HsName
        "@NsConstr"
        "Rect_upper_right",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_upper_right_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:95:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_upper_right_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:96:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathAnon
            (DeclPathCtxtField
              (Just (CName "rect"))
              (CName "upper_right")
              DeclPathCtxtTop),
          structAliases = [],
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:95:9"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:96:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "manual_examples.h:94:3"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Rect_upper_right",
        structConstr = HsName
          "@NsConstr"
          "Rect_upper_right",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_upper_right_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:95:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_upper_right_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:96:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathAnon
              (DeclPathCtxtField
                (Just (CName "rect"))
                (CName "upper_right")
                DeclPathCtxtTop),
            structAliases = [],
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:95:9"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:96:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "manual_examples.h:94:3"}}
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
                  "Rect_upper_right",
                structConstr = HsName
                  "@NsConstr"
                  "Rect_upper_right",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:95:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:96:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtField
                        (Just (CName "rect"))
                        (CName "upper_right")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:95:9"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:96:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:94:3"}})
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
                  "Rect_upper_right",
                structConstr = HsName
                  "@NsConstr"
                  "Rect_upper_right",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:95:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:96:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtField
                        (Just (CName "rect"))
                        (CName "upper_right")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:95:9"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:96:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:94:3"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Rect_upper_right"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Rect_upper_right"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Rect",
      structConstr = HsName
        "@NsConstr"
        "Rect",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_lower_left",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Rect_lower_left"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "lower_left",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathAnon
                  (DeclPathCtxtField
                    (Just (CName "rect"))
                    (CName "lower_left")
                    DeclPathCtxtTop)),
              fieldSourceLoc =
              "manual_examples.h:92:5"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "rect_upper_right",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Rect_upper_right"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "upper_right",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathAnon
                  (DeclPathCtxtField
                    (Just (CName "rect"))
                    (CName "upper_right")
                    DeclPathCtxtTop)),
              fieldSourceLoc =
              "manual_examples.h:97:5"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "rect")
            DeclPathCtxtTop,
          structAliases = [],
          structSizeof = 16,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "lower_left",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathAnon
                  (DeclPathCtxtField
                    (Just (CName "rect"))
                    (CName "lower_left")
                    DeclPathCtxtTop)),
              fieldSourceLoc =
              "manual_examples.h:92:5"},
            StructField {
              fieldName = CName "upper_right",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathAnon
                  (DeclPathCtxtField
                    (Just (CName "rect"))
                    (CName "upper_right")
                    DeclPathCtxtTop)),
              fieldSourceLoc =
              "manual_examples.h:97:5"}],
          structFlam = Nothing,
          structSourceLoc =
          "manual_examples.h:88:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Rect",
        structConstr = HsName
          "@NsConstr"
          "Rect",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_lower_left",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Rect_lower_left"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "lower_left",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathAnon
                    (DeclPathCtxtField
                      (Just (CName "rect"))
                      (CName "lower_left")
                      DeclPathCtxtTop)),
                fieldSourceLoc =
                "manual_examples.h:92:5"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "rect_upper_right",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Rect_upper_right"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "upper_right",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathAnon
                    (DeclPathCtxtField
                      (Just (CName "rect"))
                      (CName "upper_right")
                      DeclPathCtxtTop)),
                fieldSourceLoc =
                "manual_examples.h:97:5"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "rect")
              DeclPathCtxtTop,
            structAliases = [],
            structSizeof = 16,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "lower_left",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathAnon
                    (DeclPathCtxtField
                      (Just (CName "rect"))
                      (CName "lower_left")
                      DeclPathCtxtTop)),
                fieldSourceLoc =
                "manual_examples.h:92:5"},
              StructField {
                fieldName = CName "upper_right",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathAnon
                    (DeclPathCtxtField
                      (Just (CName "rect"))
                      (CName "upper_right")
                      DeclPathCtxtTop)),
                fieldSourceLoc =
                "manual_examples.h:97:5"}],
            structFlam = Nothing,
            structSourceLoc =
            "manual_examples.h:88:8"}}
      StorableInstance {
        storableSizeOf = 16,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Rect",
                structConstr = HsName
                  "@NsConstr"
                  "Rect",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Rect_lower_left"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "lower_left",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "rect"))
                              (CName "lower_left")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "manual_examples.h:92:5"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Rect_upper_right"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "upper_right",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "rect"))
                              (CName "upper_right")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "manual_examples.h:97:5"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "rect")
                      DeclPathCtxtTop,
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "lower_left",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "rect"))
                              (CName "lower_left")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "manual_examples.h:92:5"},
                      StructField {
                        fieldName = CName "upper_right",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "rect"))
                              (CName "upper_right")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "manual_examples.h:97:5"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:88:8"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Rect",
                structConstr = HsName
                  "@NsConstr"
                  "Rect",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_lower_left",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Rect_lower_left"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "lower_left",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "rect"))
                              (CName "lower_left")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "manual_examples.h:92:5"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "rect_upper_right",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Rect_upper_right"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "upper_right",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "rect"))
                              (CName "upper_right")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "manual_examples.h:97:5"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "rect")
                      DeclPathCtxtTop,
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "lower_left",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "rect"))
                              (CName "lower_left")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "manual_examples.h:92:5"},
                      StructField {
                        fieldName = CName "upper_right",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "rect"))
                              (CName "upper_right")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "manual_examples.h:97:5"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:88:8"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Rect"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Rect"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Config_Deref",
      structConstr = HsName
        "@NsConstr"
        "Config_Deref",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "config_Deref_width",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "width",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:101:7"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "config_Deref_height",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "height",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:102:7"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathAnon
            (DeclPathCtxtPtr
              (DeclPathCtxtTypedef
                (CName "config"))),
          structAliases = [],
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "width",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:101:7"},
            StructField {
              fieldName = CName "height",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "manual_examples.h:102:7"}],
          structFlam = Nothing,
          structSourceLoc =
          "manual_examples.h:100:9"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Config_Deref",
        structConstr = HsName
          "@NsConstr"
          "Config_Deref",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "config_Deref_width",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "width",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:101:7"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "config_Deref_height",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "height",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:102:7"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathAnon
              (DeclPathCtxtPtr
                (DeclPathCtxtTypedef
                  (CName "config"))),
            structAliases = [],
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "width",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:101:7"},
              StructField {
                fieldName = CName "height",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "manual_examples.h:102:7"}],
            structFlam = Nothing,
            structSourceLoc =
            "manual_examples.h:100:9"}}
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
                  "Config_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "Config_Deref",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "config_Deref_width",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "width",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:101:7"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "config_Deref_height",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "height",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:102:7"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtPtr
                        (DeclPathCtxtTypedef
                          (CName "config"))),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "width",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:101:7"},
                      StructField {
                        fieldName = CName "height",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:102:7"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:100:9"}})
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
                  "Config_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "Config_Deref",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "config_Deref_width",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "width",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:101:7"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "config_Deref_height",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "height",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:102:7"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtPtr
                        (DeclPathCtxtTypedef
                          (CName "config"))),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "width",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:101:7"},
                      StructField {
                        fieldName = CName "height",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "manual_examples.h:102:7"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "manual_examples.h:100:9"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Config_Deref"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Config_Deref"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Config",
      newtypeConstr = HsName
        "@NsConstr"
        "Config",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Config",
        fieldType = HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Config_Deref")),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "config",
          typedefType = TypePointer
            (TypeStruct
              (DeclPathAnon
                (DeclPathCtxtPtr
                  (DeclPathCtxtTypedef
                    (CName "config"))))),
          typedefSourceLoc =
          "manual_examples.h:103:4"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Config"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Adio'0301s",
      newtypeConstr = HsName
        "@NsConstr"
        "Adio'0301s",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Adio'0301s",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "adio\769s",
          typedefType = TypePrim
            (PrimIntegral PrimInt Signed),
          typedefSourceLoc =
          "manual_examples.h:109:13"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Adio'0301s"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "C\25968\23383",
      newtypeConstr = HsName
        "@NsConstr"
        "C\25968\23383",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_C\25968\23383",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName
            "\25968\23383",
          typedefType = TypePrim
            (PrimIntegral PrimInt Signed),
          typedefSourceLoc =
          "manual_examples.h:111:13"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "C\25968\23383"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Data",
      newtypeConstr = HsName
        "@NsConstr"
        "Data",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Data",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "data",
          typedefType = TypePrim
            (PrimIntegral PrimInt Signed),
          typedefSourceLoc =
          "manual_examples.h:113:13"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "Data"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Signal",
      newtypeConstr = HsName
        "@NsConstr"
        "Signal",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Signal",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "signal")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "start",
              valueValue = 1,
              valueSourceLoc =
              "manual_examples.h:121:3"},
            EnumValue {
              valueName = CName "pause",
              valueValue = 2,
              valueSourceLoc =
              "manual_examples.h:122:3"},
            EnumValue {
              valueName = CName "resume",
              valueValue = 3,
              valueSourceLoc =
              "manual_examples.h:123:3"},
            EnumValue {
              valueName = CName "stop",
              valueValue = 4,
              valueSourceLoc =
              "manual_examples.h:124:3"}],
          enumSourceLoc =
          "manual_examples.h:120:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Signal",
        structConstr = HsName
          "@NsConstr"
          "Signal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Signal",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "signal")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "start",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:121:3"},
              EnumValue {
                valueName = CName "pause",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:122:3"},
              EnumValue {
                valueName = CName "resume",
                valueValue = 3,
                valueSourceLoc =
                "manual_examples.h:123:3"},
              EnumValue {
                valueName = CName "stop",
                valueValue = 4,
                valueSourceLoc =
                "manual_examples.h:124:3"}],
            enumSourceLoc =
            "manual_examples.h:120:6"}}
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
                  "Signal",
                structConstr = HsName
                  "@NsConstr"
                  "Signal",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Signal",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "signal")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "start",
                        valueValue = 1,
                        valueSourceLoc =
                        "manual_examples.h:121:3"},
                      EnumValue {
                        valueName = CName "pause",
                        valueValue = 2,
                        valueSourceLoc =
                        "manual_examples.h:122:3"},
                      EnumValue {
                        valueName = CName "resume",
                        valueValue = 3,
                        valueSourceLoc =
                        "manual_examples.h:123:3"},
                      EnumValue {
                        valueName = CName "stop",
                        valueValue = 4,
                        valueSourceLoc =
                        "manual_examples.h:124:3"}],
                    enumSourceLoc =
                    "manual_examples.h:120:6"}})
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
                  "Signal",
                structConstr = HsName
                  "@NsConstr"
                  "Signal",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Signal",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "signal")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "start",
                        valueValue = 1,
                        valueSourceLoc =
                        "manual_examples.h:121:3"},
                      EnumValue {
                        valueName = CName "pause",
                        valueValue = 2,
                        valueSourceLoc =
                        "manual_examples.h:122:3"},
                      EnumValue {
                        valueName = CName "resume",
                        valueValue = 3,
                        valueSourceLoc =
                        "manual_examples.h:123:3"},
                      EnumValue {
                        valueName = CName "stop",
                        valueValue = 4,
                        valueSourceLoc =
                        "manual_examples.h:124:3"}],
                    enumSourceLoc =
                    "manual_examples.h:120:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Signal"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Signal"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Signal"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Signal",
        structConstr = HsName
          "@NsConstr"
          "Signal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Signal",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "signal")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "start",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:121:3"},
              EnumValue {
                valueName = CName "pause",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:122:3"},
              EnumValue {
                valueName = CName "resume",
                valueValue = 3,
                valueSourceLoc =
                "manual_examples.h:123:3"},
              EnumValue {
                valueName = CName "stop",
                valueValue = 4,
                valueSourceLoc =
                "manual_examples.h:124:3"}],
            enumSourceLoc =
            "manual_examples.h:120:6"}}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 1 (NE.fromList ["Start"]),
          _×_ 2 (NE.fromList ["Pause"]),
          _×_ 3 (NE.fromList ["Resume"]),
          _×_ 4 (NE.fromList ["Stop"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Signal",
        structConstr = HsName
          "@NsConstr"
          "Signal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Signal",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "signal")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "start",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:121:3"},
              EnumValue {
                valueName = CName "pause",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:122:3"},
              EnumValue {
                valueName = CName "resume",
                valueValue = 3,
                valueSourceLoc =
                "manual_examples.h:123:3"},
              EnumValue {
                valueName = CName "stop",
                valueValue = 4,
                valueSourceLoc =
                "manual_examples.h:124:3"}],
            enumSourceLoc =
            "manual_examples.h:120:6"}}
      (HsName "@NsConstr" "Start")
      (HsName "@NsConstr" "Stop")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Signal",
        structConstr = HsName
          "@NsConstr"
          "Signal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Signal",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "signal")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "start",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:121:3"},
              EnumValue {
                valueName = CName "pause",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:122:3"},
              EnumValue {
                valueName = CName "resume",
                valueValue = 3,
                valueSourceLoc =
                "manual_examples.h:123:3"},
              EnumValue {
                valueName = CName "stop",
                valueValue = 4,
                valueSourceLoc =
                "manual_examples.h:124:3"}],
            enumSourceLoc =
            "manual_examples.h:120:6"}}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Start",
      patSynType = HsName
        "@NsTypeConstr"
        "Signal",
      patSynConstr = HsName
        "@NsConstr"
        "Signal",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "start",
          valueValue = 1,
          valueSourceLoc =
          "manual_examples.h:121:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Pause",
      patSynType = HsName
        "@NsTypeConstr"
        "Signal",
      patSynConstr = HsName
        "@NsConstr"
        "Signal",
      patSynValue = 2,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "pause",
          valueValue = 2,
          valueSourceLoc =
          "manual_examples.h:122:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Resume",
      patSynType = HsName
        "@NsTypeConstr"
        "Signal",
      patSynConstr = HsName
        "@NsConstr"
        "Signal",
      patSynValue = 3,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "resume",
          valueValue = 3,
          valueSourceLoc =
          "manual_examples.h:123:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Stop",
      patSynType = HsName
        "@NsTypeConstr"
        "Signal",
      patSynConstr = HsName
        "@NsConstr"
        "Signal",
      patSynValue = 4,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "stop",
          valueValue = 4,
          valueSourceLoc =
          "manual_examples.h:124:3"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "HTTP_status",
      newtypeConstr = HsName
        "@NsConstr"
        "HTTP_status",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_HTTP_status",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "HTTP_status")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "ok",
              valueValue = 200,
              valueSourceLoc =
              "manual_examples.h:128:3"},
            EnumValue {
              valueName = CName "moved",
              valueValue = 301,
              valueSourceLoc =
              "manual_examples.h:129:3"},
            EnumValue {
              valueName = CName "bad_request",
              valueValue = 400,
              valueSourceLoc =
              "manual_examples.h:130:3"},
            EnumValue {
              valueName = CName
                "unauthorized",
              valueValue = 401,
              valueSourceLoc =
              "manual_examples.h:131:3"},
            EnumValue {
              valueName = CName "not_found",
              valueValue = 404,
              valueSourceLoc =
              "manual_examples.h:132:3"}],
          enumSourceLoc =
          "manual_examples.h:127:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "HTTP_status",
        structConstr = HsName
          "@NsConstr"
          "HTTP_status",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_HTTP_status",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "HTTP_status")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "ok",
                valueValue = 200,
                valueSourceLoc =
                "manual_examples.h:128:3"},
              EnumValue {
                valueName = CName "moved",
                valueValue = 301,
                valueSourceLoc =
                "manual_examples.h:129:3"},
              EnumValue {
                valueName = CName "bad_request",
                valueValue = 400,
                valueSourceLoc =
                "manual_examples.h:130:3"},
              EnumValue {
                valueName = CName
                  "unauthorized",
                valueValue = 401,
                valueSourceLoc =
                "manual_examples.h:131:3"},
              EnumValue {
                valueName = CName "not_found",
                valueValue = 404,
                valueSourceLoc =
                "manual_examples.h:132:3"}],
            enumSourceLoc =
            "manual_examples.h:127:6"}}
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
                  "HTTP_status",
                structConstr = HsName
                  "@NsConstr"
                  "HTTP_status",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_HTTP_status",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "HTTP_status")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "ok",
                        valueValue = 200,
                        valueSourceLoc =
                        "manual_examples.h:128:3"},
                      EnumValue {
                        valueName = CName "moved",
                        valueValue = 301,
                        valueSourceLoc =
                        "manual_examples.h:129:3"},
                      EnumValue {
                        valueName = CName "bad_request",
                        valueValue = 400,
                        valueSourceLoc =
                        "manual_examples.h:130:3"},
                      EnumValue {
                        valueName = CName
                          "unauthorized",
                        valueValue = 401,
                        valueSourceLoc =
                        "manual_examples.h:131:3"},
                      EnumValue {
                        valueName = CName "not_found",
                        valueValue = 404,
                        valueSourceLoc =
                        "manual_examples.h:132:3"}],
                    enumSourceLoc =
                    "manual_examples.h:127:6"}})
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
                  "HTTP_status",
                structConstr = HsName
                  "@NsConstr"
                  "HTTP_status",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_HTTP_status",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "HTTP_status")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "ok",
                        valueValue = 200,
                        valueSourceLoc =
                        "manual_examples.h:128:3"},
                      EnumValue {
                        valueName = CName "moved",
                        valueValue = 301,
                        valueSourceLoc =
                        "manual_examples.h:129:3"},
                      EnumValue {
                        valueName = CName "bad_request",
                        valueValue = 400,
                        valueSourceLoc =
                        "manual_examples.h:130:3"},
                      EnumValue {
                        valueName = CName
                          "unauthorized",
                        valueValue = 401,
                        valueSourceLoc =
                        "manual_examples.h:131:3"},
                      EnumValue {
                        valueName = CName "not_found",
                        valueValue = 404,
                        valueSourceLoc =
                        "manual_examples.h:132:3"}],
                    enumSourceLoc =
                    "manual_examples.h:127:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "HTTP_status"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "HTTP_status"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "HTTP_status"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "HTTP_status",
        structConstr = HsName
          "@NsConstr"
          "HTTP_status",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_HTTP_status",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "HTTP_status")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "ok",
                valueValue = 200,
                valueSourceLoc =
                "manual_examples.h:128:3"},
              EnumValue {
                valueName = CName "moved",
                valueValue = 301,
                valueSourceLoc =
                "manual_examples.h:129:3"},
              EnumValue {
                valueName = CName "bad_request",
                valueValue = 400,
                valueSourceLoc =
                "manual_examples.h:130:3"},
              EnumValue {
                valueName = CName
                  "unauthorized",
                valueValue = 401,
                valueSourceLoc =
                "manual_examples.h:131:3"},
              EnumValue {
                valueName = CName "not_found",
                valueValue = 404,
                valueSourceLoc =
                "manual_examples.h:132:3"}],
            enumSourceLoc =
            "manual_examples.h:127:6"}}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 200 (NE.fromList ["Ok"]),
          _×_ 301 (NE.fromList ["Moved"]),
          _×_
            400
            (NE.fromList ["Bad_request"]),
          _×_
            401
            (NE.fromList ["Unauthorized"]),
          _×_
            404
            (NE.fromList ["Not_found"])])
      False),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "HTTP_status",
        structConstr = HsName
          "@NsConstr"
          "HTTP_status",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_HTTP_status",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "HTTP_status")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "ok",
                valueValue = 200,
                valueSourceLoc =
                "manual_examples.h:128:3"},
              EnumValue {
                valueName = CName "moved",
                valueValue = 301,
                valueSourceLoc =
                "manual_examples.h:129:3"},
              EnumValue {
                valueName = CName "bad_request",
                valueValue = 400,
                valueSourceLoc =
                "manual_examples.h:130:3"},
              EnumValue {
                valueName = CName
                  "unauthorized",
                valueValue = 401,
                valueSourceLoc =
                "manual_examples.h:131:3"},
              EnumValue {
                valueName = CName "not_found",
                valueValue = 404,
                valueSourceLoc =
                "manual_examples.h:132:3"}],
            enumSourceLoc =
            "manual_examples.h:127:6"}}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Ok",
      patSynType = HsName
        "@NsTypeConstr"
        "HTTP_status",
      patSynConstr = HsName
        "@NsConstr"
        "HTTP_status",
      patSynValue = 200,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "ok",
          valueValue = 200,
          valueSourceLoc =
          "manual_examples.h:128:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Moved",
      patSynType = HsName
        "@NsTypeConstr"
        "HTTP_status",
      patSynConstr = HsName
        "@NsConstr"
        "HTTP_status",
      patSynValue = 301,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "moved",
          valueValue = 301,
          valueSourceLoc =
          "manual_examples.h:129:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Bad_request",
      patSynType = HsName
        "@NsTypeConstr"
        "HTTP_status",
      patSynConstr = HsName
        "@NsConstr"
        "HTTP_status",
      patSynValue = 400,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "bad_request",
          valueValue = 400,
          valueSourceLoc =
          "manual_examples.h:130:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Unauthorized",
      patSynType = HsName
        "@NsTypeConstr"
        "HTTP_status",
      patSynConstr = HsName
        "@NsConstr"
        "HTTP_status",
      patSynValue = 401,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "unauthorized",
          valueValue = 401,
          valueSourceLoc =
          "manual_examples.h:131:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Not_found",
      patSynType = HsName
        "@NsTypeConstr"
        "HTTP_status",
      patSynConstr = HsName
        "@NsConstr"
        "HTTP_status",
      patSynValue = 404,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "not_found",
          valueValue = 404,
          valueSourceLoc =
          "manual_examples.h:132:3"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Descending",
      newtypeConstr = HsName
        "@NsConstr"
        "Descending",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Descending",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "descending")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "X",
              valueValue = 100,
              valueSourceLoc =
              "manual_examples.h:136:3"},
            EnumValue {
              valueName = CName "Y",
              valueValue = 99,
              valueSourceLoc =
              "manual_examples.h:137:3"},
            EnumValue {
              valueName = CName "Y_alias",
              valueValue = 99,
              valueSourceLoc =
              "manual_examples.h:138:3"},
            EnumValue {
              valueName = CName "Z",
              valueValue = 98,
              valueSourceLoc =
              "manual_examples.h:139:3"}],
          enumSourceLoc =
          "manual_examples.h:135:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Descending",
        structConstr = HsName
          "@NsConstr"
          "Descending",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Descending",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "descending")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "X",
                valueValue = 100,
                valueSourceLoc =
                "manual_examples.h:136:3"},
              EnumValue {
                valueName = CName "Y",
                valueValue = 99,
                valueSourceLoc =
                "manual_examples.h:137:3"},
              EnumValue {
                valueName = CName "Y_alias",
                valueValue = 99,
                valueSourceLoc =
                "manual_examples.h:138:3"},
              EnumValue {
                valueName = CName "Z",
                valueValue = 98,
                valueSourceLoc =
                "manual_examples.h:139:3"}],
            enumSourceLoc =
            "manual_examples.h:135:6"}}
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
                  "Descending",
                structConstr = HsName
                  "@NsConstr"
                  "Descending",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Descending",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "descending")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "X",
                        valueValue = 100,
                        valueSourceLoc =
                        "manual_examples.h:136:3"},
                      EnumValue {
                        valueName = CName "Y",
                        valueValue = 99,
                        valueSourceLoc =
                        "manual_examples.h:137:3"},
                      EnumValue {
                        valueName = CName "Y_alias",
                        valueValue = 99,
                        valueSourceLoc =
                        "manual_examples.h:138:3"},
                      EnumValue {
                        valueName = CName "Z",
                        valueValue = 98,
                        valueSourceLoc =
                        "manual_examples.h:139:3"}],
                    enumSourceLoc =
                    "manual_examples.h:135:6"}})
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
                  "Descending",
                structConstr = HsName
                  "@NsConstr"
                  "Descending",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Descending",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "descending")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "X",
                        valueValue = 100,
                        valueSourceLoc =
                        "manual_examples.h:136:3"},
                      EnumValue {
                        valueName = CName "Y",
                        valueValue = 99,
                        valueSourceLoc =
                        "manual_examples.h:137:3"},
                      EnumValue {
                        valueName = CName "Y_alias",
                        valueValue = 99,
                        valueSourceLoc =
                        "manual_examples.h:138:3"},
                      EnumValue {
                        valueName = CName "Z",
                        valueValue = 98,
                        valueSourceLoc =
                        "manual_examples.h:139:3"}],
                    enumSourceLoc =
                    "manual_examples.h:135:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Descending"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Descending"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Descending"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Descending",
        structConstr = HsName
          "@NsConstr"
          "Descending",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Descending",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "descending")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "X",
                valueValue = 100,
                valueSourceLoc =
                "manual_examples.h:136:3"},
              EnumValue {
                valueName = CName "Y",
                valueValue = 99,
                valueSourceLoc =
                "manual_examples.h:137:3"},
              EnumValue {
                valueName = CName "Y_alias",
                valueValue = 99,
                valueSourceLoc =
                "manual_examples.h:138:3"},
              EnumValue {
                valueName = CName "Z",
                valueValue = 98,
                valueSourceLoc =
                "manual_examples.h:139:3"}],
            enumSourceLoc =
            "manual_examples.h:135:6"}}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 98 (NE.fromList ["Z"]),
          _×_
            99
            (NE.fromList ["Y", "Y_alias"]),
          _×_ 100 (NE.fromList ["X"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Descending",
        structConstr = HsName
          "@NsConstr"
          "Descending",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Descending",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "descending")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "X",
                valueValue = 100,
                valueSourceLoc =
                "manual_examples.h:136:3"},
              EnumValue {
                valueName = CName "Y",
                valueValue = 99,
                valueSourceLoc =
                "manual_examples.h:137:3"},
              EnumValue {
                valueName = CName "Y_alias",
                valueValue = 99,
                valueSourceLoc =
                "manual_examples.h:138:3"},
              EnumValue {
                valueName = CName "Z",
                valueValue = 98,
                valueSourceLoc =
                "manual_examples.h:139:3"}],
            enumSourceLoc =
            "manual_examples.h:135:6"}}
      (HsName "@NsConstr" "Z")
      (HsName "@NsConstr" "X")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Descending",
        structConstr = HsName
          "@NsConstr"
          "Descending",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Descending",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "descending")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "X",
                valueValue = 100,
                valueSourceLoc =
                "manual_examples.h:136:3"},
              EnumValue {
                valueName = CName "Y",
                valueValue = 99,
                valueSourceLoc =
                "manual_examples.h:137:3"},
              EnumValue {
                valueName = CName "Y_alias",
                valueValue = 99,
                valueSourceLoc =
                "manual_examples.h:138:3"},
              EnumValue {
                valueName = CName "Z",
                valueValue = 98,
                valueSourceLoc =
                "manual_examples.h:139:3"}],
            enumSourceLoc =
            "manual_examples.h:135:6"}}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "X",
      patSynType = HsName
        "@NsTypeConstr"
        "Descending",
      patSynConstr = HsName
        "@NsConstr"
        "Descending",
      patSynValue = 100,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "X",
          valueValue = 100,
          valueSourceLoc =
          "manual_examples.h:136:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Y",
      patSynType = HsName
        "@NsTypeConstr"
        "Descending",
      patSynConstr = HsName
        "@NsConstr"
        "Descending",
      patSynValue = 99,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "Y",
          valueValue = 99,
          valueSourceLoc =
          "manual_examples.h:137:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Y_alias",
      patSynType = HsName
        "@NsTypeConstr"
        "Descending",
      patSynConstr = HsName
        "@NsConstr"
        "Descending",
      patSynValue = 99,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "Y_alias",
          valueValue = 99,
          valueSourceLoc =
          "manual_examples.h:138:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Z",
      patSynType = HsName
        "@NsTypeConstr"
        "Descending",
      patSynConstr = HsName
        "@NsConstr"
        "Descending",
      patSynValue = 98,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "Z",
          valueValue = 98,
          valueSourceLoc =
          "manual_examples.h:139:3"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Result",
      newtypeConstr = HsName
        "@NsConstr"
        "Result",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Result",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "result")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Signed),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "failed",
              valueValue = `-1`,
              valueSourceLoc =
              "manual_examples.h:143:3"},
            EnumValue {
              valueName = CName "success",
              valueValue = 0,
              valueSourceLoc =
              "manual_examples.h:144:3"},
            EnumValue {
              valueName = CName "postponed",
              valueValue = 1,
              valueSourceLoc =
              "manual_examples.h:145:3"},
            EnumValue {
              valueName = CName
                "already_done",
              valueValue = 2,
              valueSourceLoc =
              "manual_examples.h:146:3"}],
          enumSourceLoc =
          "manual_examples.h:142:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Result",
        structConstr = HsName
          "@NsConstr"
          "Result",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Result",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "result")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "failed",
                valueValue = `-1`,
                valueSourceLoc =
                "manual_examples.h:143:3"},
              EnumValue {
                valueName = CName "success",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:144:3"},
              EnumValue {
                valueName = CName "postponed",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:145:3"},
              EnumValue {
                valueName = CName
                  "already_done",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:146:3"}],
            enumSourceLoc =
            "manual_examples.h:142:6"}}
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
                  "Result",
                structConstr = HsName
                  "@NsConstr"
                  "Result",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Result",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "result")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "failed",
                        valueValue = `-1`,
                        valueSourceLoc =
                        "manual_examples.h:143:3"},
                      EnumValue {
                        valueName = CName "success",
                        valueValue = 0,
                        valueSourceLoc =
                        "manual_examples.h:144:3"},
                      EnumValue {
                        valueName = CName "postponed",
                        valueValue = 1,
                        valueSourceLoc =
                        "manual_examples.h:145:3"},
                      EnumValue {
                        valueName = CName
                          "already_done",
                        valueValue = 2,
                        valueSourceLoc =
                        "manual_examples.h:146:3"}],
                    enumSourceLoc =
                    "manual_examples.h:142:6"}})
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
                  "Result",
                structConstr = HsName
                  "@NsConstr"
                  "Result",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Result",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "result")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "failed",
                        valueValue = `-1`,
                        valueSourceLoc =
                        "manual_examples.h:143:3"},
                      EnumValue {
                        valueName = CName "success",
                        valueValue = 0,
                        valueSourceLoc =
                        "manual_examples.h:144:3"},
                      EnumValue {
                        valueName = CName "postponed",
                        valueValue = 1,
                        valueSourceLoc =
                        "manual_examples.h:145:3"},
                      EnumValue {
                        valueName = CName
                          "already_done",
                        valueValue = 2,
                        valueSourceLoc =
                        "manual_examples.h:146:3"}],
                    enumSourceLoc =
                    "manual_examples.h:142:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Result"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Result"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Result"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Result",
        structConstr = HsName
          "@NsConstr"
          "Result",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Result",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "result")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "failed",
                valueValue = `-1`,
                valueSourceLoc =
                "manual_examples.h:143:3"},
              EnumValue {
                valueName = CName "success",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:144:3"},
              EnumValue {
                valueName = CName "postponed",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:145:3"},
              EnumValue {
                valueName = CName
                  "already_done",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:146:3"}],
            enumSourceLoc =
            "manual_examples.h:142:6"}}
      (HsPrimType HsPrimCInt)
      (Map.fromList
        [
          _×_
            `-1`
            (NE.fromList ["Failed"]),
          _×_ 0 (NE.fromList ["Success"]),
          _×_
            1
            (NE.fromList ["Postponed"]),
          _×_
            2
            (NE.fromList ["Already_done"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Result",
        structConstr = HsName
          "@NsConstr"
          "Result",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Result",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "result")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "failed",
                valueValue = `-1`,
                valueSourceLoc =
                "manual_examples.h:143:3"},
              EnumValue {
                valueName = CName "success",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:144:3"},
              EnumValue {
                valueName = CName "postponed",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:145:3"},
              EnumValue {
                valueName = CName
                  "already_done",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:146:3"}],
            enumSourceLoc =
            "manual_examples.h:142:6"}}
      (HsName "@NsConstr" "Failed")
      (HsName
        "@NsConstr"
        "Already_done")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Result",
        structConstr = HsName
          "@NsConstr"
          "Result",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Result",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "result")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "failed",
                valueValue = `-1`,
                valueSourceLoc =
                "manual_examples.h:143:3"},
              EnumValue {
                valueName = CName "success",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:144:3"},
              EnumValue {
                valueName = CName "postponed",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:145:3"},
              EnumValue {
                valueName = CName
                  "already_done",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:146:3"}],
            enumSourceLoc =
            "manual_examples.h:142:6"}}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Failed",
      patSynType = HsName
        "@NsTypeConstr"
        "Result",
      patSynConstr = HsName
        "@NsConstr"
        "Result",
      patSynValue = `-1`,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "failed",
          valueValue = `-1`,
          valueSourceLoc =
          "manual_examples.h:143:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Success",
      patSynType = HsName
        "@NsTypeConstr"
        "Result",
      patSynConstr = HsName
        "@NsConstr"
        "Result",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "success",
          valueValue = 0,
          valueSourceLoc =
          "manual_examples.h:144:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Postponed",
      patSynType = HsName
        "@NsTypeConstr"
        "Result",
      patSynConstr = HsName
        "@NsConstr"
        "Result",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "postponed",
          valueValue = 1,
          valueSourceLoc =
          "manual_examples.h:145:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Already_done",
      patSynType = HsName
        "@NsTypeConstr"
        "Result",
      patSynConstr = HsName
        "@NsConstr"
        "Result",
      patSynValue = 2,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "already_done",
          valueValue = 2,
          valueSourceLoc =
          "manual_examples.h:146:3"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Vote",
      newtypeConstr = HsName
        "@NsConstr"
        "Vote",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Vote",
        fieldType = HsPrimType
          HsPrimCUChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "vote")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimChar
              (PrimSignExplicit Unsigned)),
          enumSizeof = 1,
          enumAlignment = 1,
          enumValues = [
            EnumValue {
              valueName = CName "infavour",
              valueValue = 0,
              valueSourceLoc =
              "manual_examples.h:150:3"},
            EnumValue {
              valueName = CName "against",
              valueValue = 1,
              valueSourceLoc =
              "manual_examples.h:151:3"},
            EnumValue {
              valueName = CName "abstain",
              valueValue = 2,
              valueSourceLoc =
              "manual_examples.h:152:3"}],
          enumSourceLoc =
          "manual_examples.h:149:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Vote",
        structConstr = HsName
          "@NsConstr"
          "Vote",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Vote",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "vote")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumValues = [
              EnumValue {
                valueName = CName "infavour",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:150:3"},
              EnumValue {
                valueName = CName "against",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:151:3"},
              EnumValue {
                valueName = CName "abstain",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:152:3"}],
            enumSourceLoc =
            "manual_examples.h:149:6"}}
      StorableInstance {
        storableSizeOf = 1,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Vote",
                structConstr = HsName
                  "@NsConstr"
                  "Vote",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Vote",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "vote")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimChar
                        (PrimSignExplicit Unsigned)),
                    enumSizeof = 1,
                    enumAlignment = 1,
                    enumValues = [
                      EnumValue {
                        valueName = CName "infavour",
                        valueValue = 0,
                        valueSourceLoc =
                        "manual_examples.h:150:3"},
                      EnumValue {
                        valueName = CName "against",
                        valueValue = 1,
                        valueSourceLoc =
                        "manual_examples.h:151:3"},
                      EnumValue {
                        valueName = CName "abstain",
                        valueValue = 2,
                        valueSourceLoc =
                        "manual_examples.h:152:3"}],
                    enumSourceLoc =
                    "manual_examples.h:149:6"}})
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
                  "Vote",
                structConstr = HsName
                  "@NsConstr"
                  "Vote",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Vote",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "vote")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimChar
                        (PrimSignExplicit Unsigned)),
                    enumSizeof = 1,
                    enumAlignment = 1,
                    enumValues = [
                      EnumValue {
                        valueName = CName "infavour",
                        valueValue = 0,
                        valueSourceLoc =
                        "manual_examples.h:150:3"},
                      EnumValue {
                        valueName = CName "against",
                        valueValue = 1,
                        valueSourceLoc =
                        "manual_examples.h:151:3"},
                      EnumValue {
                        valueName = CName "abstain",
                        valueValue = 2,
                        valueSourceLoc =
                        "manual_examples.h:152:3"}],
                    enumSourceLoc =
                    "manual_examples.h:149:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Vote"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Vote"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Vote"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Vote",
        structConstr = HsName
          "@NsConstr"
          "Vote",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Vote",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "vote")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumValues = [
              EnumValue {
                valueName = CName "infavour",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:150:3"},
              EnumValue {
                valueName = CName "against",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:151:3"},
              EnumValue {
                valueName = CName "abstain",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:152:3"}],
            enumSourceLoc =
            "manual_examples.h:149:6"}}
      (HsPrimType HsPrimCUChar)
      (Map.fromList
        [
          _×_
            0
            (NE.fromList ["Infavour"]),
          _×_ 1 (NE.fromList ["Against"]),
          _×_
            2
            (NE.fromList ["Abstain"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Vote",
        structConstr = HsName
          "@NsConstr"
          "Vote",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Vote",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "vote")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumValues = [
              EnumValue {
                valueName = CName "infavour",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:150:3"},
              EnumValue {
                valueName = CName "against",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:151:3"},
              EnumValue {
                valueName = CName "abstain",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:152:3"}],
            enumSourceLoc =
            "manual_examples.h:149:6"}}
      (HsName "@NsConstr" "Infavour")
      (HsName "@NsConstr" "Abstain")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Vote",
        structConstr = HsName
          "@NsConstr"
          "Vote",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Vote",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "vote")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumValues = [
              EnumValue {
                valueName = CName "infavour",
                valueValue = 0,
                valueSourceLoc =
                "manual_examples.h:150:3"},
              EnumValue {
                valueName = CName "against",
                valueValue = 1,
                valueSourceLoc =
                "manual_examples.h:151:3"},
              EnumValue {
                valueName = CName "abstain",
                valueValue = 2,
                valueSourceLoc =
                "manual_examples.h:152:3"}],
            enumSourceLoc =
            "manual_examples.h:149:6"}}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Infavour",
      patSynType = HsName
        "@NsTypeConstr"
        "Vote",
      patSynConstr = HsName
        "@NsConstr"
        "Vote",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "infavour",
          valueValue = 0,
          valueSourceLoc =
          "manual_examples.h:150:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Against",
      patSynType = HsName
        "@NsTypeConstr"
        "Vote",
      patSynConstr = HsName
        "@NsConstr"
        "Vote",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "against",
          valueValue = 1,
          valueSourceLoc =
          "manual_examples.h:151:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Abstain",
      patSynType = HsName
        "@NsTypeConstr"
        "Vote",
      patSynConstr = HsName
        "@NsConstr"
        "Vote",
      patSynValue = 2,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "abstain",
          valueValue = 2,
          valueSourceLoc =
          "manual_examples.h:152:3"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      newtypeConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_CXCursorKind",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "CXCursorKind")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName
                "CXCursor_FirstExpr",
              valueValue = 100,
              valueSourceLoc =
              "manual_examples.h:158:3"},
            EnumValue {
              valueName = CName
                "CXCursor_UnexposedExpr",
              valueValue = 100,
              valueSourceLoc =
              "manual_examples.h:159:3"},
            EnumValue {
              valueName = CName
                "CXCursor_DeclRefExpr",
              valueValue = 101,
              valueSourceLoc =
              "manual_examples.h:160:3"},
            EnumValue {
              valueName = CName
                "CXCursor_MemberRefExpr",
              valueValue = 102,
              valueSourceLoc =
              "manual_examples.h:161:3"},
            EnumValue {
              valueName = CName
                "CXCursor_PackIndexingExpr",
              valueValue = 156,
              valueSourceLoc =
              "manual_examples.h:163:3"},
            EnumValue {
              valueName = CName
                "CXCursor_LastExpr",
              valueValue = 156,
              valueSourceLoc =
              "manual_examples.h:164:3"},
            EnumValue {
              valueName = CName
                "CXCursor_FirstStmt",
              valueValue = 200,
              valueSourceLoc =
              "manual_examples.h:166:3"},
            EnumValue {
              valueName = CName
                "CXCursor_UnexposedStmt",
              valueValue = 200,
              valueSourceLoc =
              "manual_examples.h:167:3"},
            EnumValue {
              valueName = CName
                "CXCursor_LabelStmt",
              valueValue = 201,
              valueSourceLoc =
              "manual_examples.h:168:3"},
            EnumValue {
              valueName = CName
                "CXCursor_CompoundStmt",
              valueValue = 202,
              valueSourceLoc =
              "manual_examples.h:169:3"},
            EnumValue {
              valueName = CName
                "CXCursor_OpenACCUpdateConstruct",
              valueValue = 331,
              valueSourceLoc =
              "manual_examples.h:171:3"},
            EnumValue {
              valueName = CName
                "CXCursor_LastStmt",
              valueValue = 331,
              valueSourceLoc =
              "manual_examples.h:172:3"}],
          enumSourceLoc =
          "manual_examples.h:157:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "CXCursorKind",
        structConstr = HsName
          "@NsConstr"
          "CXCursorKind",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_CXCursorKind",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "CXCursorKind")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName
                  "CXCursor_FirstExpr",
                valueValue = 100,
                valueSourceLoc =
                "manual_examples.h:158:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_UnexposedExpr",
                valueValue = 100,
                valueSourceLoc =
                "manual_examples.h:159:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_DeclRefExpr",
                valueValue = 101,
                valueSourceLoc =
                "manual_examples.h:160:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_MemberRefExpr",
                valueValue = 102,
                valueSourceLoc =
                "manual_examples.h:161:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_PackIndexingExpr",
                valueValue = 156,
                valueSourceLoc =
                "manual_examples.h:163:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_LastExpr",
                valueValue = 156,
                valueSourceLoc =
                "manual_examples.h:164:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_FirstStmt",
                valueValue = 200,
                valueSourceLoc =
                "manual_examples.h:166:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_UnexposedStmt",
                valueValue = 200,
                valueSourceLoc =
                "manual_examples.h:167:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_LabelStmt",
                valueValue = 201,
                valueSourceLoc =
                "manual_examples.h:168:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_CompoundStmt",
                valueValue = 202,
                valueSourceLoc =
                "manual_examples.h:169:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_OpenACCUpdateConstruct",
                valueValue = 331,
                valueSourceLoc =
                "manual_examples.h:171:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_LastStmt",
                valueValue = 331,
                valueSourceLoc =
                "manual_examples.h:172:3"}],
            enumSourceLoc =
            "manual_examples.h:157:6"}}
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
                  "CXCursorKind",
                structConstr = HsName
                  "@NsConstr"
                  "CXCursorKind",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_CXCursorKind",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "CXCursorKind")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName
                          "CXCursor_FirstExpr",
                        valueValue = 100,
                        valueSourceLoc =
                        "manual_examples.h:158:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_UnexposedExpr",
                        valueValue = 100,
                        valueSourceLoc =
                        "manual_examples.h:159:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_DeclRefExpr",
                        valueValue = 101,
                        valueSourceLoc =
                        "manual_examples.h:160:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_MemberRefExpr",
                        valueValue = 102,
                        valueSourceLoc =
                        "manual_examples.h:161:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_PackIndexingExpr",
                        valueValue = 156,
                        valueSourceLoc =
                        "manual_examples.h:163:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_LastExpr",
                        valueValue = 156,
                        valueSourceLoc =
                        "manual_examples.h:164:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_FirstStmt",
                        valueValue = 200,
                        valueSourceLoc =
                        "manual_examples.h:166:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_UnexposedStmt",
                        valueValue = 200,
                        valueSourceLoc =
                        "manual_examples.h:167:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_LabelStmt",
                        valueValue = 201,
                        valueSourceLoc =
                        "manual_examples.h:168:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_CompoundStmt",
                        valueValue = 202,
                        valueSourceLoc =
                        "manual_examples.h:169:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_OpenACCUpdateConstruct",
                        valueValue = 331,
                        valueSourceLoc =
                        "manual_examples.h:171:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_LastStmt",
                        valueValue = 331,
                        valueSourceLoc =
                        "manual_examples.h:172:3"}],
                    enumSourceLoc =
                    "manual_examples.h:157:6"}})
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
                  "CXCursorKind",
                structConstr = HsName
                  "@NsConstr"
                  "CXCursorKind",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_CXCursorKind",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "CXCursorKind")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName
                          "CXCursor_FirstExpr",
                        valueValue = 100,
                        valueSourceLoc =
                        "manual_examples.h:158:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_UnexposedExpr",
                        valueValue = 100,
                        valueSourceLoc =
                        "manual_examples.h:159:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_DeclRefExpr",
                        valueValue = 101,
                        valueSourceLoc =
                        "manual_examples.h:160:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_MemberRefExpr",
                        valueValue = 102,
                        valueSourceLoc =
                        "manual_examples.h:161:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_PackIndexingExpr",
                        valueValue = 156,
                        valueSourceLoc =
                        "manual_examples.h:163:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_LastExpr",
                        valueValue = 156,
                        valueSourceLoc =
                        "manual_examples.h:164:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_FirstStmt",
                        valueValue = 200,
                        valueSourceLoc =
                        "manual_examples.h:166:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_UnexposedStmt",
                        valueValue = 200,
                        valueSourceLoc =
                        "manual_examples.h:167:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_LabelStmt",
                        valueValue = 201,
                        valueSourceLoc =
                        "manual_examples.h:168:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_CompoundStmt",
                        valueValue = 202,
                        valueSourceLoc =
                        "manual_examples.h:169:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_OpenACCUpdateConstruct",
                        valueValue = 331,
                        valueSourceLoc =
                        "manual_examples.h:171:3"},
                      EnumValue {
                        valueName = CName
                          "CXCursor_LastStmt",
                        valueValue = 331,
                        valueSourceLoc =
                        "manual_examples.h:172:3"}],
                    enumSourceLoc =
                    "manual_examples.h:157:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "CXCursorKind"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "CXCursorKind"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "CXCursorKind"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "CXCursorKind",
        structConstr = HsName
          "@NsConstr"
          "CXCursorKind",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_CXCursorKind",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "CXCursorKind")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName
                  "CXCursor_FirstExpr",
                valueValue = 100,
                valueSourceLoc =
                "manual_examples.h:158:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_UnexposedExpr",
                valueValue = 100,
                valueSourceLoc =
                "manual_examples.h:159:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_DeclRefExpr",
                valueValue = 101,
                valueSourceLoc =
                "manual_examples.h:160:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_MemberRefExpr",
                valueValue = 102,
                valueSourceLoc =
                "manual_examples.h:161:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_PackIndexingExpr",
                valueValue = 156,
                valueSourceLoc =
                "manual_examples.h:163:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_LastExpr",
                valueValue = 156,
                valueSourceLoc =
                "manual_examples.h:164:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_FirstStmt",
                valueValue = 200,
                valueSourceLoc =
                "manual_examples.h:166:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_UnexposedStmt",
                valueValue = 200,
                valueSourceLoc =
                "manual_examples.h:167:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_LabelStmt",
                valueValue = 201,
                valueSourceLoc =
                "manual_examples.h:168:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_CompoundStmt",
                valueValue = 202,
                valueSourceLoc =
                "manual_examples.h:169:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_OpenACCUpdateConstruct",
                valueValue = 331,
                valueSourceLoc =
                "manual_examples.h:171:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_LastStmt",
                valueValue = 331,
                valueSourceLoc =
                "manual_examples.h:172:3"}],
            enumSourceLoc =
            "manual_examples.h:157:6"}}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_
            100
            (NE.fromList
              [
                "CXCursor_FirstExpr",
                "CXCursor_UnexposedExpr"]),
          _×_
            101
            (NE.fromList
              ["CXCursor_DeclRefExpr"]),
          _×_
            102
            (NE.fromList
              ["CXCursor_MemberRefExpr"]),
          _×_
            156
            (NE.fromList
              [
                "CXCursor_PackIndexingExpr",
                "CXCursor_LastExpr"]),
          _×_
            200
            (NE.fromList
              [
                "CXCursor_FirstStmt",
                "CXCursor_UnexposedStmt"]),
          _×_
            201
            (NE.fromList
              ["CXCursor_LabelStmt"]),
          _×_
            202
            (NE.fromList
              ["CXCursor_CompoundStmt"]),
          _×_
            331
            (NE.fromList
              [
                "CXCursor_OpenACCUpdateConstruct",
                "CXCursor_LastStmt"])])
      False),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "CXCursorKind",
        structConstr = HsName
          "@NsConstr"
          "CXCursorKind",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_CXCursorKind",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "CXCursorKind")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName
                  "CXCursor_FirstExpr",
                valueValue = 100,
                valueSourceLoc =
                "manual_examples.h:158:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_UnexposedExpr",
                valueValue = 100,
                valueSourceLoc =
                "manual_examples.h:159:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_DeclRefExpr",
                valueValue = 101,
                valueSourceLoc =
                "manual_examples.h:160:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_MemberRefExpr",
                valueValue = 102,
                valueSourceLoc =
                "manual_examples.h:161:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_PackIndexingExpr",
                valueValue = 156,
                valueSourceLoc =
                "manual_examples.h:163:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_LastExpr",
                valueValue = 156,
                valueSourceLoc =
                "manual_examples.h:164:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_FirstStmt",
                valueValue = 200,
                valueSourceLoc =
                "manual_examples.h:166:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_UnexposedStmt",
                valueValue = 200,
                valueSourceLoc =
                "manual_examples.h:167:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_LabelStmt",
                valueValue = 201,
                valueSourceLoc =
                "manual_examples.h:168:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_CompoundStmt",
                valueValue = 202,
                valueSourceLoc =
                "manual_examples.h:169:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_OpenACCUpdateConstruct",
                valueValue = 331,
                valueSourceLoc =
                "manual_examples.h:171:3"},
              EnumValue {
                valueName = CName
                  "CXCursor_LastStmt",
                valueValue = 331,
                valueSourceLoc =
                "manual_examples.h:172:3"}],
            enumSourceLoc =
            "manual_examples.h:157:6"}}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_FirstExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 100,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_FirstExpr",
          valueValue = 100,
          valueSourceLoc =
          "manual_examples.h:158:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_UnexposedExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 100,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_UnexposedExpr",
          valueValue = 100,
          valueSourceLoc =
          "manual_examples.h:159:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_DeclRefExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 101,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_DeclRefExpr",
          valueValue = 101,
          valueSourceLoc =
          "manual_examples.h:160:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_MemberRefExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 102,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_MemberRefExpr",
          valueValue = 102,
          valueSourceLoc =
          "manual_examples.h:161:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_PackIndexingExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 156,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_PackIndexingExpr",
          valueValue = 156,
          valueSourceLoc =
          "manual_examples.h:163:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_LastExpr",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 156,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_LastExpr",
          valueValue = 156,
          valueSourceLoc =
          "manual_examples.h:164:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_FirstStmt",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 200,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_FirstStmt",
          valueValue = 200,
          valueSourceLoc =
          "manual_examples.h:166:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_UnexposedStmt",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 200,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_UnexposedStmt",
          valueValue = 200,
          valueSourceLoc =
          "manual_examples.h:167:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_LabelStmt",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 201,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_LabelStmt",
          valueValue = 201,
          valueSourceLoc =
          "manual_examples.h:168:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_CompoundStmt",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 202,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_CompoundStmt",
          valueValue = 202,
          valueSourceLoc =
          "manual_examples.h:169:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_OpenACCUpdateConstruct",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 331,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_OpenACCUpdateConstruct",
          valueValue = 331,
          valueSourceLoc =
          "manual_examples.h:171:3"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CXCursor_LastStmt",
      patSynType = HsName
        "@NsTypeConstr"
        "CXCursorKind",
      patSynConstr = HsName
        "@NsConstr"
        "CXCursorKind",
      patSynValue = 331,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "CXCursor_LastStmt",
          valueValue = 331,
          valueSourceLoc =
          "manual_examples.h:172:3"}}]
