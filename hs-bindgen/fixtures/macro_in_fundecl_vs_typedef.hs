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
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_in_fundecl_vs_typedef.h:4:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "MC",
          macroArgs = [],
          macroBody = MTerm
            (MType
              (TypePrim
                (PrimChar
                  (PrimSignImplicit Nothing))))}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "MC"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "MC"),
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "quux1",
      foreignImportType = HsFun
        (HsTypRef
          (HsName "@NsTypeConstr" "MC"))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "TC"))
          (HsIO
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName = "quux1",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "quux1",
          functionArgs = [
            TypeTypedef (CName "MC"),
            TypeTypedef (CName "TC")],
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit Nothing)),
          functionHeader =
          "macro_in_fundecl_vs_typedef.h",
          functionSourceLoc =
          "macro_in_fundecl_vs_typedef.h:8:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "quux2",
      foreignImportType = HsFun
        (HsTypRef
          (HsName "@NsTypeConstr" "MC"))
        (HsFun
          (HsPrimType HsPrimCChar)
          (HsIO
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "TC")))),
      foreignImportOrigName = "quux2",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "quux2",
          functionArgs = [
            TypeTypedef (CName "MC"),
            TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))],
          functionRes = TypeTypedef
            (CName "TC"),
          functionHeader =
          "macro_in_fundecl_vs_typedef.h",
          functionSourceLoc =
          "macro_in_fundecl_vs_typedef.h:9:4"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "wam1",
      foreignImportType = HsFun
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
                  "MC"))))),
      foreignImportOrigName = "wam1",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "wam1",
          functionArgs = [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeTypedef (CName "TC"))],
          functionRes = TypePointer
            (TypeTypedef (CName "MC")),
          functionHeader =
          "macro_in_fundecl_vs_typedef.h",
          functionSourceLoc =
          "macro_in_fundecl_vs_typedef.h:10:5"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "wam2",
      foreignImportType = HsFun
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
                  "TC"))))),
      foreignImportOrigName = "wam2",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "wam2",
          functionArgs = [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeTypedef (CName "MC"))],
          functionRes = TypePointer
            (TypeTypedef (CName "TC")),
          functionHeader =
          "macro_in_fundecl_vs_typedef.h",
          functionSourceLoc =
          "macro_in_fundecl_vs_typedef.h:11:5"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef1",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct2")))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "MC"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "struct_typedef1",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "struct_typedef1",
          functionArgs = [
            TypePointer
              (TypeTypedef (CName "struct2")),
            TypeTypedef (CName "MC")],
          functionRes = TypeVoid,
          functionHeader =
          "macro_in_fundecl_vs_typedef.h",
          functionSourceLoc =
          "macro_in_fundecl_vs_typedef.h:23:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef2",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct3_t")))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "MC"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "struct_typedef2",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "struct_typedef2",
          functionArgs = [
            TypePointer
              (TypeTypedef
                (CName "struct3_t")),
            TypeTypedef (CName "MC")],
          functionRes = TypeVoid,
          functionHeader =
          "macro_in_fundecl_vs_typedef.h",
          functionSourceLoc =
          "macro_in_fundecl_vs_typedef.h:24:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "struct_typedef3",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct4")))
        (HsFun
          (HsTypRef
            (HsName "@NsTypeConstr" "MC"))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "struct_typedef3",
      foreignImportHeader =
      "macro_in_fundecl_vs_typedef.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "struct_typedef3",
          functionArgs = [
            TypePointer
              (TypeTypedef (CName "struct4")),
            TypeTypedef (CName "MC")],
          functionRes = TypeVoid,
          functionHeader =
          "macro_in_fundecl_vs_typedef.h",
          functionSourceLoc =
          "macro_in_fundecl_vs_typedef.h:25:6"}},
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
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "TC",
          typedefType = TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed))),
          typedefSourceLoc =
          "macro_in_fundecl_vs_typedef.h:5:14"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "TC"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "TC"),
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "macro_in_fundecl_vs_typedef.h:18:30"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "struct1")
            DeclPathCtxtTop,
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "macro_in_fundecl_vs_typedef.h:18:30"}],
          structFlam = Nothing,
          structSourceLoc =
          "macro_in_fundecl_vs_typedef.h:18:16"}},
  DeclInstance
    (InstanceStorable
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "macro_in_fundecl_vs_typedef.h:18:30"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "struct1")
              DeclPathCtxtTop,
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "macro_in_fundecl_vs_typedef.h:18:30"}],
            structFlam = Nothing,
            structSourceLoc =
            "macro_in_fundecl_vs_typedef.h:18:16"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:18:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "struct1")
                      DeclPathCtxtTop,
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:18:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "macro_in_fundecl_vs_typedef.h:18:16"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:18:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "struct1")
                      DeclPathCtxtTop,
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:18:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "macro_in_fundecl_vs_typedef.h:18:16"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct1"),
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "macro_in_fundecl_vs_typedef.h:19:30"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathAnon
            (DeclPathCtxtTypedef
              (CName "struct2")),
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "macro_in_fundecl_vs_typedef.h:19:30"}],
          structFlam = Nothing,
          structSourceLoc =
          "macro_in_fundecl_vs_typedef.h:19:9"}},
  DeclInstance
    (InstanceStorable
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "macro_in_fundecl_vs_typedef.h:19:30"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathAnon
              (DeclPathCtxtTypedef
                (CName "struct2")),
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "macro_in_fundecl_vs_typedef.h:19:30"}],
            structFlam = Nothing,
            structSourceLoc =
            "macro_in_fundecl_vs_typedef.h:19:9"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:19:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtTypedef
                        (CName "struct2")),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:19:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "macro_in_fundecl_vs_typedef.h:19:9"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:19:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtTypedef
                        (CName "struct2")),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:19:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "macro_in_fundecl_vs_typedef.h:19:9"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct2"),
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "macro_in_fundecl_vs_typedef.h:20:30"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "struct3")
            DeclPathCtxtTop,
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "macro_in_fundecl_vs_typedef.h:20:30"}],
          structFlam = Nothing,
          structSourceLoc =
          "macro_in_fundecl_vs_typedef.h:20:16"}},
  DeclInstance
    (InstanceStorable
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "macro_in_fundecl_vs_typedef.h:20:30"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "struct3")
              DeclPathCtxtTop,
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "macro_in_fundecl_vs_typedef.h:20:30"}],
            structFlam = Nothing,
            structSourceLoc =
            "macro_in_fundecl_vs_typedef.h:20:16"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:20:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "struct3")
                      DeclPathCtxtTop,
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:20:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "macro_in_fundecl_vs_typedef.h:20:16"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:20:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "struct3")
                      DeclPathCtxtTop,
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:20:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "macro_in_fundecl_vs_typedef.h:20:16"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct3"),
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
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "struct3_t",
          typedefType = TypeStruct
            (DeclPathName
              (CName "struct3")
              DeclPathCtxtTop),
          typedefSourceLoc =
          "macro_in_fundecl_vs_typedef.h:20:35"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Struct3_t"),
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "macro_in_fundecl_vs_typedef.h:21:30"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "struct4")
            DeclPathCtxtTop,
          structAliases = [
            CName "struct4"],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "macro_in_fundecl_vs_typedef.h:21:30"}],
          structFlam = Nothing,
          structSourceLoc =
          "macro_in_fundecl_vs_typedef.h:21:16"}},
  DeclInstance
    (InstanceStorable
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "macro_in_fundecl_vs_typedef.h:21:30"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "struct4")
              DeclPathCtxtTop,
            structAliases = [
              CName "struct4"],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "macro_in_fundecl_vs_typedef.h:21:30"}],
            structFlam = Nothing,
            structSourceLoc =
            "macro_in_fundecl_vs_typedef.h:21:16"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:21:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "struct4")
                      DeclPathCtxtTop,
                    structAliases = [
                      CName "struct4"],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:21:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "macro_in_fundecl_vs_typedef.h:21:16"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:21:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "struct4")
                      DeclPathCtxtTop,
                    structAliases = [
                      CName "struct4"],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "macro_in_fundecl_vs_typedef.h:21:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "macro_in_fundecl_vs_typedef.h:21:16"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Struct4"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Struct4")]
