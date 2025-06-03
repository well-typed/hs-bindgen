[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Thing",
      structConstr = HsName
        "@NsConstr"
        "Thing",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "thing_x",
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
              "struct_arg.h:3:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "thing"),
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "struct_arg.h:3:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "struct_arg.h:2:8"},
      structInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Thing",
        structConstr = HsName
          "@NsConstr"
          "Thing",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "thing_x",
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
                "struct_arg.h:3:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "thing"),
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "struct_arg.h:3:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "struct_arg.h:2:8"},
        structInstances = Set.fromList
          [Eq, Show, Storable]}
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
                  "Thing",
                structConstr = HsName
                  "@NsConstr"
                  "Thing",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "thing_x",
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
                        "struct_arg.h:3:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "thing"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "struct_arg.h:3:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "struct_arg.h:2:8"},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
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
                  "Thing",
                structConstr = HsName
                  "@NsConstr"
                  "Thing",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "thing_x",
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
                        "struct_arg.h:3:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "thing"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "struct_arg.h:3:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "struct_arg.h:2:8"},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
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
      "Thing"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Thing"),
  DeclInlineCInclude
    "struct_arg.h",
  DeclInlineC
    "signed int testmodule_thing_fun_1 (struct thing *arg1) { return thing_fun_1(*arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_1",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Thing")))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "testmodule_thing_fun_1",
      foreignImportHeader =
      "struct_arg.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "thing_fun_1",
          functionArgs = [
            TypeStruct
              (DeclPathName (CName "thing"))],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed),
          functionHeader = "struct_arg.h",
          functionSourceLoc =
          "struct_arg.h:6:5"}},
  DeclInlineCInclude
    "struct_arg.h",
  DeclInlineC
    "void testmodule_thing_fun_2 (signed int arg1, struct thing *arg2) { *arg2 = thing_fun_2(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_2",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")))
          (HsIO (HsPrimType HsPrimUnit))),
      foreignImportOrigName =
      "testmodule_thing_fun_2",
      foreignImportHeader =
      "struct_arg.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "thing_fun_2",
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionRes = TypeStruct
            (DeclPathName (CName "thing")),
          functionHeader = "struct_arg.h",
          functionSourceLoc =
          "struct_arg.h:7:14"}}]
