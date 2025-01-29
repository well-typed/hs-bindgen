[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "func",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName "@NsTypeConstr" "Bar")))
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName = "func",
      foreignImportHeader =
      "weird01.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "func",
          functionType = TypeFun
            [
              TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "bar"))
                    DeclPathTop))]
            (TypePrim PrimVoid),
          functionHeader = "weird01.h",
          functionSourceLoc =
          "examples/weird01.h:8:6"}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Foo",
      structConstr = HsName
        "@NsConstr"
        "Foo",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_z",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "z",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/weird01.h:2:13"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "foo"))
            DeclPathTop,
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "z",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/weird01.h:2:13"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/weird01.h:1:8",
          structBitfields = []}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Foo",
        structConstr = HsName
          "@NsConstr"
          "Foo",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_z",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "z",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/weird01.h:2:13"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "foo"))
              DeclPathTop,
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "z",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/weird01.h:2:13"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/weird01.h:1:8",
            structBitfields = []}}
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_z",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/weird01.h:2:13"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/weird01.h:2:13"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/weird01.h:1:8",
                    structBitfields = []}})
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_z",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/weird01.h:2:13"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/weird01.h:2:13"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/weird01.h:1:8",
                    structBitfields = []}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/weird01.h:4:21"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "bar"))
            DeclPathTop,
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/weird01.h:4:21"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/weird01.h:3:16",
          structBitfields = []}},
  DeclInstance
    (InstanceStorable
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/weird01.h:4:21"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "bar"))
              DeclPathTop,
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/weird01.h:4:21"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/weird01.h:3:16",
            structBitfields = []}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/weird01.h:4:21"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/weird01.h:4:21"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/weird01.h:3:16",
                    structBitfields = []}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/weird01.h:4:21"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/weird01.h:4:21"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/weird01.h:3:16",
                    structBitfields = []}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))})]
