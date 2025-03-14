[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Triple",
      newtypeConstr = HsName
        "@NsConstr"
        "Triple",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unTriple",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "triple",
          typedefType = TypeConstArray
            3
            (TypePrim
              (PrimIntegral PrimInt Signed)),
          typedefSourceLoc =
          "fixedarray.h:1:13"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Example",
      structConstr = HsName
        "@NsConstr"
        "Example",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "example_triple",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCInt),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "triple",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              fieldSourceLoc =
              "fixedarray.h:4:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_sudoku",
          fieldType = HsConstArray
            3
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "sudoku",
              fieldOffset = 96,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                3
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              fieldSourceLoc =
              "fixedarray.h:5:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "Example"))
            DeclPathTop,
          structSizeof = 48,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "triple",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              fieldSourceLoc =
              "fixedarray.h:4:9"},
            StructField {
              fieldName = CName "sudoku",
              fieldOffset = 96,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                3
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              fieldSourceLoc =
              "fixedarray.h:5:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "fixedarray.h:3:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Example",
        structConstr = HsName
          "@NsConstr"
          "Example",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "example_triple",
            fieldType = HsConstArray
              3
              (HsPrimType HsPrimCInt),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "triple",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral PrimInt Signed)),
                fieldSourceLoc =
                "fixedarray.h:4:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "example_sudoku",
            fieldType = HsConstArray
              3
              (HsConstArray
                3
                (HsPrimType HsPrimCInt)),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "sudoku",
                fieldOffset = 96,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  3
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed))),
                fieldSourceLoc =
                "fixedarray.h:5:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "Example"))
              DeclPathTop,
            structSizeof = 48,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "triple",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral PrimInt Signed)),
                fieldSourceLoc =
                "fixedarray.h:4:9"},
              StructField {
                fieldName = CName "sudoku",
                fieldOffset = 96,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  3
                  (TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed))),
                fieldSourceLoc =
                "fixedarray.h:5:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "fixedarray.h:3:8"}}
      StorableInstance {
        storableSizeOf = 48,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Example",
                structConstr = HsName
                  "@NsConstr"
                  "Example",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "example_triple",
                    fieldType = HsConstArray
                      3
                      (HsPrimType HsPrimCInt),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "triple",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "fixedarray.h:4:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "example_sudoku",
                    fieldType = HsConstArray
                      3
                      (HsConstArray
                        3
                        (HsPrimType HsPrimCInt)),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sudoku",
                        fieldOffset = 96,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          3
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "fixedarray.h:5:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "Example"))
                      DeclPathTop,
                    structSizeof = 48,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "triple",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "fixedarray.h:4:9"},
                      StructField {
                        fieldName = CName "sudoku",
                        fieldOffset = 96,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          3
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "fixedarray.h:5:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "fixedarray.h:3:8"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 12]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Example",
                structConstr = HsName
                  "@NsConstr"
                  "Example",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "example_triple",
                    fieldType = HsConstArray
                      3
                      (HsPrimType HsPrimCInt),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "triple",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "fixedarray.h:4:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "example_sudoku",
                    fieldType = HsConstArray
                      3
                      (HsConstArray
                        3
                        (HsPrimType HsPrimCInt)),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sudoku",
                        fieldOffset = 96,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          3
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "fixedarray.h:5:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "Example"))
                      DeclPathTop,
                    structSizeof = 48,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "triple",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "fixedarray.h:4:9"},
                      StructField {
                        fieldName = CName "sudoku",
                        fieldOffset = 96,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          3
                          (TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "fixedarray.h:5:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "fixedarray.h:3:8"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    12
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Example"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Example")]
