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
          "un_Triple",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "triple",
          typedefType = TypeConstArray
            Size {
              size = 3,
              sizeExpression = MTerm
                (MInt
                  IntegerLiteral {
                    integerLiteralText = "3",
                    integerLiteralType = Size,
                    integerLiteralValue = 3})}
            (TypePrim
              (PrimIntegral PrimInt Signed)),
          typedefSourceLoc =
          "fixedarray.h:1:13"},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclNewtypeInstance
    DeriveStock
    Show
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
                Size {
                  size = 3,
                  sizeExpression = MTerm
                    (MInt
                      IntegerLiteral {
                        integerLiteralText = "3",
                        integerLiteralType = Size,
                        integerLiteralValue = 3})}
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
                Size {
                  size = 3,
                  sizeExpression = MTerm
                    (MInt
                      IntegerLiteral {
                        integerLiteralText = "3",
                        integerLiteralType = Size,
                        integerLiteralValue = 3})}
                (TypeConstArray
                  Size {
                    size = 3,
                    sizeExpression = MTerm
                      (MInt
                        IntegerLiteral {
                          integerLiteralText = "3",
                          integerLiteralType = Size,
                          integerLiteralValue = 3})}
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              fieldSourceLoc =
              "fixedarray.h:5:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "Example"),
          structAliases = [],
          structSizeof = 48,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "triple",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                Size {
                  size = 3,
                  sizeExpression = MTerm
                    (MInt
                      IntegerLiteral {
                        integerLiteralText = "3",
                        integerLiteralType = Size,
                        integerLiteralValue = 3})}
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              fieldSourceLoc =
              "fixedarray.h:4:9"},
            StructField {
              fieldName = CName "sudoku",
              fieldOffset = 96,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                Size {
                  size = 3,
                  sizeExpression = MTerm
                    (MInt
                      IntegerLiteral {
                        integerLiteralText = "3",
                        integerLiteralType = Size,
                        integerLiteralValue = 3})}
                (TypeConstArray
                  Size {
                    size = 3,
                    sizeExpression = MTerm
                      (MInt
                        IntegerLiteral {
                          integerLiteralText = "3",
                          integerLiteralType = Size,
                          integerLiteralValue = 3})}
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              fieldSourceLoc =
              "fixedarray.h:5:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "fixedarray.h:3:8"},
      structInstances = Set.fromList
        [Eq, Show, Storable]},
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
                  Size {
                    size = 3,
                    sizeExpression = MTerm
                      (MInt
                        IntegerLiteral {
                          integerLiteralText = "3",
                          integerLiteralType = Size,
                          integerLiteralValue = 3})}
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
                  Size {
                    size = 3,
                    sizeExpression = MTerm
                      (MInt
                        IntegerLiteral {
                          integerLiteralText = "3",
                          integerLiteralType = Size,
                          integerLiteralValue = 3})}
                  (TypeConstArray
                    Size {
                      size = 3,
                      sizeExpression = MTerm
                        (MInt
                          IntegerLiteral {
                            integerLiteralText = "3",
                            integerLiteralType = Size,
                            integerLiteralValue = 3})}
                    (TypePrim
                      (PrimIntegral PrimInt Signed))),
                fieldSourceLoc =
                "fixedarray.h:5:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "Example"),
            structAliases = [],
            structSizeof = 48,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "triple",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  Size {
                    size = 3,
                    sizeExpression = MTerm
                      (MInt
                        IntegerLiteral {
                          integerLiteralText = "3",
                          integerLiteralType = Size,
                          integerLiteralValue = 3})}
                  (TypePrim
                    (PrimIntegral PrimInt Signed)),
                fieldSourceLoc =
                "fixedarray.h:4:9"},
              StructField {
                fieldName = CName "sudoku",
                fieldOffset = 96,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  Size {
                    size = 3,
                    sizeExpression = MTerm
                      (MInt
                        IntegerLiteral {
                          integerLiteralText = "3",
                          integerLiteralType = Size,
                          integerLiteralValue = 3})}
                  (TypeConstArray
                    Size {
                      size = 3,
                      sizeExpression = MTerm
                        (MInt
                          IntegerLiteral {
                            integerLiteralText = "3",
                            integerLiteralType = Size,
                            integerLiteralValue = 3})}
                    (TypePrim
                      (PrimIntegral PrimInt Signed))),
                fieldSourceLoc =
                "fixedarray.h:5:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "fixedarray.h:3:8"},
        structInstances = Set.fromList
          [Eq, Show, Storable]}
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
                          Size {
                            size = 3,
                            sizeExpression = MTerm
                              (MInt
                                IntegerLiteral {
                                  integerLiteralText = "3",
                                  integerLiteralType = Size,
                                  integerLiteralValue = 3})}
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
                          Size {
                            size = 3,
                            sizeExpression = MTerm
                              (MInt
                                IntegerLiteral {
                                  integerLiteralText = "3",
                                  integerLiteralType = Size,
                                  integerLiteralValue = 3})}
                          (TypeConstArray
                            Size {
                              size = 3,
                              sizeExpression = MTerm
                                (MInt
                                  IntegerLiteral {
                                    integerLiteralText = "3",
                                    integerLiteralType = Size,
                                    integerLiteralValue = 3})}
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "fixedarray.h:5:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "Example"),
                    structAliases = [],
                    structSizeof = 48,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "triple",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          Size {
                            size = 3,
                            sizeExpression = MTerm
                              (MInt
                                IntegerLiteral {
                                  integerLiteralText = "3",
                                  integerLiteralType = Size,
                                  integerLiteralValue = 3})}
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "fixedarray.h:4:9"},
                      StructField {
                        fieldName = CName "sudoku",
                        fieldOffset = 96,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          Size {
                            size = 3,
                            sizeExpression = MTerm
                              (MInt
                                IntegerLiteral {
                                  integerLiteralText = "3",
                                  integerLiteralType = Size,
                                  integerLiteralValue = 3})}
                          (TypeConstArray
                            Size {
                              size = 3,
                              sizeExpression = MTerm
                                (MInt
                                  IntegerLiteral {
                                    integerLiteralText = "3",
                                    integerLiteralType = Size,
                                    integerLiteralValue = 3})}
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "fixedarray.h:5:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "fixedarray.h:3:8"},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
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
                          Size {
                            size = 3,
                            sizeExpression = MTerm
                              (MInt
                                IntegerLiteral {
                                  integerLiteralText = "3",
                                  integerLiteralType = Size,
                                  integerLiteralValue = 3})}
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
                          Size {
                            size = 3,
                            sizeExpression = MTerm
                              (MInt
                                IntegerLiteral {
                                  integerLiteralText = "3",
                                  integerLiteralType = Size,
                                  integerLiteralValue = 3})}
                          (TypeConstArray
                            Size {
                              size = 3,
                              sizeExpression = MTerm
                                (MInt
                                  IntegerLiteral {
                                    integerLiteralText = "3",
                                    integerLiteralType = Size,
                                    integerLiteralValue = 3})}
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "fixedarray.h:5:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "Example"),
                    structAliases = [],
                    structSizeof = 48,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "triple",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          Size {
                            size = 3,
                            sizeExpression = MTerm
                              (MInt
                                IntegerLiteral {
                                  integerLiteralText = "3",
                                  integerLiteralType = Size,
                                  integerLiteralValue = 3})}
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "fixedarray.h:4:9"},
                      StructField {
                        fieldName = CName "sudoku",
                        fieldOffset = 96,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          Size {
                            size = 3,
                            sizeExpression = MTerm
                              (MInt
                                IntegerLiteral {
                                  integerLiteralText = "3",
                                  integerLiteralType = Size,
                                  integerLiteralValue = 3})}
                          (TypeConstArray
                            Size {
                              size = 3,
                              sizeExpression = MTerm
                                (MInt
                                  IntegerLiteral {
                                    integerLiteralText = "3",
                                    integerLiteralType = Size,
                                    integerLiteralValue = 3})}
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "fixedarray.h:5:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "fixedarray.h:3:8"},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
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
