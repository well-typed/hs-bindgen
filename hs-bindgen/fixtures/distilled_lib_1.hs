[
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              IntegralTyCon
              [TyVarTy (Idx 0)]],
          quantTyBody = TyVarTy (Idx 0)}},
      varDeclBody = VarDeclIntegral
        5
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "b",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              IntegralTyCon
              [TyVarTy (Idx 0)]],
          quantTyBody = TyVarTy (Idx 0)}},
      varDeclBody = VarDeclIntegral
        3
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "sOME_DEFINED_CONSTANT",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              IntegralTyCon
              [TyVarTy (Idx 0)]],
          quantTyBody = TyVarTy (Idx 0)}},
      varDeclBody = VarDeclIntegral
        4
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a_DEFINE_0",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              IntegralTyCon
              [TyVarTy (Idx 0)]],
          quantTyBody = TyVarTy (Idx 0)}},
      varDeclBody = VarDeclIntegral
        0
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a_DEFINE_1",
      varDeclType = ForallTy {
        forallTySize = Size 0,
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (TyConApp
              (IntLikeTyCon
                (PrimInt Unsigned))
              [])}},
      varDeclBody = VarDeclIntegral
        20560
        HsPrimCUInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a_DEFINE_2",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              IntegralTyCon
              [TyVarTy (Idx 0)]],
          quantTyBody = TyVarTy (Idx 0)}},
      varDeclBody = VarDeclIntegral
        2
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "tWO_ARGS",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              IntegralTyCon
              [TyVarTy (Idx 0)]],
          quantTyBody = TyVarTy (Idx 0)}},
      varDeclBody = VarDeclIntegral
        13398
        HsPrimCInt},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "some_fun",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "A_type_t")))
        (HsFun
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Uint32_t"))
          (HsFun
            (HsPrimType HsPrimVoid)
            (HsIO
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Int32_t"))))),
      foreignImportOrigName =
      "some_fun",
      foreignImportHeader =
      "distilled_lib_1.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "some_fun",
          functionType = TypeFun
            [
              TypePointer
                (TypeTypedef
                  (CName "a_type_t")),
              TypeTypedef (CName "uint32_t"),
              TypePrim PrimVoid]
            (TypeTypedef (CName "int32_t")),
          functionHeader =
          "distilled_lib_1.h",
          functionSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 71,
            singleLocColumn = 9}}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Another_typedef_struct_t",
      structConstr = HsName
        "@NsConstr"
        "Another_typedef_struct_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "another_typedef_struct_t_foo",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "foo",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 8,
                singleLocColumn = 22}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "another_typedef_struct_t_bar",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "bar",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 8,
                singleLocColumn = 32}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTypedef
              (CName
                "another_typedef_struct_t"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "foo",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 8,
                singleLocColumn = 22}},
            StructField {
              fieldName = CName "bar",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 8,
                singleLocColumn = 32}}],
          structFlam = Nothing,
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 8,
            singleLocColumn = 9}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Another_typedef_struct_t",
        structConstr = HsName
          "@NsConstr"
          "Another_typedef_struct_t",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "another_typedef_struct_t_foo",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "foo",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 8,
                  singleLocColumn = 22}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "another_typedef_struct_t_bar",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "bar",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 8,
                  singleLocColumn = 32}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTypedef
                (CName
                  "another_typedef_struct_t"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "foo",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 8,
                  singleLocColumn = 22}},
              StructField {
                fieldName = CName "bar",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 8,
                  singleLocColumn = 32}}],
            structFlam = Nothing,
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "distilled_lib_1.h"],
              singleLocLine = 8,
              singleLocColumn = 9}}}
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
                  "Another_typedef_struct_t",
                structConstr = HsName
                  "@NsConstr"
                  "Another_typedef_struct_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "another_typedef_struct_t_foo",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "foo",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 8,
                          singleLocColumn = 22}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "another_typedef_struct_t_bar",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "bar",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 8,
                          singleLocColumn = 32}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTypedef
                        (CName
                          "another_typedef_struct_t"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "foo",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 8,
                          singleLocColumn = 22}},
                      StructField {
                        fieldName = CName "bar",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 8,
                          singleLocColumn = 32}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "distilled_lib_1.h"],
                      singleLocLine = 8,
                      singleLocColumn = 9}}})
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
                  "Another_typedef_struct_t",
                structConstr = HsName
                  "@NsConstr"
                  "Another_typedef_struct_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "another_typedef_struct_t_foo",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "foo",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 8,
                          singleLocColumn = 22}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "another_typedef_struct_t_bar",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "bar",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 8,
                          singleLocColumn = 32}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTypedef
                        (CName
                          "another_typedef_struct_t"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "foo",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 8,
                          singleLocColumn = 22}},
                      StructField {
                        fieldName = CName "bar",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 8,
                          singleLocColumn = 32}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "distilled_lib_1.h"],
                      singleLocLine = 8,
                      singleLocColumn = 9}}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      newtypeConstr = HsName
        "@NsConstr"
        "Another_typedef_enum_e",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unAnother_typedef_enum_e",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName
            "another_typedef_enum_e",
          enumType = TypePrim
            (PrimIntegral
              (PrimInt Unsigned)),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "FOO",
              valueValue = 0,
              valueSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 9,
                singleLocColumn = 16}},
            EnumValue {
              valueName = CName "BAR",
              valueValue = 1,
              valueSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 9,
                singleLocColumn = 21}}],
          enumSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 9,
            singleLocColumn = 9}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Another_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "Another_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "unAnother_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName
              "another_typedef_enum_e",
            enumType = TypePrim
              (PrimIntegral
                (PrimInt Unsigned)),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "FOO",
                valueValue = 0,
                valueSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 9,
                  singleLocColumn = 16}},
              EnumValue {
                valueName = CName "BAR",
                valueValue = 1,
                valueSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 9,
                  singleLocColumn = 21}}],
            enumSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "distilled_lib_1.h"],
              singleLocLine = 9,
              singleLocColumn = 9}}}
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
                  "Another_typedef_enum_e",
                structConstr = HsName
                  "@NsConstr"
                  "Another_typedef_enum_e",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "unAnother_typedef_enum_e",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName
                      "another_typedef_enum_e",
                    enumType = TypePrim
                      (PrimIntegral
                        (PrimInt Unsigned)),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FOO",
                        valueValue = 0,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 9,
                          singleLocColumn = 16}},
                      EnumValue {
                        valueName = CName "BAR",
                        valueValue = 1,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 9,
                          singleLocColumn = 21}}],
                    enumSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "distilled_lib_1.h"],
                      singleLocLine = 9,
                      singleLocColumn = 9}}})
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
                  "Another_typedef_enum_e",
                structConstr = HsName
                  "@NsConstr"
                  "Another_typedef_enum_e",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "unAnother_typedef_enum_e",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName
                      "another_typedef_enum_e",
                    enumType = TypePrim
                      (PrimIntegral
                        (PrimInt Unsigned)),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FOO",
                        valueValue = 0,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 9,
                          singleLocColumn = 16}},
                      EnumValue {
                        valueName = CName "BAR",
                        valueValue = 1,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 9,
                          singleLocColumn = 21}}],
                    enumSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "distilled_lib_1.h"],
                      singleLocLine = 9,
                      singleLocColumn = 9}}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "FOO",
      patSynType = HsName
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "Another_typedef_enum_e",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "FOO",
          valueValue = 0,
          valueSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 9,
            singleLocColumn = 16}}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "BAR",
      patSynType = HsName
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "Another_typedef_enum_e",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "BAR",
          valueValue = 1,
          valueSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 9,
            singleLocColumn = 21}}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "A_type_t",
      newtypeConstr = HsName
        "@NsConstr"
        "A_type_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unA_type_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "a_type_t",
          typedefType = TypePrim
            (PrimIntegral (PrimInt Signed)),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 13,
            singleLocColumn = 13}}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Var_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Var_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unVar_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "var_t",
          typedefType = TypePrim
            (PrimIntegral (PrimInt Signed)),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 14,
            singleLocColumn = 13}}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Uint8_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Uint8_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unUint8_t",
        fieldType = HsPrimType
          HsPrimCSChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "uint8_t",
          typedefType = TypePrim
            (PrimChar (Just Unsigned)),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "musl-include",
              "bits",
              "alltypes.h"],
            singleLocLine = 121,
            singleLocColumn = 25}}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Uint16_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Uint16_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unUint16_t",
        fieldType = HsPrimType
          HsPrimCUShort,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "uint16_t",
          typedefType = TypePrim
            (PrimIntegral
              (PrimShort Unsigned)),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "musl-include",
              "bits",
              "alltypes.h"],
            singleLocLine = 126,
            singleLocColumn = 25}}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Uint32_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Uint32_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unUint32_t",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "uint32_t",
          typedefType = TypePrim
            (PrimIntegral
              (PrimInt Unsigned)),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "musl-include",
              "bits",
              "alltypes.h"],
            singleLocLine = 131,
            singleLocColumn = 25}}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "A_typedef_struct",
      structConstr = HsName
        "@NsConstr"
        "A_typedef_struct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_0",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_0",
              fieldOffset = 0,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 36,
                singleLocColumn = 31}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_1",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Uint8_t"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_1",
              fieldOffset = 8,
              fieldType = TypeTypedef
                (CName "uint8_t"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 37,
                singleLocColumn = 31}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_2",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Uint16_t"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_2",
              fieldOffset = 16,
              fieldType = TypeTypedef
                (CName "uint16_t"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 38,
                singleLocColumn = 31}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_3",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Uint32_t"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_3",
              fieldOffset = 32,
              fieldType = TypeTypedef
                (CName "uint32_t"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 39,
                singleLocColumn = 31}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_4",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Another_typedef_struct_t"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_4",
              fieldOffset = 64,
              fieldType = TypeStruct
                (DeclPathStruct
                  (DeclNameTypedef
                    (CName
                      "another_typedef_struct_t"))
                  DeclPathTop),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 40,
                singleLocColumn = 31}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_5",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Another_typedef_struct_t")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_5",
              fieldOffset = 128,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTypedef
                      (CName
                        "another_typedef_struct_t"))
                    DeclPathTop)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 41,
                singleLocColumn = 31}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_6",
          fieldType = HsPtr
            (HsPrimType HsPrimVoid),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_6",
              fieldOffset = 192,
              fieldType = TypePointer
                (TypePrim PrimVoid),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 42,
                singleLocColumn = 31}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_7",
          fieldType = HsConstArray
            7
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Uint32_t")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_7",
              fieldOffset = 256,
              fieldType = TypeConstArray
                7
                (TypeTypedef
                  (CName "uint32_t")),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 43,
                singleLocColumn = 31}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_8",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Another_typedef_enum_e"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_8",
              fieldOffset = 480,
              fieldType = TypeEnum
                (CName
                  "another_typedef_enum_e"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 44,
                singleLocColumn = 31}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_9",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Another_typedef_enum_e"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_9",
              fieldOffset = 512,
              fieldType = TypeTypedef
                (CName
                  "another_typedef_enum_e"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 45,
                singleLocColumn = 31}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_10",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Another_typedef_enum_e"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_10",
              fieldOffset = 640,
              fieldType = TypeTypedef
                (CName
                  "another_typedef_enum_e"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 46,
                singleLocColumn = 31}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag
              (CName "a_typedef_struct"))
            DeclPathTop,
          structSizeof = 140,
          structAlignment = 1,
          structFields = [
            StructField {
              fieldName = CName "field_0",
              fieldOffset = 0,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 36,
                singleLocColumn = 31}},
            StructField {
              fieldName = CName "field_1",
              fieldOffset = 8,
              fieldType = TypeTypedef
                (CName "uint8_t"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 37,
                singleLocColumn = 31}},
            StructField {
              fieldName = CName "field_2",
              fieldOffset = 16,
              fieldType = TypeTypedef
                (CName "uint16_t"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 38,
                singleLocColumn = 31}},
            StructField {
              fieldName = CName "field_3",
              fieldOffset = 32,
              fieldType = TypeTypedef
                (CName "uint32_t"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 39,
                singleLocColumn = 31}},
            StructField {
              fieldName = CName "field_4",
              fieldOffset = 64,
              fieldType = TypeStruct
                (DeclPathStruct
                  (DeclNameTypedef
                    (CName
                      "another_typedef_struct_t"))
                  DeclPathTop),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 40,
                singleLocColumn = 31}},
            StructField {
              fieldName = CName "field_5",
              fieldOffset = 128,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTypedef
                      (CName
                        "another_typedef_struct_t"))
                    DeclPathTop)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 41,
                singleLocColumn = 31}},
            StructField {
              fieldName = CName "field_6",
              fieldOffset = 192,
              fieldType = TypePointer
                (TypePrim PrimVoid),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 42,
                singleLocColumn = 31}},
            StructField {
              fieldName = CName "field_7",
              fieldOffset = 256,
              fieldType = TypeConstArray
                7
                (TypeTypedef
                  (CName "uint32_t")),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 43,
                singleLocColumn = 31}},
            StructField {
              fieldName = CName "field_8",
              fieldOffset = 480,
              fieldType = TypeEnum
                (CName
                  "another_typedef_enum_e"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 44,
                singleLocColumn = 31}},
            StructField {
              fieldName = CName "field_9",
              fieldOffset = 512,
              fieldType = TypeTypedef
                (CName
                  "another_typedef_enum_e"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 45,
                singleLocColumn = 31}},
            StructField {
              fieldName = CName "field_10",
              fieldOffset = 640,
              fieldType = TypeTypedef
                (CName
                  "another_typedef_enum_e"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 46,
                singleLocColumn = 31}}],
          structFlam = Nothing,
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 34,
            singleLocColumn = 16}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "A_typedef_struct",
        structConstr = HsName
          "@NsConstr"
          "A_typedef_struct",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_0",
            fieldType = HsPrimType
              HsPrimCBool,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_0",
                fieldOffset = 0,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 36,
                  singleLocColumn = 31}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_1",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Uint8_t"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_1",
                fieldOffset = 8,
                fieldType = TypeTypedef
                  (CName "uint8_t"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 37,
                  singleLocColumn = 31}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_2",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Uint16_t"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_2",
                fieldOffset = 16,
                fieldType = TypeTypedef
                  (CName "uint16_t"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 38,
                  singleLocColumn = 31}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_3",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Uint32_t"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_3",
                fieldOffset = 32,
                fieldType = TypeTypedef
                  (CName "uint32_t"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 39,
                  singleLocColumn = 31}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_4",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Another_typedef_struct_t"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_4",
                fieldOffset = 64,
                fieldType = TypeStruct
                  (DeclPathStruct
                    (DeclNameTypedef
                      (CName
                        "another_typedef_struct_t"))
                    DeclPathTop),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 40,
                  singleLocColumn = 31}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_5",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Another_typedef_struct_t")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_5",
                fieldOffset = 128,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTypedef
                        (CName
                          "another_typedef_struct_t"))
                      DeclPathTop)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 41,
                  singleLocColumn = 31}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_6",
            fieldType = HsPtr
              (HsPrimType HsPrimVoid),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_6",
                fieldOffset = 192,
                fieldType = TypePointer
                  (TypePrim PrimVoid),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 42,
                  singleLocColumn = 31}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_7",
            fieldType = HsConstArray
              7
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Uint32_t")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_7",
                fieldOffset = 256,
                fieldType = TypeConstArray
                  7
                  (TypeTypedef
                    (CName "uint32_t")),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 43,
                  singleLocColumn = 31}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_8",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Another_typedef_enum_e"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_8",
                fieldOffset = 480,
                fieldType = TypeEnum
                  (CName
                    "another_typedef_enum_e"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 44,
                  singleLocColumn = 31}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_9",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Another_typedef_enum_e"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_9",
                fieldOffset = 512,
                fieldType = TypeTypedef
                  (CName
                    "another_typedef_enum_e"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 45,
                  singleLocColumn = 31}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_10",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Another_typedef_enum_e"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_10",
                fieldOffset = 640,
                fieldType = TypeTypedef
                  (CName
                    "another_typedef_enum_e"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 46,
                  singleLocColumn = 31}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag
                (CName "a_typedef_struct"))
              DeclPathTop,
            structSizeof = 140,
            structAlignment = 1,
            structFields = [
              StructField {
                fieldName = CName "field_0",
                fieldOffset = 0,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 36,
                  singleLocColumn = 31}},
              StructField {
                fieldName = CName "field_1",
                fieldOffset = 8,
                fieldType = TypeTypedef
                  (CName "uint8_t"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 37,
                  singleLocColumn = 31}},
              StructField {
                fieldName = CName "field_2",
                fieldOffset = 16,
                fieldType = TypeTypedef
                  (CName "uint16_t"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 38,
                  singleLocColumn = 31}},
              StructField {
                fieldName = CName "field_3",
                fieldOffset = 32,
                fieldType = TypeTypedef
                  (CName "uint32_t"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 39,
                  singleLocColumn = 31}},
              StructField {
                fieldName = CName "field_4",
                fieldOffset = 64,
                fieldType = TypeStruct
                  (DeclPathStruct
                    (DeclNameTypedef
                      (CName
                        "another_typedef_struct_t"))
                    DeclPathTop),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 40,
                  singleLocColumn = 31}},
              StructField {
                fieldName = CName "field_5",
                fieldOffset = 128,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTypedef
                        (CName
                          "another_typedef_struct_t"))
                      DeclPathTop)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 41,
                  singleLocColumn = 31}},
              StructField {
                fieldName = CName "field_6",
                fieldOffset = 192,
                fieldType = TypePointer
                  (TypePrim PrimVoid),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 42,
                  singleLocColumn = 31}},
              StructField {
                fieldName = CName "field_7",
                fieldOffset = 256,
                fieldType = TypeConstArray
                  7
                  (TypeTypedef
                    (CName "uint32_t")),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 43,
                  singleLocColumn = 31}},
              StructField {
                fieldName = CName "field_8",
                fieldOffset = 480,
                fieldType = TypeEnum
                  (CName
                    "another_typedef_enum_e"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 44,
                  singleLocColumn = 31}},
              StructField {
                fieldName = CName "field_9",
                fieldOffset = 512,
                fieldType = TypeTypedef
                  (CName
                    "another_typedef_enum_e"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 45,
                  singleLocColumn = 31}},
              StructField {
                fieldName = CName "field_10",
                fieldOffset = 640,
                fieldType = TypeTypedef
                  (CName
                    "another_typedef_enum_e"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 46,
                  singleLocColumn = 31}}],
            structFlam = Nothing,
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "distilled_lib_1.h"],
              singleLocLine = 34,
              singleLocColumn = 16}}}
      StorableInstance {
        storableSizeOf = 140,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "A_typedef_struct",
                structConstr = HsName
                  "@NsConstr"
                  "A_typedef_struct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_0",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_0",
                        fieldOffset = 0,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 36,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Uint8_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_1",
                        fieldOffset = 8,
                        fieldType = TypeTypedef
                          (CName "uint8_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 37,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_2",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Uint16_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_2",
                        fieldOffset = 16,
                        fieldType = TypeTypedef
                          (CName "uint16_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 38,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_3",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Uint32_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_3",
                        fieldOffset = 32,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 39,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_4",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_struct_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_4",
                        fieldOffset = 64,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTypedef
                              (CName
                                "another_typedef_struct_t"))
                            DeclPathTop),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 40,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_5",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Another_typedef_struct_t")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_5",
                        fieldOffset = 128,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTypedef
                                (CName
                                  "another_typedef_struct_t"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 41,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_6",
                    fieldType = HsPtr
                      (HsPrimType HsPrimVoid),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_6",
                        fieldOffset = 192,
                        fieldType = TypePointer
                          (TypePrim PrimVoid),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 42,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_7",
                    fieldType = HsConstArray
                      7
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Uint32_t")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_7",
                        fieldOffset = 256,
                        fieldType = TypeConstArray
                          7
                          (TypeTypedef
                            (CName "uint32_t")),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 43,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_8",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_enum_e"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_8",
                        fieldOffset = 480,
                        fieldType = TypeEnum
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 44,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_9",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_enum_e"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_9",
                        fieldOffset = 512,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 45,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_10",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_enum_e"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_10",
                        fieldOffset = 640,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 46,
                          singleLocColumn = 31}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "a_typedef_struct"))
                      DeclPathTop,
                    structSizeof = 140,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "field_0",
                        fieldOffset = 0,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 36,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_1",
                        fieldOffset = 8,
                        fieldType = TypeTypedef
                          (CName "uint8_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 37,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_2",
                        fieldOffset = 16,
                        fieldType = TypeTypedef
                          (CName "uint16_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 38,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_3",
                        fieldOffset = 32,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 39,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_4",
                        fieldOffset = 64,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTypedef
                              (CName
                                "another_typedef_struct_t"))
                            DeclPathTop),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 40,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_5",
                        fieldOffset = 128,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTypedef
                                (CName
                                  "another_typedef_struct_t"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 41,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_6",
                        fieldOffset = 192,
                        fieldType = TypePointer
                          (TypePrim PrimVoid),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 42,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_7",
                        fieldOffset = 256,
                        fieldType = TypeConstArray
                          7
                          (TypeTypedef
                            (CName "uint32_t")),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 43,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_8",
                        fieldOffset = 480,
                        fieldType = TypeEnum
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 44,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_9",
                        fieldOffset = 512,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 45,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_10",
                        fieldOffset = 640,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 46,
                          singleLocColumn = 31}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "distilled_lib_1.h"],
                      singleLocLine = 34,
                      singleLocColumn = 16}}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 1,
              PeekByteOff (Idx 0) 2,
              PeekByteOff (Idx 0) 4,
              PeekByteOff (Idx 0) 8,
              PeekByteOff (Idx 0) 16,
              PeekByteOff (Idx 0) 24,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 60,
              PeekByteOff (Idx 0) 64,
              PeekByteOff (Idx 0) 80]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "A_typedef_struct",
                structConstr = HsName
                  "@NsConstr"
                  "A_typedef_struct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_0",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_0",
                        fieldOffset = 0,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 36,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Uint8_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_1",
                        fieldOffset = 8,
                        fieldType = TypeTypedef
                          (CName "uint8_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 37,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_2",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Uint16_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_2",
                        fieldOffset = 16,
                        fieldType = TypeTypedef
                          (CName "uint16_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 38,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_3",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Uint32_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_3",
                        fieldOffset = 32,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 39,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_4",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_struct_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_4",
                        fieldOffset = 64,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTypedef
                              (CName
                                "another_typedef_struct_t"))
                            DeclPathTop),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 40,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_5",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Another_typedef_struct_t")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_5",
                        fieldOffset = 128,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTypedef
                                (CName
                                  "another_typedef_struct_t"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 41,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_6",
                    fieldType = HsPtr
                      (HsPrimType HsPrimVoid),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_6",
                        fieldOffset = 192,
                        fieldType = TypePointer
                          (TypePrim PrimVoid),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 42,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_7",
                    fieldType = HsConstArray
                      7
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Uint32_t")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_7",
                        fieldOffset = 256,
                        fieldType = TypeConstArray
                          7
                          (TypeTypedef
                            (CName "uint32_t")),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 43,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_8",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_enum_e"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_8",
                        fieldOffset = 480,
                        fieldType = TypeEnum
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 44,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_9",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_enum_e"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_9",
                        fieldOffset = 512,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 45,
                          singleLocColumn = 31}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_10",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_enum_e"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_10",
                        fieldOffset = 640,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 46,
                          singleLocColumn = 31}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "a_typedef_struct"))
                      DeclPathTop,
                    structSizeof = 140,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "field_0",
                        fieldOffset = 0,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 36,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_1",
                        fieldOffset = 8,
                        fieldType = TypeTypedef
                          (CName "uint8_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 37,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_2",
                        fieldOffset = 16,
                        fieldType = TypeTypedef
                          (CName "uint16_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 38,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_3",
                        fieldOffset = 32,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 39,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_4",
                        fieldOffset = 64,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTypedef
                              (CName
                                "another_typedef_struct_t"))
                            DeclPathTop),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 40,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_5",
                        fieldOffset = 128,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTypedef
                                (CName
                                  "another_typedef_struct_t"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 41,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_6",
                        fieldOffset = 192,
                        fieldType = TypePointer
                          (TypePrim PrimVoid),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 42,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_7",
                        fieldOffset = 256,
                        fieldType = TypeConstArray
                          7
                          (TypeTypedef
                            (CName "uint32_t")),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 43,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_8",
                        fieldOffset = 480,
                        fieldType = TypeEnum
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 44,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_9",
                        fieldOffset = 512,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 45,
                          singleLocColumn = 31}},
                      StructField {
                        fieldName = CName "field_10",
                        fieldOffset = 640,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 46,
                          singleLocColumn = 31}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "distilled_lib_1.h"],
                      singleLocLine = 34,
                      singleLocColumn = 16}}}
              (Add 11)
              (Seq
                [
                  PokeByteOff (Idx 12) 0 (Idx 0),
                  PokeByteOff (Idx 12) 1 (Idx 1),
                  PokeByteOff (Idx 12) 2 (Idx 2),
                  PokeByteOff (Idx 12) 4 (Idx 3),
                  PokeByteOff (Idx 12) 8 (Idx 4),
                  PokeByteOff (Idx 12) 16 (Idx 5),
                  PokeByteOff (Idx 12) 24 (Idx 6),
                  PokeByteOff (Idx 12) 32 (Idx 7),
                  PokeByteOff (Idx 12) 60 (Idx 8),
                  PokeByteOff (Idx 12) 64 (Idx 9),
                  PokeByteOff
                    (Idx 12)
                    80
                    (Idx 10)])))}),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "A_typedef_struct_t",
      newtypeConstr = HsName
        "@NsConstr"
        "A_typedef_struct_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unA_typedef_struct_t",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "A_typedef_struct"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName
            "a_typedef_struct_t",
          typedefType = TypeStruct
            (DeclPathStruct
              (DeclNameTag
                (CName "a_typedef_struct"))
              DeclPathTop),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 47,
            singleLocColumn = 3}}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "A_typedef_struct_t"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "A_typedef_enum_e",
      newtypeConstr = HsName
        "@NsConstr"
        "A_typedef_enum_e",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unA_typedef_enum_e",
        fieldType = HsPrimType
          HsPrimCSChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName
            "a_typedef_enum_e",
          enumType = TypePrim
            (PrimChar (Just Unsigned)),
          enumSizeof = 1,
          enumAlignment = 1,
          enumValues = [
            EnumValue {
              valueName = CName "ENUM_CASE_0",
              valueValue = 0,
              valueSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 62,
                singleLocColumn = 3}},
            EnumValue {
              valueName = CName "ENUM_CASE_1",
              valueValue = 1,
              valueSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 63,
                singleLocColumn = 3}},
            EnumValue {
              valueName = CName "ENUM_CASE_2",
              valueValue = 2,
              valueSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 64,
                singleLocColumn = 3}},
            EnumValue {
              valueName = CName "ENUM_CASE_3",
              valueValue = 3,
              valueSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "distilled_lib_1.h"],
                singleLocLine = 65,
                singleLocColumn = 3}}],
          enumSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 60,
            singleLocColumn = 9}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "A_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "A_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "unA_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCSChar,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName
              "a_typedef_enum_e",
            enumType = TypePrim
              (PrimChar (Just Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumValues = [
              EnumValue {
                valueName = CName "ENUM_CASE_0",
                valueValue = 0,
                valueSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 62,
                  singleLocColumn = 3}},
              EnumValue {
                valueName = CName "ENUM_CASE_1",
                valueValue = 1,
                valueSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 63,
                  singleLocColumn = 3}},
              EnumValue {
                valueName = CName "ENUM_CASE_2",
                valueValue = 2,
                valueSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 64,
                  singleLocColumn = 3}},
              EnumValue {
                valueName = CName "ENUM_CASE_3",
                valueValue = 3,
                valueSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "distilled_lib_1.h"],
                  singleLocLine = 65,
                  singleLocColumn = 3}}],
            enumSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "distilled_lib_1.h"],
              singleLocLine = 60,
              singleLocColumn = 9}}}
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
                  "A_typedef_enum_e",
                structConstr = HsName
                  "@NsConstr"
                  "A_typedef_enum_e",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "unA_typedef_enum_e",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName
                      "a_typedef_enum_e",
                    enumType = TypePrim
                      (PrimChar (Just Unsigned)),
                    enumSizeof = 1,
                    enumAlignment = 1,
                    enumValues = [
                      EnumValue {
                        valueName = CName "ENUM_CASE_0",
                        valueValue = 0,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 62,
                          singleLocColumn = 3}},
                      EnumValue {
                        valueName = CName "ENUM_CASE_1",
                        valueValue = 1,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 63,
                          singleLocColumn = 3}},
                      EnumValue {
                        valueName = CName "ENUM_CASE_2",
                        valueValue = 2,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 64,
                          singleLocColumn = 3}},
                      EnumValue {
                        valueName = CName "ENUM_CASE_3",
                        valueValue = 3,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 65,
                          singleLocColumn = 3}}],
                    enumSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "distilled_lib_1.h"],
                      singleLocLine = 60,
                      singleLocColumn = 9}}})
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
                  "A_typedef_enum_e",
                structConstr = HsName
                  "@NsConstr"
                  "A_typedef_enum_e",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "unA_typedef_enum_e",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName
                      "a_typedef_enum_e",
                    enumType = TypePrim
                      (PrimChar (Just Unsigned)),
                    enumSizeof = 1,
                    enumAlignment = 1,
                    enumValues = [
                      EnumValue {
                        valueName = CName "ENUM_CASE_0",
                        valueValue = 0,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 62,
                          singleLocColumn = 3}},
                      EnumValue {
                        valueName = CName "ENUM_CASE_1",
                        valueValue = 1,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 63,
                          singleLocColumn = 3}},
                      EnumValue {
                        valueName = CName "ENUM_CASE_2",
                        valueValue = 2,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 64,
                          singleLocColumn = 3}},
                      EnumValue {
                        valueName = CName "ENUM_CASE_3",
                        valueValue = 3,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "distilled_lib_1.h"],
                          singleLocLine = 65,
                          singleLocColumn = 3}}],
                    enumSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "distilled_lib_1.h"],
                      singleLocLine = 60,
                      singleLocColumn = 9}}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "ENUM_CASE_0",
      patSynType = HsName
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "ENUM_CASE_0",
          valueValue = 0,
          valueSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 62,
            singleLocColumn = 3}}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "ENUM_CASE_1",
      patSynType = HsName
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "ENUM_CASE_1",
          valueValue = 1,
          valueSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 63,
            singleLocColumn = 3}}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "ENUM_CASE_2",
      patSynType = HsName
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 2,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "ENUM_CASE_2",
          valueValue = 2,
          valueSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 64,
            singleLocColumn = 3}}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "ENUM_CASE_3",
      patSynType = HsName
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 3,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "ENUM_CASE_3",
          valueValue = 3,
          valueSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 65,
            singleLocColumn = 3}}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Int32_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Int32_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unInt32_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "int32_t",
          typedefType = TypePrim
            (PrimIntegral (PrimInt Signed)),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "musl-include",
              "bits",
              "alltypes.h"],
            singleLocLine = 106,
            singleLocColumn = 25}}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Callback_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Callback_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unCallback_t",
        fieldType = HsFunPtr
          (HsFun
            (HsPtr (HsPrimType HsPrimVoid))
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Uint32_t"))
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Uint32_t"))))),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName
            "callback_t",
          typedefType = TypePointer
            (TypeFun
              [
                TypePointer (TypePrim PrimVoid),
                TypeTypedef (CName "uint32_t")]
              (TypeTypedef
                (CName "uint32_t"))),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "distilled_lib_1.h"],
            singleLocLine = 76,
            singleLocColumn = 19}}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Callback_t")]
