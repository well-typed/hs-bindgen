[
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a",
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
        5
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "b",
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
        3
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "sOME_DEFINED_CONSTANT",
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
        "a_DEFINE_0",
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
        0
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a_DEFINE_1",
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
                          (IntLike (Int Unsigned)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        20560
        HsPrimCUInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a_DEFINE_2",
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
        2
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "tWO_ARGS",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon (TupleTyCon 2))))
            [
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
                    []],
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
                    []]]}},
      varDeclBody = VarDeclApp
        (InfixAppHead MTuple)
        [
          VarDeclIntegral
            13398
            HsPrimCInt,
          VarDeclIntegral
            30874
            HsPrimCInt]},
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
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Uint8_t")))
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
              TypeIncompleteArray
                (TypeTypedef (CName "uint8_t"))]
            (TypeTypedef (CName "int32_t")),
          functionHeader =
          "distilled_lib_1.h",
          functionSourceLoc =
          "distilled_lib_1.h:71:9"}},
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
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "distilled_lib_1.h:8:22"}},
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
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "distilled_lib_1.h:8:32"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            (DeclNameTypedef
              (CName
                "another_typedef_struct_t"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "foo",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "distilled_lib_1.h:8:22"},
            StructField {
              fieldName = CName "bar",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "distilled_lib_1.h:8:32"}],
          structFlam = Nothing,
          structSourceLoc =
          "distilled_lib_1.h:8:9"}},
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "distilled_lib_1.h:8:22"}},
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "distilled_lib_1.h:8:32"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              (DeclNameTypedef
                (CName
                  "another_typedef_struct_t"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "foo",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "distilled_lib_1.h:8:22"},
              StructField {
                fieldName = CName "bar",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "distilled_lib_1.h:8:32"}],
            structFlam = Nothing,
            structSourceLoc =
            "distilled_lib_1.h:8:9"}}
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "distilled_lib_1.h:8:22"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "distilled_lib_1.h:8:32"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTypedef
                        (CName
                          "another_typedef_struct_t"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "foo",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "distilled_lib_1.h:8:22"},
                      StructField {
                        fieldName = CName "bar",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "distilled_lib_1.h:8:32"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "distilled_lib_1.h:8:9"}})
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "distilled_lib_1.h:8:22"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "distilled_lib_1.h:8:32"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTypedef
                        (CName
                          "another_typedef_struct_t"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "foo",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "distilled_lib_1.h:8:22"},
                      StructField {
                        fieldName = CName "bar",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "distilled_lib_1.h:8:32"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "distilled_lib_1.h:8:9"}}
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
      "Another_typedef_struct_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Another_typedef_struct_t"),
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
          enumDeclPath = DeclPathConstr
            (DeclNameTypedef
              (CName
                "another_typedef_enum_e"))
            DeclPathTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "FOO",
              valueValue = 0,
              valueSourceLoc =
              "distilled_lib_1.h:9:16"},
            EnumValue {
              valueName = CName "BAR",
              valueValue = 1,
              valueSourceLoc =
              "distilled_lib_1.h:9:21"}],
          enumSourceLoc =
          "distilled_lib_1.h:9:9"}},
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
            enumDeclPath = DeclPathConstr
              (DeclNameTypedef
                (CName
                  "another_typedef_enum_e"))
              DeclPathTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "FOO",
                valueValue = 0,
                valueSourceLoc =
                "distilled_lib_1.h:9:16"},
              EnumValue {
                valueName = CName "BAR",
                valueValue = 1,
                valueSourceLoc =
                "distilled_lib_1.h:9:21"}],
            enumSourceLoc =
            "distilled_lib_1.h:9:9"}}
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
                    enumDeclPath = DeclPathConstr
                      (DeclNameTypedef
                        (CName
                          "another_typedef_enum_e"))
                      DeclPathTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "distilled_lib_1.h:9:16"},
                      EnumValue {
                        valueName = CName "BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "distilled_lib_1.h:9:21"}],
                    enumSourceLoc =
                    "distilled_lib_1.h:9:9"}})
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
                    enumDeclPath = DeclPathConstr
                      (DeclNameTypedef
                        (CName
                          "another_typedef_enum_e"))
                      DeclPathTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "distilled_lib_1.h:9:16"},
                      EnumValue {
                        valueName = CName "BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "distilled_lib_1.h:9:21"}],
                    enumSourceLoc =
                    "distilled_lib_1.h:9:9"}}
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
      "Another_typedef_enum_e"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Another_typedef_enum_e"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Another_typedef_enum_e"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Another_typedef_enum_e"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Another_typedef_enum_e"),
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
          valueSourceLoc =
          "distilled_lib_1.h:9:16"}},
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
          valueSourceLoc =
          "distilled_lib_1.h:9:21"}},
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
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "Another_typedef_enum_e"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName
            "another_typedef_enum_e",
          typedefType = TypeEnum
            (DeclPathConstr
              (DeclNameTypedef
                (CName
                  "another_typedef_enum_e"))
              DeclPathTop),
          typedefSourceLoc =
          "distilled_lib_1.h:9:27"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Another_typedef_enum_e"),
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
            (PrimIntegral PrimInt Signed),
          typedefSourceLoc =
          "distilled_lib_1.h:13:13"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
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
            (PrimIntegral PrimInt Signed),
          typedefSourceLoc =
          "distilled_lib_1.h:14:13"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
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
          typedefSourceLoc =
          "alltypes.h:121:25"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Uint8_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
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
              PrimShort
              Unsigned),
          typedefSourceLoc =
          "alltypes.h:126:25"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Uint16_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
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
            (PrimIntegral PrimInt Unsigned),
          typedefSourceLoc =
          "alltypes.h:131:25"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
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
              fieldWidth = Nothing,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc =
              "distilled_lib_1.h:36:31"}},
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
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "uint8_t"),
              fieldSourceLoc =
              "distilled_lib_1.h:37:31"}},
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
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "uint16_t"),
              fieldSourceLoc =
              "distilled_lib_1.h:38:31"}},
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
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "uint32_t"),
              fieldSourceLoc =
              "distilled_lib_1.h:39:31"}},
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
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathConstr
                  (DeclNameTypedef
                    (CName
                      "another_typedef_struct_t"))
                  DeclPathTop),
              fieldSourceLoc =
              "distilled_lib_1.h:40:31"}},
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
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathConstr
                    (DeclNameTypedef
                      (CName
                        "another_typedef_struct_t"))
                    DeclPathTop)),
              fieldSourceLoc =
              "distilled_lib_1.h:41:31"}},
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
              fieldWidth = Nothing,
              fieldType = TypePointer
                TypeVoid,
              fieldSourceLoc =
              "distilled_lib_1.h:42:31"}},
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
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                7
                (TypeTypedef
                  (CName "uint32_t")),
              fieldSourceLoc =
              "distilled_lib_1.h:43:31"}},
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
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName
                  "another_typedef_enum_e"),
              fieldSourceLoc =
              "distilled_lib_1.h:44:31"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_9",
          fieldType = HsConstArray
            4
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Another_typedef_enum_e")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_9",
              fieldOffset = 512,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                4
                (TypeTypedef
                  (CName
                    "another_typedef_enum_e")),
              fieldSourceLoc =
              "distilled_lib_1.h:45:31"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_field_10",
          fieldType = HsConstArray
            5
            (HsConstArray
              3
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Another_typedef_enum_e"))),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "field_10",
              fieldOffset = 640,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                5
                (TypeConstArray
                  3
                  (TypeTypedef
                    (CName
                      "another_typedef_enum_e"))),
              fieldSourceLoc =
              "distilled_lib_1.h:46:31"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            (DeclNameTag
              (CName "a_typedef_struct"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 140,
          structAlignment = 1,
          structFields = [
            StructField {
              fieldName = CName "field_0",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc =
              "distilled_lib_1.h:36:31"},
            StructField {
              fieldName = CName "field_1",
              fieldOffset = 8,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "uint8_t"),
              fieldSourceLoc =
              "distilled_lib_1.h:37:31"},
            StructField {
              fieldName = CName "field_2",
              fieldOffset = 16,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "uint16_t"),
              fieldSourceLoc =
              "distilled_lib_1.h:38:31"},
            StructField {
              fieldName = CName "field_3",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "uint32_t"),
              fieldSourceLoc =
              "distilled_lib_1.h:39:31"},
            StructField {
              fieldName = CName "field_4",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathConstr
                  (DeclNameTypedef
                    (CName
                      "another_typedef_struct_t"))
                  DeclPathTop),
              fieldSourceLoc =
              "distilled_lib_1.h:40:31"},
            StructField {
              fieldName = CName "field_5",
              fieldOffset = 128,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathConstr
                    (DeclNameTypedef
                      (CName
                        "another_typedef_struct_t"))
                    DeclPathTop)),
              fieldSourceLoc =
              "distilled_lib_1.h:41:31"},
            StructField {
              fieldName = CName "field_6",
              fieldOffset = 192,
              fieldWidth = Nothing,
              fieldType = TypePointer
                TypeVoid,
              fieldSourceLoc =
              "distilled_lib_1.h:42:31"},
            StructField {
              fieldName = CName "field_7",
              fieldOffset = 256,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                7
                (TypeTypedef
                  (CName "uint32_t")),
              fieldSourceLoc =
              "distilled_lib_1.h:43:31"},
            StructField {
              fieldName = CName "field_8",
              fieldOffset = 480,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName
                  "another_typedef_enum_e"),
              fieldSourceLoc =
              "distilled_lib_1.h:44:31"},
            StructField {
              fieldName = CName "field_9",
              fieldOffset = 512,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                4
                (TypeTypedef
                  (CName
                    "another_typedef_enum_e")),
              fieldSourceLoc =
              "distilled_lib_1.h:45:31"},
            StructField {
              fieldName = CName "field_10",
              fieldOffset = 640,
              fieldWidth = Nothing,
              fieldType = TypeConstArray
                5
                (TypeConstArray
                  3
                  (TypeTypedef
                    (CName
                      "another_typedef_enum_e"))),
              fieldSourceLoc =
              "distilled_lib_1.h:46:31"}],
          structFlam = Nothing,
          structSourceLoc =
          "distilled_lib_1.h:34:16"}},
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
                fieldWidth = Nothing,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc =
                "distilled_lib_1.h:36:31"}},
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
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "uint8_t"),
                fieldSourceLoc =
                "distilled_lib_1.h:37:31"}},
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
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "uint16_t"),
                fieldSourceLoc =
                "distilled_lib_1.h:38:31"}},
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
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "uint32_t"),
                fieldSourceLoc =
                "distilled_lib_1.h:39:31"}},
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
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathConstr
                    (DeclNameTypedef
                      (CName
                        "another_typedef_struct_t"))
                    DeclPathTop),
                fieldSourceLoc =
                "distilled_lib_1.h:40:31"}},
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
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathConstr
                      (DeclNameTypedef
                        (CName
                          "another_typedef_struct_t"))
                      DeclPathTop)),
                fieldSourceLoc =
                "distilled_lib_1.h:41:31"}},
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
                fieldWidth = Nothing,
                fieldType = TypePointer
                  TypeVoid,
                fieldSourceLoc =
                "distilled_lib_1.h:42:31"}},
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
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  7
                  (TypeTypedef
                    (CName "uint32_t")),
                fieldSourceLoc =
                "distilled_lib_1.h:43:31"}},
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
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName
                    "another_typedef_enum_e"),
                fieldSourceLoc =
                "distilled_lib_1.h:44:31"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_9",
            fieldType = HsConstArray
              4
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Another_typedef_enum_e")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_9",
                fieldOffset = 512,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  4
                  (TypeTypedef
                    (CName
                      "another_typedef_enum_e")),
                fieldSourceLoc =
                "distilled_lib_1.h:45:31"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_field_10",
            fieldType = HsConstArray
              5
              (HsConstArray
                3
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Another_typedef_enum_e"))),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "field_10",
                fieldOffset = 640,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  5
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (CName
                        "another_typedef_enum_e"))),
                fieldSourceLoc =
                "distilled_lib_1.h:46:31"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              (DeclNameTag
                (CName "a_typedef_struct"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 140,
            structAlignment = 1,
            structFields = [
              StructField {
                fieldName = CName "field_0",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc =
                "distilled_lib_1.h:36:31"},
              StructField {
                fieldName = CName "field_1",
                fieldOffset = 8,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "uint8_t"),
                fieldSourceLoc =
                "distilled_lib_1.h:37:31"},
              StructField {
                fieldName = CName "field_2",
                fieldOffset = 16,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "uint16_t"),
                fieldSourceLoc =
                "distilled_lib_1.h:38:31"},
              StructField {
                fieldName = CName "field_3",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "uint32_t"),
                fieldSourceLoc =
                "distilled_lib_1.h:39:31"},
              StructField {
                fieldName = CName "field_4",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathConstr
                    (DeclNameTypedef
                      (CName
                        "another_typedef_struct_t"))
                    DeclPathTop),
                fieldSourceLoc =
                "distilled_lib_1.h:40:31"},
              StructField {
                fieldName = CName "field_5",
                fieldOffset = 128,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathConstr
                      (DeclNameTypedef
                        (CName
                          "another_typedef_struct_t"))
                      DeclPathTop)),
                fieldSourceLoc =
                "distilled_lib_1.h:41:31"},
              StructField {
                fieldName = CName "field_6",
                fieldOffset = 192,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  TypeVoid,
                fieldSourceLoc =
                "distilled_lib_1.h:42:31"},
              StructField {
                fieldName = CName "field_7",
                fieldOffset = 256,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  7
                  (TypeTypedef
                    (CName "uint32_t")),
                fieldSourceLoc =
                "distilled_lib_1.h:43:31"},
              StructField {
                fieldName = CName "field_8",
                fieldOffset = 480,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName
                    "another_typedef_enum_e"),
                fieldSourceLoc =
                "distilled_lib_1.h:44:31"},
              StructField {
                fieldName = CName "field_9",
                fieldOffset = 512,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  4
                  (TypeTypedef
                    (CName
                      "another_typedef_enum_e")),
                fieldSourceLoc =
                "distilled_lib_1.h:45:31"},
              StructField {
                fieldName = CName "field_10",
                fieldOffset = 640,
                fieldWidth = Nothing,
                fieldType = TypeConstArray
                  5
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (CName
                        "another_typedef_enum_e"))),
                fieldSourceLoc =
                "distilled_lib_1.h:46:31"}],
            structFlam = Nothing,
            structSourceLoc =
            "distilled_lib_1.h:34:16"}}
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "distilled_lib_1.h:36:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint8_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:37:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint16_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:38:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:39:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTypedef
                              (CName
                                "another_typedef_struct_t"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "distilled_lib_1.h:40:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTypedef
                                (CName
                                  "another_typedef_struct_t"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "distilled_lib_1.h:41:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          TypeVoid,
                        fieldSourceLoc =
                        "distilled_lib_1.h:42:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          7
                          (TypeTypedef
                            (CName "uint32_t")),
                        fieldSourceLoc =
                        "distilled_lib_1.h:43:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:44:31"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_9",
                    fieldType = HsConstArray
                      4
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Another_typedef_enum_e")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_9",
                        fieldOffset = 512,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          4
                          (TypeTypedef
                            (CName
                              "another_typedef_enum_e")),
                        fieldSourceLoc =
                        "distilled_lib_1.h:45:31"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_10",
                    fieldType = HsConstArray
                      5
                      (HsConstArray
                        3
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "Another_typedef_enum_e"))),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_10",
                        fieldOffset = 640,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          5
                          (TypeConstArray
                            3
                            (TypeTypedef
                              (CName
                                "another_typedef_enum_e"))),
                        fieldSourceLoc =
                        "distilled_lib_1.h:46:31"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag
                        (CName "a_typedef_struct"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 140,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "field_0",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "distilled_lib_1.h:36:31"},
                      StructField {
                        fieldName = CName "field_1",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint8_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:37:31"},
                      StructField {
                        fieldName = CName "field_2",
                        fieldOffset = 16,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint16_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:38:31"},
                      StructField {
                        fieldName = CName "field_3",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:39:31"},
                      StructField {
                        fieldName = CName "field_4",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTypedef
                              (CName
                                "another_typedef_struct_t"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "distilled_lib_1.h:40:31"},
                      StructField {
                        fieldName = CName "field_5",
                        fieldOffset = 128,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTypedef
                                (CName
                                  "another_typedef_struct_t"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "distilled_lib_1.h:41:31"},
                      StructField {
                        fieldName = CName "field_6",
                        fieldOffset = 192,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          TypeVoid,
                        fieldSourceLoc =
                        "distilled_lib_1.h:42:31"},
                      StructField {
                        fieldName = CName "field_7",
                        fieldOffset = 256,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          7
                          (TypeTypedef
                            (CName "uint32_t")),
                        fieldSourceLoc =
                        "distilled_lib_1.h:43:31"},
                      StructField {
                        fieldName = CName "field_8",
                        fieldOffset = 480,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:44:31"},
                      StructField {
                        fieldName = CName "field_9",
                        fieldOffset = 512,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          4
                          (TypeTypedef
                            (CName
                              "another_typedef_enum_e")),
                        fieldSourceLoc =
                        "distilled_lib_1.h:45:31"},
                      StructField {
                        fieldName = CName "field_10",
                        fieldOffset = 640,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          5
                          (TypeConstArray
                            3
                            (TypeTypedef
                              (CName
                                "another_typedef_enum_e"))),
                        fieldSourceLoc =
                        "distilled_lib_1.h:46:31"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "distilled_lib_1.h:34:16"}})
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "distilled_lib_1.h:36:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint8_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:37:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint16_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:38:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:39:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTypedef
                              (CName
                                "another_typedef_struct_t"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "distilled_lib_1.h:40:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTypedef
                                (CName
                                  "another_typedef_struct_t"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "distilled_lib_1.h:41:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          TypeVoid,
                        fieldSourceLoc =
                        "distilled_lib_1.h:42:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          7
                          (TypeTypedef
                            (CName "uint32_t")),
                        fieldSourceLoc =
                        "distilled_lib_1.h:43:31"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:44:31"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_9",
                    fieldType = HsConstArray
                      4
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Another_typedef_enum_e")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_9",
                        fieldOffset = 512,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          4
                          (TypeTypedef
                            (CName
                              "another_typedef_enum_e")),
                        fieldSourceLoc =
                        "distilled_lib_1.h:45:31"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_field_10",
                    fieldType = HsConstArray
                      5
                      (HsConstArray
                        3
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "Another_typedef_enum_e"))),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "field_10",
                        fieldOffset = 640,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          5
                          (TypeConstArray
                            3
                            (TypeTypedef
                              (CName
                                "another_typedef_enum_e"))),
                        fieldSourceLoc =
                        "distilled_lib_1.h:46:31"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag
                        (CName "a_typedef_struct"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 140,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "field_0",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "distilled_lib_1.h:36:31"},
                      StructField {
                        fieldName = CName "field_1",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint8_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:37:31"},
                      StructField {
                        fieldName = CName "field_2",
                        fieldOffset = 16,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint16_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:38:31"},
                      StructField {
                        fieldName = CName "field_3",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:39:31"},
                      StructField {
                        fieldName = CName "field_4",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTypedef
                              (CName
                                "another_typedef_struct_t"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "distilled_lib_1.h:40:31"},
                      StructField {
                        fieldName = CName "field_5",
                        fieldOffset = 128,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTypedef
                                (CName
                                  "another_typedef_struct_t"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "distilled_lib_1.h:41:31"},
                      StructField {
                        fieldName = CName "field_6",
                        fieldOffset = 192,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          TypeVoid,
                        fieldSourceLoc =
                        "distilled_lib_1.h:42:31"},
                      StructField {
                        fieldName = CName "field_7",
                        fieldOffset = 256,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          7
                          (TypeTypedef
                            (CName "uint32_t")),
                        fieldSourceLoc =
                        "distilled_lib_1.h:43:31"},
                      StructField {
                        fieldName = CName "field_8",
                        fieldOffset = 480,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName
                            "another_typedef_enum_e"),
                        fieldSourceLoc =
                        "distilled_lib_1.h:44:31"},
                      StructField {
                        fieldName = CName "field_9",
                        fieldOffset = 512,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          4
                          (TypeTypedef
                            (CName
                              "another_typedef_enum_e")),
                        fieldSourceLoc =
                        "distilled_lib_1.h:45:31"},
                      StructField {
                        fieldName = CName "field_10",
                        fieldOffset = 640,
                        fieldWidth = Nothing,
                        fieldType = TypeConstArray
                          5
                          (TypeConstArray
                            3
                            (TypeTypedef
                              (CName
                                "another_typedef_enum_e"))),
                        fieldSourceLoc =
                        "distilled_lib_1.h:46:31"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "distilled_lib_1.h:34:16"}}
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
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "A_typedef_struct"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "A_typedef_struct"),
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
            (DeclPathConstr
              (DeclNameTag
                (CName "a_typedef_struct"))
              DeclPathTop),
          typedefSourceLoc =
          "distilled_lib_1.h:47:3"}},
  DeclNewtypeInstance
    DeriveNewtype
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
          enumDeclPath = DeclPathConstr
            (DeclNameTypedef
              (CName "a_typedef_enum_e"))
            DeclPathTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimChar (Just Unsigned)),
          enumSizeof = 1,
          enumAlignment = 1,
          enumValues = [
            EnumValue {
              valueName = CName "ENUM_CASE_0",
              valueValue = 0,
              valueSourceLoc =
              "distilled_lib_1.h:62:3"},
            EnumValue {
              valueName = CName "ENUM_CASE_1",
              valueValue = 1,
              valueSourceLoc =
              "distilled_lib_1.h:63:3"},
            EnumValue {
              valueName = CName "ENUM_CASE_2",
              valueValue = 2,
              valueSourceLoc =
              "distilled_lib_1.h:64:3"},
            EnumValue {
              valueName = CName "ENUM_CASE_3",
              valueValue = 3,
              valueSourceLoc =
              "distilled_lib_1.h:65:3"}],
          enumSourceLoc =
          "distilled_lib_1.h:60:9"}},
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
            enumDeclPath = DeclPathConstr
              (DeclNameTypedef
                (CName "a_typedef_enum_e"))
              DeclPathTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimChar (Just Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumValues = [
              EnumValue {
                valueName = CName "ENUM_CASE_0",
                valueValue = 0,
                valueSourceLoc =
                "distilled_lib_1.h:62:3"},
              EnumValue {
                valueName = CName "ENUM_CASE_1",
                valueValue = 1,
                valueSourceLoc =
                "distilled_lib_1.h:63:3"},
              EnumValue {
                valueName = CName "ENUM_CASE_2",
                valueValue = 2,
                valueSourceLoc =
                "distilled_lib_1.h:64:3"},
              EnumValue {
                valueName = CName "ENUM_CASE_3",
                valueValue = 3,
                valueSourceLoc =
                "distilled_lib_1.h:65:3"}],
            enumSourceLoc =
            "distilled_lib_1.h:60:9"}}
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
                    enumDeclPath = DeclPathConstr
                      (DeclNameTypedef
                        (CName "a_typedef_enum_e"))
                      DeclPathTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimChar (Just Unsigned)),
                    enumSizeof = 1,
                    enumAlignment = 1,
                    enumValues = [
                      EnumValue {
                        valueName = CName "ENUM_CASE_0",
                        valueValue = 0,
                        valueSourceLoc =
                        "distilled_lib_1.h:62:3"},
                      EnumValue {
                        valueName = CName "ENUM_CASE_1",
                        valueValue = 1,
                        valueSourceLoc =
                        "distilled_lib_1.h:63:3"},
                      EnumValue {
                        valueName = CName "ENUM_CASE_2",
                        valueValue = 2,
                        valueSourceLoc =
                        "distilled_lib_1.h:64:3"},
                      EnumValue {
                        valueName = CName "ENUM_CASE_3",
                        valueValue = 3,
                        valueSourceLoc =
                        "distilled_lib_1.h:65:3"}],
                    enumSourceLoc =
                    "distilled_lib_1.h:60:9"}})
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
                    enumDeclPath = DeclPathConstr
                      (DeclNameTypedef
                        (CName "a_typedef_enum_e"))
                      DeclPathTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimChar (Just Unsigned)),
                    enumSizeof = 1,
                    enumAlignment = 1,
                    enumValues = [
                      EnumValue {
                        valueName = CName "ENUM_CASE_0",
                        valueValue = 0,
                        valueSourceLoc =
                        "distilled_lib_1.h:62:3"},
                      EnumValue {
                        valueName = CName "ENUM_CASE_1",
                        valueValue = 1,
                        valueSourceLoc =
                        "distilled_lib_1.h:63:3"},
                      EnumValue {
                        valueName = CName "ENUM_CASE_2",
                        valueValue = 2,
                        valueSourceLoc =
                        "distilled_lib_1.h:64:3"},
                      EnumValue {
                        valueName = CName "ENUM_CASE_3",
                        valueValue = 3,
                        valueSourceLoc =
                        "distilled_lib_1.h:65:3"}],
                    enumSourceLoc =
                    "distilled_lib_1.h:60:9"}}
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
      "A_typedef_enum_e"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "A_typedef_enum_e"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "A_typedef_enum_e"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "A_typedef_enum_e"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "A_typedef_enum_e"),
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
          valueSourceLoc =
          "distilled_lib_1.h:62:3"}},
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
          valueSourceLoc =
          "distilled_lib_1.h:63:3"}},
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
          valueSourceLoc =
          "distilled_lib_1.h:64:3"}},
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
          valueSourceLoc =
          "distilled_lib_1.h:65:3"}},
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
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "A_typedef_enum_e"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName
            "a_typedef_enum_e",
          typedefType = TypeEnum
            (DeclPathConstr
              (DeclNameTypedef
                (CName "a_typedef_enum_e"))
              DeclPathTop),
          typedefSourceLoc =
          "distilled_lib_1.h:66:13"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "A_typedef_enum_e"),
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
            (PrimIntegral PrimInt Signed),
          typedefSourceLoc =
          "alltypes.h:106:25"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Int32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
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
                TypePointer TypeVoid,
                TypeTypedef (CName "uint32_t")]
              (TypeTypedef
                (CName "uint32_t"))),
          typedefSourceLoc =
          "distilled_lib_1.h:76:19"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Callback_t")]
