[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "C__SFILE",
      structConstr = HsName
        "@NsConstr"
        "C__SFILE",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "c__SFILE__r",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "_r",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "attributes.h:9:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "c__SFILE__w",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "_w",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "attributes.h:10:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "c__SFILE__close",
          fieldType = HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsIO (HsPrimType HsPrimCInt))),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "_close",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeFun
                  [TypePointer TypeVoid]
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              fieldSourceLoc =
              "attributes.h:11:19"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "__sFILE"),
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "_r",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "attributes.h:9:9"},
            StructField {
              fieldName = CName "_w",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "attributes.h:10:9"},
            StructField {
              fieldName = CName "_close",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeFun
                  [TypePointer TypeVoid]
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              fieldSourceLoc =
              "attributes.h:11:19"}],
          structFlam = Nothing,
          structSourceLoc =
          "attributes.h:8:16"},
      structInstances = Set.fromList
        []},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "C__SFILE",
        structConstr = HsName
          "@NsConstr"
          "C__SFILE",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "c__SFILE__r",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "_r",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "attributes.h:9:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "c__SFILE__w",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "_w",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "attributes.h:10:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "c__SFILE__close",
            fieldType = HsFunPtr
              (HsFun
                (HsPtr (HsPrimType HsPrimVoid))
                (HsIO (HsPrimType HsPrimCInt))),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "_close",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeFun
                    [TypePointer TypeVoid]
                    (TypePrim
                      (PrimIntegral PrimInt Signed))),
                fieldSourceLoc =
                "attributes.h:11:19"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "__sFILE"),
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "_r",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "attributes.h:9:9"},
              StructField {
                fieldName = CName "_w",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "attributes.h:10:9"},
              StructField {
                fieldName = CName "_close",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeFun
                    [TypePointer TypeVoid]
                    (TypePrim
                      (PrimIntegral PrimInt Signed))),
                fieldSourceLoc =
                "attributes.h:11:19"}],
            structFlam = Nothing,
            structSourceLoc =
            "attributes.h:8:16"},
        structInstances = Set.fromList
          []}
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
                  "C__SFILE",
                structConstr = HsName
                  "@NsConstr"
                  "C__SFILE",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "c__SFILE__r",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "_r",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "attributes.h:9:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "c__SFILE__w",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "_w",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "attributes.h:10:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "c__SFILE__close",
                    fieldType = HsFunPtr
                      (HsFun
                        (HsPtr (HsPrimType HsPrimVoid))
                        (HsIO (HsPrimType HsPrimCInt))),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "_close",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeFun
                            [TypePointer TypeVoid]
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "attributes.h:11:19"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "__sFILE"),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "_r",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "attributes.h:9:9"},
                      StructField {
                        fieldName = CName "_w",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "attributes.h:10:9"},
                      StructField {
                        fieldName = CName "_close",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeFun
                            [TypePointer TypeVoid]
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "attributes.h:11:19"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "attributes.h:8:16"},
                structInstances = Set.fromList
                  []})
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
                  "C__SFILE",
                structConstr = HsName
                  "@NsConstr"
                  "C__SFILE",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "c__SFILE__r",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "_r",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "attributes.h:9:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "c__SFILE__w",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "_w",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "attributes.h:10:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "c__SFILE__close",
                    fieldType = HsFunPtr
                      (HsFun
                        (HsPtr (HsPrimType HsPrimVoid))
                        (HsIO (HsPrimType HsPrimCInt))),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "_close",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeFun
                            [TypePointer TypeVoid]
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "attributes.h:11:19"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "__sFILE"),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "_r",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "attributes.h:9:9"},
                      StructField {
                        fieldName = CName "_w",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "attributes.h:10:9"},
                      StructField {
                        fieldName = CName "_close",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeFun
                            [TypePointer TypeVoid]
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        fieldSourceLoc =
                        "attributes.h:11:19"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "attributes.h:8:16"},
                structInstances = Set.fromList
                  []}
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
      "C__SFILE"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "C__SFILE"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "FILE",
      newtypeConstr = HsName
        "@NsConstr"
        "FILE",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_FILE",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "C__SFILE"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "FILE",
          typedefType = TypeStruct
            (DeclPathName
              (CName "__sFILE")),
          typedefSourceLoc =
          "attributes.h:12:3"},
      newtypeInstances = Set.fromList
        []},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "FILE")]
