[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "UnionA",
      newtypeConstr = HsName
        "@NsConstr"
        "UnionA",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_UnionA",
        fieldType = HsByteArray,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginUnion
        Union {
          unionDeclPath = DeclPathName
            (CName "unionA"),
          unionAliases = [],
          unionSizeof = 4,
          unionAlignment = 4,
          unionFields = [
            UnionField {
              ufieldName = CName "a",
              ufieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              ufieldSourceLoc =
              "nested_unions.h:3:21"},
            UnionField {
              ufieldName = CName "b",
              ufieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              ufieldSourceLoc =
              "nested_unions.h:4:22"}],
          unionSourceLoc =
          "nested_unions.h:2:15"},
      newtypeInstances = Set.fromList
        []},
  DeclNewtypeInstance
    (DeriveVia
      (HsSizedByteArray 4 4))
    Storable
    (HsName
      "@NsTypeConstr"
      "UnionA"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "UnionA")
    (HsPrimType HsPrimCInt)
    (HsName
      "@NsVar"
      "get_unionA_a"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "UnionA")
    (HsPrimType HsPrimCInt)
    (HsName
      "@NsVar"
      "set_unionA_a"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "UnionA")
    (HsPrimType HsPrimCChar)
    (HsName
      "@NsVar"
      "get_unionA_b"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "UnionA")
    (HsPrimType HsPrimCChar)
    (HsName
      "@NsVar"
      "set_unionA_b"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "ExA",
      structConstr = HsName
        "@NsConstr"
        "ExA",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "exA_fieldA1",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "UnionA"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "fieldA1",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeUnion
                (DeclPathName (CName "unionA")),
              fieldSourceLoc =
              "nested_unions.h:5:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "exA"),
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "fieldA1",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeUnion
                (DeclPathName (CName "unionA")),
              fieldSourceLoc =
              "nested_unions.h:5:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "nested_unions.h:1:8"},
      structInstances = Set.fromList
        []},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExA",
        structConstr = HsName
          "@NsConstr"
          "ExA",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "exA_fieldA1",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "UnionA"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "fieldA1",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeUnion
                  (DeclPathName (CName "unionA")),
                fieldSourceLoc =
                "nested_unions.h:5:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "exA"),
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "fieldA1",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeUnion
                  (DeclPathName (CName "unionA")),
                fieldSourceLoc =
                "nested_unions.h:5:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "nested_unions.h:1:8"},
        structInstances = Set.fromList
          []}
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
                  "ExA",
                structConstr = HsName
                  "@NsConstr"
                  "ExA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exA_fieldA1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "UnionA"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldA1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathName (CName "unionA")),
                        fieldSourceLoc =
                        "nested_unions.h:5:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "exA"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldA1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathName (CName "unionA")),
                        fieldSourceLoc =
                        "nested_unions.h:5:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_unions.h:1:8"},
                structInstances = Set.fromList
                  []})
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
                  "ExA",
                structConstr = HsName
                  "@NsConstr"
                  "ExA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exA_fieldA1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "UnionA"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldA1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathName (CName "unionA")),
                        fieldSourceLoc =
                        "nested_unions.h:5:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "exA"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldA1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathName (CName "unionA")),
                        fieldSourceLoc =
                        "nested_unions.h:5:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_unions.h:1:8"},
                structInstances = Set.fromList
                  []}
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
    (HsName "@NsTypeConstr" "ExA"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "ExA"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      newtypeConstr = HsName
        "@NsConstr"
        "ExB_fieldB1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_ExB_fieldB1",
        fieldType = HsByteArray,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginUnion
        Union {
          unionDeclPath = DeclPathAnon
            (DeclPathCtxtField
              (Just (CName "exB"))
              (CName "fieldB1")
              DeclPathCtxtTop),
          unionAliases = [],
          unionSizeof = 4,
          unionAlignment = 4,
          unionFields = [
            UnionField {
              ufieldName = CName "a",
              ufieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              ufieldSourceLoc =
              "nested_unions.h:10:21"},
            UnionField {
              ufieldName = CName "b",
              ufieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              ufieldSourceLoc =
              "nested_unions.h:11:22"}],
          unionSourceLoc =
          "nested_unions.h:9:9"},
      newtypeInstances = Set.fromList
        []},
  DeclNewtypeInstance
    (DeriveVia
      (HsSizedByteArray 4 4))
    Storable
    (HsName
      "@NsTypeConstr"
      "ExB_fieldB1"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "ExB_fieldB1")
    (HsPrimType HsPrimCInt)
    (HsName
      "@NsVar"
      "get_exB_fieldB1_a"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "ExB_fieldB1")
    (HsPrimType HsPrimCInt)
    (HsName
      "@NsVar"
      "set_exB_fieldB1_a"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "ExB_fieldB1")
    (HsPrimType HsPrimCChar)
    (HsName
      "@NsVar"
      "get_exB_fieldB1_b"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "ExB_fieldB1")
    (HsPrimType HsPrimCChar)
    (HsName
      "@NsVar"
      "set_exB_fieldB1_b"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "ExB",
      structConstr = HsName
        "@NsConstr"
        "ExB",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "exB_fieldB1",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "ExB_fieldB1"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "fieldB1",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeUnion
                (DeclPathAnon
                  (DeclPathCtxtField
                    (Just (CName "exB"))
                    (CName "fieldB1")
                    DeclPathCtxtTop)),
              fieldSourceLoc =
              "nested_unions.h:12:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "exB"),
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "fieldB1",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeUnion
                (DeclPathAnon
                  (DeclPathCtxtField
                    (Just (CName "exB"))
                    (CName "fieldB1")
                    DeclPathCtxtTop)),
              fieldSourceLoc =
              "nested_unions.h:12:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "nested_unions.h:8:8"},
      structInstances = Set.fromList
        []},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB",
        structConstr = HsName
          "@NsConstr"
          "ExB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "exB_fieldB1",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "ExB_fieldB1"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "fieldB1",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeUnion
                  (DeclPathAnon
                    (DeclPathCtxtField
                      (Just (CName "exB"))
                      (CName "fieldB1")
                      DeclPathCtxtTop)),
                fieldSourceLoc =
                "nested_unions.h:12:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "exB"),
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "fieldB1",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeUnion
                  (DeclPathAnon
                    (DeclPathCtxtField
                      (Just (CName "exB"))
                      (CName "fieldB1")
                      DeclPathCtxtTop)),
                fieldSourceLoc =
                "nested_unions.h:12:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "nested_unions.h:8:8"},
        structInstances = Set.fromList
          []}
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
                  "ExB",
                structConstr = HsName
                  "@NsConstr"
                  "ExB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exB_fieldB1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "ExB_fieldB1"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldB1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "exB"))
                              (CName "fieldB1")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "nested_unions.h:12:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "exB"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldB1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "exB"))
                              (CName "fieldB1")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "nested_unions.h:12:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_unions.h:8:8"},
                structInstances = Set.fromList
                  []})
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
                  "ExB",
                structConstr = HsName
                  "@NsConstr"
                  "ExB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exB_fieldB1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "ExB_fieldB1"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldB1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "exB"))
                              (CName "fieldB1")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "nested_unions.h:12:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "exB"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldB1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "exB"))
                              (CName "fieldB1")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "nested_unions.h:12:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_unions.h:8:8"},
                structInstances = Set.fromList
                  []}
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
    (HsName "@NsTypeConstr" "ExB"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "ExB")]
