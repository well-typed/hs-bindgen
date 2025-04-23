[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Linked_list_A_s",
      structConstr = HsName
        "@NsConstr"
        "Linked_list_A_s",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "linked_list_A_s_x",
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
              "recursive_struct.h:2:7"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "linked_list_A_s_next",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Linked_list_A_s")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathName
                    (CName "linked_list_A_s"))),
              fieldSourceLoc =
              "recursive_struct.h:3:27"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "linked_list_A_s"),
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "recursive_struct.h:2:7"},
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathName
                    (CName "linked_list_A_s"))),
              fieldSourceLoc =
              "recursive_struct.h:3:27"}],
          structFlam = Nothing,
          structSourceLoc =
          "recursive_struct.h:1:16"},
      structInstances = Set.fromList
        []},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Linked_list_A_s",
        structConstr = HsName
          "@NsConstr"
          "Linked_list_A_s",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "linked_list_A_s_x",
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
                "recursive_struct.h:2:7"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "linked_list_A_s_next",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Linked_list_A_s")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathName
                      (CName "linked_list_A_s"))),
                fieldSourceLoc =
                "recursive_struct.h:3:27"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "linked_list_A_s"),
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "recursive_struct.h:2:7"},
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathName
                      (CName "linked_list_A_s"))),
                fieldSourceLoc =
                "recursive_struct.h:3:27"}],
            structFlam = Nothing,
            structSourceLoc =
            "recursive_struct.h:1:16"},
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
                  "Linked_list_A_s",
                structConstr = HsName
                  "@NsConstr"
                  "Linked_list_A_s",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_A_s_x",
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
                        "recursive_struct.h:2:7"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_A_s_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Linked_list_A_s")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "linked_list_A_s"))),
                        fieldSourceLoc =
                        "recursive_struct.h:3:27"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "linked_list_A_s"),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "recursive_struct.h:2:7"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "linked_list_A_s"))),
                        fieldSourceLoc =
                        "recursive_struct.h:3:27"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "recursive_struct.h:1:16"},
                structInstances = Set.fromList
                  []})
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
                  "Linked_list_A_s",
                structConstr = HsName
                  "@NsConstr"
                  "Linked_list_A_s",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_A_s_x",
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
                        "recursive_struct.h:2:7"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_A_s_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Linked_list_A_s")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "linked_list_A_s"))),
                        fieldSourceLoc =
                        "recursive_struct.h:3:27"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "linked_list_A_s"),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "recursive_struct.h:2:7"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "linked_list_A_s"))),
                        fieldSourceLoc =
                        "recursive_struct.h:3:27"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "recursive_struct.h:1:16"},
                structInstances = Set.fromList
                  []}
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
      "Linked_list_A_s"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Linked_list_A_s"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Linked_list_A_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Linked_list_A_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Linked_list_A_t",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "Linked_list_A_s"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName
            "linked_list_A_t",
          typedefType = TypeStruct
            (DeclPathName
              (CName "linked_list_A_s")),
          typedefSourceLoc =
          "recursive_struct.h:4:3"},
      newtypeInstances = Set.fromList
        []},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Linked_list_A_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Linked_list_B_t",
      structConstr = HsName
        "@NsConstr"
        "Linked_list_B_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "linked_list_B_t_x",
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
              "recursive_struct.h:10:7"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "linked_list_B_t_next",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Linked_list_B_t")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathName
                    (CName "linked_list_B_t"))),
              fieldSourceLoc =
              "recursive_struct.h:11:20"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "linked_list_B_t"),
          structAliases = [
            CName "linked_list_B_t"],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "recursive_struct.h:10:7"},
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathName
                    (CName "linked_list_B_t"))),
              fieldSourceLoc =
              "recursive_struct.h:11:20"}],
          structFlam = Nothing,
          structSourceLoc =
          "recursive_struct.h:9:8"},
      structInstances = Set.fromList
        []},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Linked_list_B_t",
        structConstr = HsName
          "@NsConstr"
          "Linked_list_B_t",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "linked_list_B_t_x",
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
                "recursive_struct.h:10:7"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "linked_list_B_t_next",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Linked_list_B_t")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathName
                      (CName "linked_list_B_t"))),
                fieldSourceLoc =
                "recursive_struct.h:11:20"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "linked_list_B_t"),
            structAliases = [
              CName "linked_list_B_t"],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "recursive_struct.h:10:7"},
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathName
                      (CName "linked_list_B_t"))),
                fieldSourceLoc =
                "recursive_struct.h:11:20"}],
            structFlam = Nothing,
            structSourceLoc =
            "recursive_struct.h:9:8"},
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
                  "Linked_list_B_t",
                structConstr = HsName
                  "@NsConstr"
                  "Linked_list_B_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_B_t_x",
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
                        "recursive_struct.h:10:7"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_B_t_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Linked_list_B_t")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "linked_list_B_t"))),
                        fieldSourceLoc =
                        "recursive_struct.h:11:20"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "linked_list_B_t"),
                    structAliases = [
                      CName "linked_list_B_t"],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "recursive_struct.h:10:7"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "linked_list_B_t"))),
                        fieldSourceLoc =
                        "recursive_struct.h:11:20"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "recursive_struct.h:9:8"},
                structInstances = Set.fromList
                  []})
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
                  "Linked_list_B_t",
                structConstr = HsName
                  "@NsConstr"
                  "Linked_list_B_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_B_t_x",
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
                        "recursive_struct.h:10:7"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "linked_list_B_t_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Linked_list_B_t")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "linked_list_B_t"))),
                        fieldSourceLoc =
                        "recursive_struct.h:11:20"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "linked_list_B_t"),
                    structAliases = [
                      CName "linked_list_B_t"],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "recursive_struct.h:10:7"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathName
                              (CName "linked_list_B_t"))),
                        fieldSourceLoc =
                        "recursive_struct.h:11:20"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "recursive_struct.h:9:8"},
                structInstances = Set.fromList
                  []}
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
      "Linked_list_B_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Linked_list_B_t")]
