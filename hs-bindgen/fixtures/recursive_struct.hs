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
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/recursive_struct.h:2:7"}},
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
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag
                      (CName "linked_list_A_s"))
                    DeclPathTop)),
              fieldSourceLoc =
              "examples/recursive_struct.h:3:27"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag
              (CName "linked_list_A_s"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/recursive_struct.h:2:7"},
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag
                      (CName "linked_list_A_s"))
                    DeclPathTop)),
              fieldSourceLoc =
              "examples/recursive_struct.h:3:27"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/recursive_struct.h:1:16",
          structBitfields = []}},
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
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/recursive_struct.h:2:7"}},
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
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag
                        (CName "linked_list_A_s"))
                      DeclPathTop)),
                fieldSourceLoc =
                "examples/recursive_struct.h:3:27"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag
                (CName "linked_list_A_s"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/recursive_struct.h:2:7"},
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag
                        (CName "linked_list_A_s"))
                      DeclPathTop)),
                fieldSourceLoc =
                "examples/recursive_struct.h:3:27"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/recursive_struct.h:1:16",
            structBitfields = []}}
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:2:7"}},
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
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_A_s"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:3:27"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "linked_list_A_s"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:2:7"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_A_s"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:3:27"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/recursive_struct.h:1:16",
                    structBitfields = []}})
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:2:7"}},
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
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_A_s"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:3:27"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "linked_list_A_s"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:2:7"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_A_s"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:3:27"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/recursive_struct.h:1:16",
                    structBitfields = []}}
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
          "unLinked_list_A_t",
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
            (DeclPathStruct
              (DeclNameTag
                (CName "linked_list_A_s"))
              DeclPathTop),
          typedefSourceLoc =
          "examples/recursive_struct.h:4:3"}},
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
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/recursive_struct.h:10:7"}},
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
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag
                      (CName "linked_list_B_t"))
                    DeclPathTop)),
              fieldSourceLoc =
              "examples/recursive_struct.h:11:20"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag
              (CName "linked_list_B_t"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/recursive_struct.h:10:7"},
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag
                      (CName "linked_list_B_t"))
                    DeclPathTop)),
              fieldSourceLoc =
              "examples/recursive_struct.h:11:20"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/recursive_struct.h:9:8",
          structBitfields = []}},
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
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/recursive_struct.h:10:7"}},
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
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag
                        (CName "linked_list_B_t"))
                      DeclPathTop)),
                fieldSourceLoc =
                "examples/recursive_struct.h:11:20"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag
                (CName "linked_list_B_t"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/recursive_struct.h:10:7"},
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag
                        (CName "linked_list_B_t"))
                      DeclPathTop)),
                fieldSourceLoc =
                "examples/recursive_struct.h:11:20"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/recursive_struct.h:9:8",
            structBitfields = []}}
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:10:7"}},
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
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_B_t"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:11:20"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "linked_list_B_t"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:10:7"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_B_t"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:11:20"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/recursive_struct.h:9:8",
                    structBitfields = []}})
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:10:7"}},
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
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_B_t"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:11:20"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "linked_list_B_t"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:10:7"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_B_t"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/recursive_struct.h:11:20"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/recursive_struct.h:9:8",
                    structBitfields = []}}
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
