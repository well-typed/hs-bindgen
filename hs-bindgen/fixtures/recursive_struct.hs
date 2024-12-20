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
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "recursive_struct.h"],
                singleLocLine = 2,
                singleLocColumn = 7}}},
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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "recursive_struct.h"],
                singleLocLine = 3,
                singleLocColumn = 27}}}],
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
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "recursive_struct.h"],
                singleLocLine = 2,
                singleLocColumn = 7}},
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag
                      (CName "linked_list_A_s"))
                    DeclPathTop)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "recursive_struct.h"],
                singleLocLine = 3,
                singleLocColumn = 27}}],
          structFlam = Nothing,
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "recursive_struct.h"],
            singleLocLine = 1,
            singleLocColumn = 16}}},
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
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "recursive_struct.h"],
                  singleLocLine = 2,
                  singleLocColumn = 7}}},
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "recursive_struct.h"],
                  singleLocLine = 3,
                  singleLocColumn = 27}}}],
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
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "recursive_struct.h"],
                  singleLocLine = 2,
                  singleLocColumn = 7}},
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag
                        (CName "linked_list_A_s"))
                      DeclPathTop)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "recursive_struct.h"],
                  singleLocLine = 3,
                  singleLocColumn = 27}}],
            structFlam = Nothing,
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "recursive_struct.h"],
              singleLocLine = 1,
              singleLocColumn = 16}}}
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
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 2,
                          singleLocColumn = 7}}},
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 3,
                          singleLocColumn = 27}}}],
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
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 2,
                          singleLocColumn = 7}},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_A_s"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 3,
                          singleLocColumn = 27}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "recursive_struct.h"],
                      singleLocLine = 1,
                      singleLocColumn = 16}}})
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
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 2,
                          singleLocColumn = 7}}},
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 3,
                          singleLocColumn = 27}}}],
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
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 2,
                          singleLocColumn = 7}},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_A_s"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 3,
                          singleLocColumn = 27}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "recursive_struct.h"],
                      singleLocLine = 1,
                      singleLocColumn = 16}}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
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
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "recursive_struct.h"],
            singleLocLine = 4,
            singleLocColumn = 3}}},
  DeclNewtypeInstance
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
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "recursive_struct.h"],
                singleLocLine = 10,
                singleLocColumn = 7}}},
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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "recursive_struct.h"],
                singleLocLine = 11,
                singleLocColumn = 20}}}],
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
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "recursive_struct.h"],
                singleLocLine = 10,
                singleLocColumn = 7}},
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag
                      (CName "linked_list_B_t"))
                    DeclPathTop)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "recursive_struct.h"],
                singleLocLine = 11,
                singleLocColumn = 20}}],
          structFlam = Nothing,
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "recursive_struct.h"],
            singleLocLine = 9,
            singleLocColumn = 8}}},
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
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "recursive_struct.h"],
                  singleLocLine = 10,
                  singleLocColumn = 7}}},
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "recursive_struct.h"],
                  singleLocLine = 11,
                  singleLocColumn = 20}}}],
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
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "recursive_struct.h"],
                  singleLocLine = 10,
                  singleLocColumn = 7}},
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag
                        (CName "linked_list_B_t"))
                      DeclPathTop)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "recursive_struct.h"],
                  singleLocLine = 11,
                  singleLocColumn = 20}}],
            structFlam = Nothing,
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "recursive_struct.h"],
              singleLocLine = 9,
              singleLocColumn = 8}}}
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
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 10,
                          singleLocColumn = 7}}},
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 11,
                          singleLocColumn = 20}}}],
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
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 10,
                          singleLocColumn = 7}},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_B_t"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 11,
                          singleLocColumn = 20}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "recursive_struct.h"],
                      singleLocLine = 9,
                      singleLocColumn = 8}}})
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
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 10,
                          singleLocColumn = 7}}},
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 11,
                          singleLocColumn = 20}}}],
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
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 10,
                          singleLocColumn = 7}},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag
                                (CName "linked_list_B_t"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "recursive_struct.h"],
                          singleLocLine = 11,
                          singleLocColumn = 20}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "recursive_struct.h"],
                      singleLocLine = 9,
                      singleLocColumn = 8}}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))})]
