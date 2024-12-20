[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Primitive",
      structConstr = HsName
        "@NsConstr"
        "Primitive",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "c",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 2,
                singleLocColumn = 10}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sc",
          fieldType = HsPrimType
            HsPrimCSChar,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "sc",
              fieldOffset = 8,
              fieldType = TypePrim
                (PrimChar (Just Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 3,
                singleLocColumn = 17}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_uc",
          fieldType = HsPrimType
            HsPrimCSChar,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "uc",
              fieldOffset = 16,
              fieldType = TypePrim
                (PrimChar (Just Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 4,
                singleLocColumn = 19}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_s",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "s",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 6,
                singleLocColumn = 11}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_si",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "si",
              fieldOffset = 48,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 7,
                singleLocColumn = 15}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ss",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ss",
              fieldOffset = 64,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 8,
                singleLocColumn = 18}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ssi",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ssi",
              fieldOffset = 80,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 9,
                singleLocColumn = 22}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_us",
          fieldType = HsPrimType
            HsPrimCUShort,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "us",
              fieldOffset = 96,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 11,
                singleLocColumn = 20}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_usi",
          fieldType = HsPrimType
            HsPrimCUShort,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "usi",
              fieldOffset = 112,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 12,
                singleLocColumn = 24}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "i",
              fieldOffset = 128,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 14,
                singleLocColumn = 9}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_s2",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "s2",
              fieldOffset = 160,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 15,
                singleLocColumn = 12}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_si2",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "si2",
              fieldOffset = 192,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 16,
                singleLocColumn = 16}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_u",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "u",
              fieldOffset = 224,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimInt Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 18,
                singleLocColumn = 14}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ui",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ui",
              fieldOffset = 256,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimInt Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 19,
                singleLocColumn = 18}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_l",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "l",
              fieldOffset = 320,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 21,
                singleLocColumn = 10}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_li",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "li",
              fieldOffset = 384,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 22,
                singleLocColumn = 14}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sl",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "sl",
              fieldOffset = 448,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 23,
                singleLocColumn = 17}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sli",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "sli",
              fieldOffset = 512,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 24,
                singleLocColumn = 21}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ul",
          fieldType = HsPrimType
            HsPrimCULong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ul",
              fieldOffset = 576,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 26,
                singleLocColumn = 19}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_uli",
          fieldType = HsPrimType
            HsPrimCULong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "uli",
              fieldOffset = 640,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 27,
                singleLocColumn = 23}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ll",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ll",
              fieldOffset = 704,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 29,
                singleLocColumn = 15}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_lli",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "lli",
              fieldOffset = 768,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 30,
                singleLocColumn = 19}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sll",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "sll",
              fieldOffset = 832,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 31,
                singleLocColumn = 22}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_slli",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "slli",
              fieldOffset = 896,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 32,
                singleLocColumn = 26}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ull",
          fieldType = HsPrimType
            HsPrimCULLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ull",
              fieldOffset = 960,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 34,
                singleLocColumn = 24}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ulli",
          fieldType = HsPrimType
            HsPrimCULLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ulli",
              fieldOffset = 1024,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 35,
                singleLocColumn = 28}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_f",
          fieldType = HsPrimType
            HsPrimCFloat,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "f",
              fieldOffset = 1088,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 37,
                singleLocColumn = 11}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_d",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "d",
              fieldOffset = 1152,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 38,
                singleLocColumn = 12}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ld",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ld",
              fieldOffset = 1280,
              fieldType = TypePrim
                (PrimFloating PrimLongDouble),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 39,
                singleLocColumn = 17}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag
              (CName "primitive"))
            DeclPathTop,
          structSizeof = 176,
          structAlignment = 16,
          structFields = [
            StructField {
              fieldName = CName "c",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 2,
                singleLocColumn = 10}},
            StructField {
              fieldName = CName "sc",
              fieldOffset = 8,
              fieldType = TypePrim
                (PrimChar (Just Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 3,
                singleLocColumn = 17}},
            StructField {
              fieldName = CName "uc",
              fieldOffset = 16,
              fieldType = TypePrim
                (PrimChar (Just Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 4,
                singleLocColumn = 19}},
            StructField {
              fieldName = CName "s",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 6,
                singleLocColumn = 11}},
            StructField {
              fieldName = CName "si",
              fieldOffset = 48,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 7,
                singleLocColumn = 15}},
            StructField {
              fieldName = CName "ss",
              fieldOffset = 64,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 8,
                singleLocColumn = 18}},
            StructField {
              fieldName = CName "ssi",
              fieldOffset = 80,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 9,
                singleLocColumn = 22}},
            StructField {
              fieldName = CName "us",
              fieldOffset = 96,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 11,
                singleLocColumn = 20}},
            StructField {
              fieldName = CName "usi",
              fieldOffset = 112,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimShort Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 12,
                singleLocColumn = 24}},
            StructField {
              fieldName = CName "i",
              fieldOffset = 128,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 14,
                singleLocColumn = 9}},
            StructField {
              fieldName = CName "s2",
              fieldOffset = 160,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 15,
                singleLocColumn = 12}},
            StructField {
              fieldName = CName "si2",
              fieldOffset = 192,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 16,
                singleLocColumn = 16}},
            StructField {
              fieldName = CName "u",
              fieldOffset = 224,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimInt Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 18,
                singleLocColumn = 14}},
            StructField {
              fieldName = CName "ui",
              fieldOffset = 256,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimInt Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 19,
                singleLocColumn = 18}},
            StructField {
              fieldName = CName "l",
              fieldOffset = 320,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 21,
                singleLocColumn = 10}},
            StructField {
              fieldName = CName "li",
              fieldOffset = 384,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 22,
                singleLocColumn = 14}},
            StructField {
              fieldName = CName "sl",
              fieldOffset = 448,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 23,
                singleLocColumn = 17}},
            StructField {
              fieldName = CName "sli",
              fieldOffset = 512,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 24,
                singleLocColumn = 21}},
            StructField {
              fieldName = CName "ul",
              fieldOffset = 576,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 26,
                singleLocColumn = 19}},
            StructField {
              fieldName = CName "uli",
              fieldOffset = 640,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLong Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 27,
                singleLocColumn = 23}},
            StructField {
              fieldName = CName "ll",
              fieldOffset = 704,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 29,
                singleLocColumn = 15}},
            StructField {
              fieldName = CName "lli",
              fieldOffset = 768,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 30,
                singleLocColumn = 19}},
            StructField {
              fieldName = CName "sll",
              fieldOffset = 832,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 31,
                singleLocColumn = 22}},
            StructField {
              fieldName = CName "slli",
              fieldOffset = 896,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 32,
                singleLocColumn = 26}},
            StructField {
              fieldName = CName "ull",
              fieldOffset = 960,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 34,
                singleLocColumn = 24}},
            StructField {
              fieldName = CName "ulli",
              fieldOffset = 1024,
              fieldType = TypePrim
                (PrimIntegral
                  (PrimLongLong Unsigned)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 35,
                singleLocColumn = 28}},
            StructField {
              fieldName = CName "f",
              fieldOffset = 1088,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 37,
                singleLocColumn = 11}},
            StructField {
              fieldName = CName "d",
              fieldOffset = 1152,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 38,
                singleLocColumn = 12}},
            StructField {
              fieldName = CName "ld",
              fieldOffset = 1280,
              fieldType = TypePrim
                (PrimFloating PrimLongDouble),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "primitive_types.h"],
                singleLocLine = 39,
                singleLocColumn = 17}}],
          structFlam = Nothing,
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "primitive_types.h"],
            singleLocLine = 1,
            singleLocColumn = 8}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Primitive",
        structConstr = HsName
          "@NsConstr"
          "Primitive",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_c",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "c",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 2,
                  singleLocColumn = 10}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_sc",
            fieldType = HsPrimType
              HsPrimCSChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "sc",
                fieldOffset = 8,
                fieldType = TypePrim
                  (PrimChar (Just Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 3,
                  singleLocColumn = 17}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_uc",
            fieldType = HsPrimType
              HsPrimCSChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "uc",
                fieldOffset = 16,
                fieldType = TypePrim
                  (PrimChar (Just Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 4,
                  singleLocColumn = 19}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_s",
            fieldType = HsPrimType
              HsPrimCShort,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "s",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 6,
                  singleLocColumn = 11}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_si",
            fieldType = HsPrimType
              HsPrimCShort,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "si",
                fieldOffset = 48,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 7,
                  singleLocColumn = 15}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ss",
            fieldType = HsPrimType
              HsPrimCShort,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ss",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 8,
                  singleLocColumn = 18}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ssi",
            fieldType = HsPrimType
              HsPrimCShort,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ssi",
                fieldOffset = 80,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 9,
                  singleLocColumn = 22}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_us",
            fieldType = HsPrimType
              HsPrimCUShort,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "us",
                fieldOffset = 96,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 11,
                  singleLocColumn = 20}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_usi",
            fieldType = HsPrimType
              HsPrimCUShort,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "usi",
                fieldOffset = 112,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 12,
                  singleLocColumn = 24}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "i",
                fieldOffset = 128,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 14,
                  singleLocColumn = 9}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_s2",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "s2",
                fieldOffset = 160,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 15,
                  singleLocColumn = 12}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_si2",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "si2",
                fieldOffset = 192,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 16,
                  singleLocColumn = 16}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_u",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "u",
                fieldOffset = 224,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimInt Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 18,
                  singleLocColumn = 14}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ui",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ui",
                fieldOffset = 256,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimInt Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 19,
                  singleLocColumn = 18}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_l",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "l",
                fieldOffset = 320,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 21,
                  singleLocColumn = 10}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_li",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "li",
                fieldOffset = 384,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 22,
                  singleLocColumn = 14}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_sl",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "sl",
                fieldOffset = 448,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 23,
                  singleLocColumn = 17}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_sli",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "sli",
                fieldOffset = 512,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 24,
                  singleLocColumn = 21}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ul",
            fieldType = HsPrimType
              HsPrimCULong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ul",
                fieldOffset = 576,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 26,
                  singleLocColumn = 19}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_uli",
            fieldType = HsPrimType
              HsPrimCULong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "uli",
                fieldOffset = 640,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 27,
                  singleLocColumn = 23}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ll",
            fieldType = HsPrimType
              HsPrimCLLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ll",
                fieldOffset = 704,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 29,
                  singleLocColumn = 15}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_lli",
            fieldType = HsPrimType
              HsPrimCLLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "lli",
                fieldOffset = 768,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 30,
                  singleLocColumn = 19}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_sll",
            fieldType = HsPrimType
              HsPrimCLLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "sll",
                fieldOffset = 832,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 31,
                  singleLocColumn = 22}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_slli",
            fieldType = HsPrimType
              HsPrimCLLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "slli",
                fieldOffset = 896,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 32,
                  singleLocColumn = 26}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ull",
            fieldType = HsPrimType
              HsPrimCULLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ull",
                fieldOffset = 960,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 34,
                  singleLocColumn = 24}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ulli",
            fieldType = HsPrimType
              HsPrimCULLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ulli",
                fieldOffset = 1024,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 35,
                  singleLocColumn = 28}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_f",
            fieldType = HsPrimType
              HsPrimCFloat,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "f",
                fieldOffset = 1088,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 37,
                  singleLocColumn = 11}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_d",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "d",
                fieldOffset = 1152,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 38,
                  singleLocColumn = 12}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ld",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ld",
                fieldOffset = 1280,
                fieldType = TypePrim
                  (PrimFloating PrimLongDouble),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 39,
                  singleLocColumn = 17}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag
                (CName "primitive"))
              DeclPathTop,
            structSizeof = 176,
            structAlignment = 16,
            structFields = [
              StructField {
                fieldName = CName "c",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 2,
                  singleLocColumn = 10}},
              StructField {
                fieldName = CName "sc",
                fieldOffset = 8,
                fieldType = TypePrim
                  (PrimChar (Just Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 3,
                  singleLocColumn = 17}},
              StructField {
                fieldName = CName "uc",
                fieldOffset = 16,
                fieldType = TypePrim
                  (PrimChar (Just Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 4,
                  singleLocColumn = 19}},
              StructField {
                fieldName = CName "s",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 6,
                  singleLocColumn = 11}},
              StructField {
                fieldName = CName "si",
                fieldOffset = 48,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 7,
                  singleLocColumn = 15}},
              StructField {
                fieldName = CName "ss",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 8,
                  singleLocColumn = 18}},
              StructField {
                fieldName = CName "ssi",
                fieldOffset = 80,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 9,
                  singleLocColumn = 22}},
              StructField {
                fieldName = CName "us",
                fieldOffset = 96,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 11,
                  singleLocColumn = 20}},
              StructField {
                fieldName = CName "usi",
                fieldOffset = 112,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimShort Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 12,
                  singleLocColumn = 24}},
              StructField {
                fieldName = CName "i",
                fieldOffset = 128,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 14,
                  singleLocColumn = 9}},
              StructField {
                fieldName = CName "s2",
                fieldOffset = 160,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 15,
                  singleLocColumn = 12}},
              StructField {
                fieldName = CName "si2",
                fieldOffset = 192,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 16,
                  singleLocColumn = 16}},
              StructField {
                fieldName = CName "u",
                fieldOffset = 224,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimInt Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 18,
                  singleLocColumn = 14}},
              StructField {
                fieldName = CName "ui",
                fieldOffset = 256,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimInt Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 19,
                  singleLocColumn = 18}},
              StructField {
                fieldName = CName "l",
                fieldOffset = 320,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 21,
                  singleLocColumn = 10}},
              StructField {
                fieldName = CName "li",
                fieldOffset = 384,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 22,
                  singleLocColumn = 14}},
              StructField {
                fieldName = CName "sl",
                fieldOffset = 448,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 23,
                  singleLocColumn = 17}},
              StructField {
                fieldName = CName "sli",
                fieldOffset = 512,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 24,
                  singleLocColumn = 21}},
              StructField {
                fieldName = CName "ul",
                fieldOffset = 576,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 26,
                  singleLocColumn = 19}},
              StructField {
                fieldName = CName "uli",
                fieldOffset = 640,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLong Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 27,
                  singleLocColumn = 23}},
              StructField {
                fieldName = CName "ll",
                fieldOffset = 704,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 29,
                  singleLocColumn = 15}},
              StructField {
                fieldName = CName "lli",
                fieldOffset = 768,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 30,
                  singleLocColumn = 19}},
              StructField {
                fieldName = CName "sll",
                fieldOffset = 832,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 31,
                  singleLocColumn = 22}},
              StructField {
                fieldName = CName "slli",
                fieldOffset = 896,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 32,
                  singleLocColumn = 26}},
              StructField {
                fieldName = CName "ull",
                fieldOffset = 960,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 34,
                  singleLocColumn = 24}},
              StructField {
                fieldName = CName "ulli",
                fieldOffset = 1024,
                fieldType = TypePrim
                  (PrimIntegral
                    (PrimLongLong Unsigned)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 35,
                  singleLocColumn = 28}},
              StructField {
                fieldName = CName "f",
                fieldOffset = 1088,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 37,
                  singleLocColumn = 11}},
              StructField {
                fieldName = CName "d",
                fieldOffset = 1152,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 38,
                  singleLocColumn = 12}},
              StructField {
                fieldName = CName "ld",
                fieldOffset = 1280,
                fieldType = TypePrim
                  (PrimFloating PrimLongDouble),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "primitive_types.h"],
                  singleLocLine = 39,
                  singleLocColumn = 17}}],
            structFlam = Nothing,
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "primitive_types.h"],
              singleLocLine = 1,
              singleLocColumn = 8}}}
      StorableInstance {
        storableSizeOf = 176,
        storableAlignment = 16,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Primitive",
                structConstr = HsName
                  "@NsConstr"
                  "Primitive",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 2,
                          singleLocColumn = 10}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sc",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sc",
                        fieldOffset = 8,
                        fieldType = TypePrim
                          (PrimChar (Just Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 3,
                          singleLocColumn = 17}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_uc",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "uc",
                        fieldOffset = 16,
                        fieldType = TypePrim
                          (PrimChar (Just Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 4,
                          singleLocColumn = 19}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_s",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "s",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 6,
                          singleLocColumn = 11}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_si",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "si",
                        fieldOffset = 48,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 7,
                          singleLocColumn = 15}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ss",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ss",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 8,
                          singleLocColumn = 18}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ssi",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ssi",
                        fieldOffset = 80,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 9,
                          singleLocColumn = 22}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_us",
                    fieldType = HsPrimType
                      HsPrimCUShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "us",
                        fieldOffset = 96,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 11,
                          singleLocColumn = 20}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_usi",
                    fieldType = HsPrimType
                      HsPrimCUShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "usi",
                        fieldOffset = 112,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 12,
                          singleLocColumn = 24}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "i",
                        fieldOffset = 128,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 14,
                          singleLocColumn = 9}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_s2",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "s2",
                        fieldOffset = 160,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 15,
                          singleLocColumn = 12}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_si2",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "si2",
                        fieldOffset = 192,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 16,
                          singleLocColumn = 16}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_u",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "u",
                        fieldOffset = 224,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimInt Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 18,
                          singleLocColumn = 14}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ui",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ui",
                        fieldOffset = 256,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimInt Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 19,
                          singleLocColumn = 18}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_l",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "l",
                        fieldOffset = 320,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 21,
                          singleLocColumn = 10}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_li",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "li",
                        fieldOffset = 384,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 22,
                          singleLocColumn = 14}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sl",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sl",
                        fieldOffset = 448,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 23,
                          singleLocColumn = 17}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sli",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sli",
                        fieldOffset = 512,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 24,
                          singleLocColumn = 21}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ul",
                    fieldType = HsPrimType
                      HsPrimCULong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ul",
                        fieldOffset = 576,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 26,
                          singleLocColumn = 19}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_uli",
                    fieldType = HsPrimType
                      HsPrimCULong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "uli",
                        fieldOffset = 640,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 27,
                          singleLocColumn = 23}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ll",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ll",
                        fieldOffset = 704,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 29,
                          singleLocColumn = 15}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_lli",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "lli",
                        fieldOffset = 768,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 30,
                          singleLocColumn = 19}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sll",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sll",
                        fieldOffset = 832,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 31,
                          singleLocColumn = 22}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_slli",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "slli",
                        fieldOffset = 896,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 32,
                          singleLocColumn = 26}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ull",
                    fieldType = HsPrimType
                      HsPrimCULLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ull",
                        fieldOffset = 960,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 34,
                          singleLocColumn = 24}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ulli",
                    fieldType = HsPrimType
                      HsPrimCULLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ulli",
                        fieldOffset = 1024,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 35,
                          singleLocColumn = 28}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_f",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "f",
                        fieldOffset = 1088,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 37,
                          singleLocColumn = 11}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_d",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 1152,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 38,
                          singleLocColumn = 12}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ld",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ld",
                        fieldOffset = 1280,
                        fieldType = TypePrim
                          (PrimFloating PrimLongDouble),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 39,
                          singleLocColumn = 17}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "primitive"))
                      DeclPathTop,
                    structSizeof = 176,
                    structAlignment = 16,
                    structFields = [
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 2,
                          singleLocColumn = 10}},
                      StructField {
                        fieldName = CName "sc",
                        fieldOffset = 8,
                        fieldType = TypePrim
                          (PrimChar (Just Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 3,
                          singleLocColumn = 17}},
                      StructField {
                        fieldName = CName "uc",
                        fieldOffset = 16,
                        fieldType = TypePrim
                          (PrimChar (Just Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 4,
                          singleLocColumn = 19}},
                      StructField {
                        fieldName = CName "s",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 6,
                          singleLocColumn = 11}},
                      StructField {
                        fieldName = CName "si",
                        fieldOffset = 48,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 7,
                          singleLocColumn = 15}},
                      StructField {
                        fieldName = CName "ss",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 8,
                          singleLocColumn = 18}},
                      StructField {
                        fieldName = CName "ssi",
                        fieldOffset = 80,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 9,
                          singleLocColumn = 22}},
                      StructField {
                        fieldName = CName "us",
                        fieldOffset = 96,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 11,
                          singleLocColumn = 20}},
                      StructField {
                        fieldName = CName "usi",
                        fieldOffset = 112,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 12,
                          singleLocColumn = 24}},
                      StructField {
                        fieldName = CName "i",
                        fieldOffset = 128,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 14,
                          singleLocColumn = 9}},
                      StructField {
                        fieldName = CName "s2",
                        fieldOffset = 160,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 15,
                          singleLocColumn = 12}},
                      StructField {
                        fieldName = CName "si2",
                        fieldOffset = 192,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 16,
                          singleLocColumn = 16}},
                      StructField {
                        fieldName = CName "u",
                        fieldOffset = 224,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimInt Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 18,
                          singleLocColumn = 14}},
                      StructField {
                        fieldName = CName "ui",
                        fieldOffset = 256,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimInt Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 19,
                          singleLocColumn = 18}},
                      StructField {
                        fieldName = CName "l",
                        fieldOffset = 320,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 21,
                          singleLocColumn = 10}},
                      StructField {
                        fieldName = CName "li",
                        fieldOffset = 384,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 22,
                          singleLocColumn = 14}},
                      StructField {
                        fieldName = CName "sl",
                        fieldOffset = 448,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 23,
                          singleLocColumn = 17}},
                      StructField {
                        fieldName = CName "sli",
                        fieldOffset = 512,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 24,
                          singleLocColumn = 21}},
                      StructField {
                        fieldName = CName "ul",
                        fieldOffset = 576,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 26,
                          singleLocColumn = 19}},
                      StructField {
                        fieldName = CName "uli",
                        fieldOffset = 640,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 27,
                          singleLocColumn = 23}},
                      StructField {
                        fieldName = CName "ll",
                        fieldOffset = 704,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 29,
                          singleLocColumn = 15}},
                      StructField {
                        fieldName = CName "lli",
                        fieldOffset = 768,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 30,
                          singleLocColumn = 19}},
                      StructField {
                        fieldName = CName "sll",
                        fieldOffset = 832,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 31,
                          singleLocColumn = 22}},
                      StructField {
                        fieldName = CName "slli",
                        fieldOffset = 896,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 32,
                          singleLocColumn = 26}},
                      StructField {
                        fieldName = CName "ull",
                        fieldOffset = 960,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 34,
                          singleLocColumn = 24}},
                      StructField {
                        fieldName = CName "ulli",
                        fieldOffset = 1024,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 35,
                          singleLocColumn = 28}},
                      StructField {
                        fieldName = CName "f",
                        fieldOffset = 1088,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 37,
                          singleLocColumn = 11}},
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 1152,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 38,
                          singleLocColumn = 12}},
                      StructField {
                        fieldName = CName "ld",
                        fieldOffset = 1280,
                        fieldType = TypePrim
                          (PrimFloating PrimLongDouble),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 39,
                          singleLocColumn = 17}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "primitive_types.h"],
                      singleLocLine = 1,
                      singleLocColumn = 8}}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 1,
              PeekByteOff (Idx 0) 2,
              PeekByteOff (Idx 0) 4,
              PeekByteOff (Idx 0) 6,
              PeekByteOff (Idx 0) 8,
              PeekByteOff (Idx 0) 10,
              PeekByteOff (Idx 0) 12,
              PeekByteOff (Idx 0) 14,
              PeekByteOff (Idx 0) 16,
              PeekByteOff (Idx 0) 20,
              PeekByteOff (Idx 0) 24,
              PeekByteOff (Idx 0) 28,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 40,
              PeekByteOff (Idx 0) 48,
              PeekByteOff (Idx 0) 56,
              PeekByteOff (Idx 0) 64,
              PeekByteOff (Idx 0) 72,
              PeekByteOff (Idx 0) 80,
              PeekByteOff (Idx 0) 88,
              PeekByteOff (Idx 0) 96,
              PeekByteOff (Idx 0) 104,
              PeekByteOff (Idx 0) 112,
              PeekByteOff (Idx 0) 120,
              PeekByteOff (Idx 0) 128,
              PeekByteOff (Idx 0) 136,
              PeekByteOff (Idx 0) 144,
              PeekByteOff (Idx 0) 160]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Primitive",
                structConstr = HsName
                  "@NsConstr"
                  "Primitive",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 2,
                          singleLocColumn = 10}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sc",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sc",
                        fieldOffset = 8,
                        fieldType = TypePrim
                          (PrimChar (Just Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 3,
                          singleLocColumn = 17}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_uc",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "uc",
                        fieldOffset = 16,
                        fieldType = TypePrim
                          (PrimChar (Just Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 4,
                          singleLocColumn = 19}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_s",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "s",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 6,
                          singleLocColumn = 11}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_si",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "si",
                        fieldOffset = 48,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 7,
                          singleLocColumn = 15}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ss",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ss",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 8,
                          singleLocColumn = 18}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ssi",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ssi",
                        fieldOffset = 80,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 9,
                          singleLocColumn = 22}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_us",
                    fieldType = HsPrimType
                      HsPrimCUShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "us",
                        fieldOffset = 96,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 11,
                          singleLocColumn = 20}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_usi",
                    fieldType = HsPrimType
                      HsPrimCUShort,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "usi",
                        fieldOffset = 112,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 12,
                          singleLocColumn = 24}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "i",
                        fieldOffset = 128,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 14,
                          singleLocColumn = 9}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_s2",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "s2",
                        fieldOffset = 160,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 15,
                          singleLocColumn = 12}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_si2",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "si2",
                        fieldOffset = 192,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 16,
                          singleLocColumn = 16}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_u",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "u",
                        fieldOffset = 224,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimInt Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 18,
                          singleLocColumn = 14}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ui",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ui",
                        fieldOffset = 256,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimInt Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 19,
                          singleLocColumn = 18}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_l",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "l",
                        fieldOffset = 320,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 21,
                          singleLocColumn = 10}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_li",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "li",
                        fieldOffset = 384,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 22,
                          singleLocColumn = 14}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sl",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sl",
                        fieldOffset = 448,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 23,
                          singleLocColumn = 17}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sli",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sli",
                        fieldOffset = 512,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 24,
                          singleLocColumn = 21}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ul",
                    fieldType = HsPrimType
                      HsPrimCULong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ul",
                        fieldOffset = 576,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 26,
                          singleLocColumn = 19}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_uli",
                    fieldType = HsPrimType
                      HsPrimCULong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "uli",
                        fieldOffset = 640,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 27,
                          singleLocColumn = 23}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ll",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ll",
                        fieldOffset = 704,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 29,
                          singleLocColumn = 15}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_lli",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "lli",
                        fieldOffset = 768,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 30,
                          singleLocColumn = 19}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sll",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sll",
                        fieldOffset = 832,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 31,
                          singleLocColumn = 22}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_slli",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "slli",
                        fieldOffset = 896,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 32,
                          singleLocColumn = 26}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ull",
                    fieldType = HsPrimType
                      HsPrimCULLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ull",
                        fieldOffset = 960,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 34,
                          singleLocColumn = 24}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ulli",
                    fieldType = HsPrimType
                      HsPrimCULLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ulli",
                        fieldOffset = 1024,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 35,
                          singleLocColumn = 28}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_f",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "f",
                        fieldOffset = 1088,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 37,
                          singleLocColumn = 11}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_d",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 1152,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 38,
                          singleLocColumn = 12}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ld",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ld",
                        fieldOffset = 1280,
                        fieldType = TypePrim
                          (PrimFloating PrimLongDouble),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 39,
                          singleLocColumn = 17}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "primitive"))
                      DeclPathTop,
                    structSizeof = 176,
                    structAlignment = 16,
                    structFields = [
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 2,
                          singleLocColumn = 10}},
                      StructField {
                        fieldName = CName "sc",
                        fieldOffset = 8,
                        fieldType = TypePrim
                          (PrimChar (Just Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 3,
                          singleLocColumn = 17}},
                      StructField {
                        fieldName = CName "uc",
                        fieldOffset = 16,
                        fieldType = TypePrim
                          (PrimChar (Just Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 4,
                          singleLocColumn = 19}},
                      StructField {
                        fieldName = CName "s",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 6,
                          singleLocColumn = 11}},
                      StructField {
                        fieldName = CName "si",
                        fieldOffset = 48,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 7,
                          singleLocColumn = 15}},
                      StructField {
                        fieldName = CName "ss",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 8,
                          singleLocColumn = 18}},
                      StructField {
                        fieldName = CName "ssi",
                        fieldOffset = 80,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 9,
                          singleLocColumn = 22}},
                      StructField {
                        fieldName = CName "us",
                        fieldOffset = 96,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 11,
                          singleLocColumn = 20}},
                      StructField {
                        fieldName = CName "usi",
                        fieldOffset = 112,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimShort Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 12,
                          singleLocColumn = 24}},
                      StructField {
                        fieldName = CName "i",
                        fieldOffset = 128,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 14,
                          singleLocColumn = 9}},
                      StructField {
                        fieldName = CName "s2",
                        fieldOffset = 160,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 15,
                          singleLocColumn = 12}},
                      StructField {
                        fieldName = CName "si2",
                        fieldOffset = 192,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 16,
                          singleLocColumn = 16}},
                      StructField {
                        fieldName = CName "u",
                        fieldOffset = 224,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimInt Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 18,
                          singleLocColumn = 14}},
                      StructField {
                        fieldName = CName "ui",
                        fieldOffset = 256,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimInt Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 19,
                          singleLocColumn = 18}},
                      StructField {
                        fieldName = CName "l",
                        fieldOffset = 320,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 21,
                          singleLocColumn = 10}},
                      StructField {
                        fieldName = CName "li",
                        fieldOffset = 384,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 22,
                          singleLocColumn = 14}},
                      StructField {
                        fieldName = CName "sl",
                        fieldOffset = 448,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 23,
                          singleLocColumn = 17}},
                      StructField {
                        fieldName = CName "sli",
                        fieldOffset = 512,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 24,
                          singleLocColumn = 21}},
                      StructField {
                        fieldName = CName "ul",
                        fieldOffset = 576,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 26,
                          singleLocColumn = 19}},
                      StructField {
                        fieldName = CName "uli",
                        fieldOffset = 640,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 27,
                          singleLocColumn = 23}},
                      StructField {
                        fieldName = CName "ll",
                        fieldOffset = 704,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 29,
                          singleLocColumn = 15}},
                      StructField {
                        fieldName = CName "lli",
                        fieldOffset = 768,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 30,
                          singleLocColumn = 19}},
                      StructField {
                        fieldName = CName "sll",
                        fieldOffset = 832,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 31,
                          singleLocColumn = 22}},
                      StructField {
                        fieldName = CName "slli",
                        fieldOffset = 896,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 32,
                          singleLocColumn = 26}},
                      StructField {
                        fieldName = CName "ull",
                        fieldOffset = 960,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 34,
                          singleLocColumn = 24}},
                      StructField {
                        fieldName = CName "ulli",
                        fieldOffset = 1024,
                        fieldType = TypePrim
                          (PrimIntegral
                            (PrimLongLong Unsigned)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 35,
                          singleLocColumn = 28}},
                      StructField {
                        fieldName = CName "f",
                        fieldOffset = 1088,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 37,
                          singleLocColumn = 11}},
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 1152,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 38,
                          singleLocColumn = 12}},
                      StructField {
                        fieldName = CName "ld",
                        fieldOffset = 1280,
                        fieldType = TypePrim
                          (PrimFloating PrimLongDouble),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "primitive_types.h"],
                          singleLocLine = 39,
                          singleLocColumn = 17}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "primitive_types.h"],
                      singleLocLine = 1,
                      singleLocColumn = 8}}}
              (Add 29)
              (Seq
                [
                  PokeByteOff (Idx 30) 0 (Idx 0),
                  PokeByteOff (Idx 30) 1 (Idx 1),
                  PokeByteOff (Idx 30) 2 (Idx 2),
                  PokeByteOff (Idx 30) 4 (Idx 3),
                  PokeByteOff (Idx 30) 6 (Idx 4),
                  PokeByteOff (Idx 30) 8 (Idx 5),
                  PokeByteOff (Idx 30) 10 (Idx 6),
                  PokeByteOff (Idx 30) 12 (Idx 7),
                  PokeByteOff (Idx 30) 14 (Idx 8),
                  PokeByteOff (Idx 30) 16 (Idx 9),
                  PokeByteOff
                    (Idx 30)
                    20
                    (Idx 10),
                  PokeByteOff
                    (Idx 30)
                    24
                    (Idx 11),
                  PokeByteOff
                    (Idx 30)
                    28
                    (Idx 12),
                  PokeByteOff
                    (Idx 30)
                    32
                    (Idx 13),
                  PokeByteOff
                    (Idx 30)
                    40
                    (Idx 14),
                  PokeByteOff
                    (Idx 30)
                    48
                    (Idx 15),
                  PokeByteOff
                    (Idx 30)
                    56
                    (Idx 16),
                  PokeByteOff
                    (Idx 30)
                    64
                    (Idx 17),
                  PokeByteOff
                    (Idx 30)
                    72
                    (Idx 18),
                  PokeByteOff
                    (Idx 30)
                    80
                    (Idx 19),
                  PokeByteOff
                    (Idx 30)
                    88
                    (Idx 20),
                  PokeByteOff
                    (Idx 30)
                    96
                    (Idx 21),
                  PokeByteOff
                    (Idx 30)
                    104
                    (Idx 22),
                  PokeByteOff
                    (Idx 30)
                    112
                    (Idx 23),
                  PokeByteOff
                    (Idx 30)
                    120
                    (Idx 24),
                  PokeByteOff
                    (Idx 30)
                    128
                    (Idx 25),
                  PokeByteOff
                    (Idx 30)
                    136
                    (Idx 26),
                  PokeByteOff
                    (Idx 30)
                    144
                    (Idx 27),
                  PokeByteOff
                    (Idx 30)
                    160
                    (Idx 28)])))})]
