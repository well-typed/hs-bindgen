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
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:2:10",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "primitive_c"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sc",
          fieldType = HsPrimType
            HsPrimCSChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:3:17",
              structFieldName = NamePair {
                nameC = CName "sc",
                nameHsIdent = HsIdentifier
                  "primitive_sc"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Signed)),
              structFieldOffset = 8,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_uc",
          fieldType = HsPrimType
            HsPrimCUChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:4:19",
              structFieldName = NamePair {
                nameC = CName "uc",
                nameHsIdent = HsIdentifier
                  "primitive_uc"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)),
              structFieldOffset = 16,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_s",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:6:11",
              structFieldName = NamePair {
                nameC = CName "s",
                nameHsIdent = HsIdentifier
                  "primitive_s"},
              structFieldType = TypePrim
                (PrimIntegral PrimShort Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_si",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:7:15",
              structFieldName = NamePair {
                nameC = CName "si",
                nameHsIdent = HsIdentifier
                  "primitive_si"},
              structFieldType = TypePrim
                (PrimIntegral PrimShort Signed),
              structFieldOffset = 48,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ss",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:8:18",
              structFieldName = NamePair {
                nameC = CName "ss",
                nameHsIdent = HsIdentifier
                  "primitive_ss"},
              structFieldType = TypePrim
                (PrimIntegral PrimShort Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ssi",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:9:22",
              structFieldName = NamePair {
                nameC = CName "ssi",
                nameHsIdent = HsIdentifier
                  "primitive_ssi"},
              structFieldType = TypePrim
                (PrimIntegral PrimShort Signed),
              structFieldOffset = 80,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_us",
          fieldType = HsPrimType
            HsPrimCUShort,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:11:20",
              structFieldName = NamePair {
                nameC = CName "us",
                nameHsIdent = HsIdentifier
                  "primitive_us"},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimShort
                  Unsigned),
              structFieldOffset = 96,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_usi",
          fieldType = HsPrimType
            HsPrimCUShort,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:12:24",
              structFieldName = NamePair {
                nameC = CName "usi",
                nameHsIdent = HsIdentifier
                  "primitive_usi"},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimShort
                  Unsigned),
              structFieldOffset = 112,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:14:9",
              structFieldName = NamePair {
                nameC = CName "i",
                nameHsIdent = HsIdentifier
                  "primitive_i"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 128,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_s2",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:15:12",
              structFieldName = NamePair {
                nameC = CName "s2",
                nameHsIdent = HsIdentifier
                  "primitive_s2"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 160,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_si2",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:16:16",
              structFieldName = NamePair {
                nameC = CName "si2",
                nameHsIdent = HsIdentifier
                  "primitive_si2"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 192,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_u",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:18:14",
              structFieldName = NamePair {
                nameC = CName "u",
                nameHsIdent = HsIdentifier
                  "primitive_u"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 224,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ui",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:19:18",
              structFieldName = NamePair {
                nameC = CName "ui",
                nameHsIdent = HsIdentifier
                  "primitive_ui"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_l",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:21:10",
              structFieldName = NamePair {
                nameC = CName "l",
                nameHsIdent = HsIdentifier
                  "primitive_l"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 320,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_li",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:22:14",
              structFieldName = NamePair {
                nameC = CName "li",
                nameHsIdent = HsIdentifier
                  "primitive_li"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 384,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sl",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:23:17",
              structFieldName = NamePair {
                nameC = CName "sl",
                nameHsIdent = HsIdentifier
                  "primitive_sl"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 448,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sli",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:24:21",
              structFieldName = NamePair {
                nameC = CName "sli",
                nameHsIdent = HsIdentifier
                  "primitive_sli"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 512,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ul",
          fieldType = HsPrimType
            HsPrimCULong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:26:19",
              structFieldName = NamePair {
                nameC = CName "ul",
                nameHsIdent = HsIdentifier
                  "primitive_ul"},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLong
                  Unsigned),
              structFieldOffset = 576,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_uli",
          fieldType = HsPrimType
            HsPrimCULong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:27:23",
              structFieldName = NamePair {
                nameC = CName "uli",
                nameHsIdent = HsIdentifier
                  "primitive_uli"},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLong
                  Unsigned),
              structFieldOffset = 640,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ll",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:29:15",
              structFieldName = NamePair {
                nameC = CName "ll",
                nameHsIdent = HsIdentifier
                  "primitive_ll"},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Signed),
              structFieldOffset = 704,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_lli",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:30:19",
              structFieldName = NamePair {
                nameC = CName "lli",
                nameHsIdent = HsIdentifier
                  "primitive_lli"},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Signed),
              structFieldOffset = 768,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sll",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:31:22",
              structFieldName = NamePair {
                nameC = CName "sll",
                nameHsIdent = HsIdentifier
                  "primitive_sll"},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Signed),
              structFieldOffset = 832,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_slli",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:32:26",
              structFieldName = NamePair {
                nameC = CName "slli",
                nameHsIdent = HsIdentifier
                  "primitive_slli"},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Signed),
              structFieldOffset = 896,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ull",
          fieldType = HsPrimType
            HsPrimCULLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:34:24",
              structFieldName = NamePair {
                nameC = CName "ull",
                nameHsIdent = HsIdentifier
                  "primitive_ull"},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Unsigned),
              structFieldOffset = 960,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ulli",
          fieldType = HsPrimType
            HsPrimCULLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:35:28",
              structFieldName = NamePair {
                nameC = CName "ulli",
                nameHsIdent = HsIdentifier
                  "primitive_ulli"},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Unsigned),
              structFieldOffset = 1024,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_f",
          fieldType = HsPrimType
            HsPrimCFloat,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:37:11",
              structFieldName = NamePair {
                nameC = CName "f",
                nameHsIdent = HsIdentifier
                  "primitive_f"},
              structFieldType = TypePrim
                (PrimFloating PrimFloat),
              structFieldOffset = 1088,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_d",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "primitive_types.h:38:12",
              structFieldName = NamePair {
                nameC = CName "d",
                nameHsIdent = HsIdentifier
                  "primitive_d"},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 1152,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "primitive_types.h:1:8",
            declId = NamePair {
              nameC = CName "primitive",
              nameHsIdent = HsIdentifier
                "Primitive"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "primitive_types.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Primitive"),
              structSizeof = 152,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "primitive_types.h:2:10",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "primitive_c"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:3:17",
                  structFieldName = NamePair {
                    nameC = CName "sc",
                    nameHsIdent = HsIdentifier
                      "primitive_sc"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Signed)),
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:4:19",
                  structFieldName = NamePair {
                    nameC = CName "uc",
                    nameHsIdent = HsIdentifier
                      "primitive_uc"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Unsigned)),
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:6:11",
                  structFieldName = NamePair {
                    nameC = CName "s",
                    nameHsIdent = HsIdentifier
                      "primitive_s"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:7:15",
                  structFieldName = NamePair {
                    nameC = CName "si",
                    nameHsIdent = HsIdentifier
                      "primitive_si"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 48,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:8:18",
                  structFieldName = NamePair {
                    nameC = CName "ss",
                    nameHsIdent = HsIdentifier
                      "primitive_ss"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:9:22",
                  structFieldName = NamePair {
                    nameC = CName "ssi",
                    nameHsIdent = HsIdentifier
                      "primitive_ssi"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 80,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:11:20",
                  structFieldName = NamePair {
                    nameC = CName "us",
                    nameHsIdent = HsIdentifier
                      "primitive_us"},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimShort
                      Unsigned),
                  structFieldOffset = 96,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:12:24",
                  structFieldName = NamePair {
                    nameC = CName "usi",
                    nameHsIdent = HsIdentifier
                      "primitive_usi"},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimShort
                      Unsigned),
                  structFieldOffset = 112,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:14:9",
                  structFieldName = NamePair {
                    nameC = CName "i",
                    nameHsIdent = HsIdentifier
                      "primitive_i"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:15:12",
                  structFieldName = NamePair {
                    nameC = CName "s2",
                    nameHsIdent = HsIdentifier
                      "primitive_s2"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 160,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:16:16",
                  structFieldName = NamePair {
                    nameC = CName "si2",
                    nameHsIdent = HsIdentifier
                      "primitive_si2"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:18:14",
                  structFieldName = NamePair {
                    nameC = CName "u",
                    nameHsIdent = HsIdentifier
                      "primitive_u"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 224,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:19:18",
                  structFieldName = NamePair {
                    nameC = CName "ui",
                    nameHsIdent = HsIdentifier
                      "primitive_ui"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:21:10",
                  structFieldName = NamePair {
                    nameC = CName "l",
                    nameHsIdent = HsIdentifier
                      "primitive_l"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 320,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:22:14",
                  structFieldName = NamePair {
                    nameC = CName "li",
                    nameHsIdent = HsIdentifier
                      "primitive_li"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 384,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:23:17",
                  structFieldName = NamePair {
                    nameC = CName "sl",
                    nameHsIdent = HsIdentifier
                      "primitive_sl"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 448,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:24:21",
                  structFieldName = NamePair {
                    nameC = CName "sli",
                    nameHsIdent = HsIdentifier
                      "primitive_sli"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 512,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:26:19",
                  structFieldName = NamePair {
                    nameC = CName "ul",
                    nameHsIdent = HsIdentifier
                      "primitive_ul"},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLong
                      Unsigned),
                  structFieldOffset = 576,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:27:23",
                  structFieldName = NamePair {
                    nameC = CName "uli",
                    nameHsIdent = HsIdentifier
                      "primitive_uli"},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLong
                      Unsigned),
                  structFieldOffset = 640,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:29:15",
                  structFieldName = NamePair {
                    nameC = CName "ll",
                    nameHsIdent = HsIdentifier
                      "primitive_ll"},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 704,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:30:19",
                  structFieldName = NamePair {
                    nameC = CName "lli",
                    nameHsIdent = HsIdentifier
                      "primitive_lli"},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 768,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:31:22",
                  structFieldName = NamePair {
                    nameC = CName "sll",
                    nameHsIdent = HsIdentifier
                      "primitive_sll"},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 832,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:32:26",
                  structFieldName = NamePair {
                    nameC = CName "slli",
                    nameHsIdent = HsIdentifier
                      "primitive_slli"},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 896,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:34:24",
                  structFieldName = NamePair {
                    nameC = CName "ull",
                    nameHsIdent = HsIdentifier
                      "primitive_ull"},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Unsigned),
                  structFieldOffset = 960,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:35:28",
                  structFieldName = NamePair {
                    nameC = CName "ulli",
                    nameHsIdent = HsIdentifier
                      "primitive_ulli"},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Unsigned),
                  structFieldOffset = 1024,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:37:11",
                  structFieldName = NamePair {
                    nameC = CName "f",
                    nameHsIdent = HsIdentifier
                      "primitive_f"},
                  structFieldType = TypePrim
                    (PrimFloating PrimFloat),
                  structFieldOffset = 1088,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "primitive_types.h:38:12",
                  structFieldName = NamePair {
                    nameC = CName "d",
                    nameHsIdent = HsIdentifier
                      "primitive_d"},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 1152,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable]},
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
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:2:10",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "primitive_c"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_sc",
            fieldType = HsPrimType
              HsPrimCSChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:3:17",
                structFieldName = NamePair {
                  nameC = CName "sc",
                  nameHsIdent = HsIdentifier
                    "primitive_sc"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignExplicit Signed)),
                structFieldOffset = 8,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_uc",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:4:19",
                structFieldName = NamePair {
                  nameC = CName "uc",
                  nameHsIdent = HsIdentifier
                    "primitive_uc"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignExplicit Unsigned)),
                structFieldOffset = 16,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_s",
            fieldType = HsPrimType
              HsPrimCShort,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:6:11",
                structFieldName = NamePair {
                  nameC = CName "s",
                  nameHsIdent = HsIdentifier
                    "primitive_s"},
                structFieldType = TypePrim
                  (PrimIntegral PrimShort Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_si",
            fieldType = HsPrimType
              HsPrimCShort,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:7:15",
                structFieldName = NamePair {
                  nameC = CName "si",
                  nameHsIdent = HsIdentifier
                    "primitive_si"},
                structFieldType = TypePrim
                  (PrimIntegral PrimShort Signed),
                structFieldOffset = 48,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ss",
            fieldType = HsPrimType
              HsPrimCShort,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:8:18",
                structFieldName = NamePair {
                  nameC = CName "ss",
                  nameHsIdent = HsIdentifier
                    "primitive_ss"},
                structFieldType = TypePrim
                  (PrimIntegral PrimShort Signed),
                structFieldOffset = 64,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ssi",
            fieldType = HsPrimType
              HsPrimCShort,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:9:22",
                structFieldName = NamePair {
                  nameC = CName "ssi",
                  nameHsIdent = HsIdentifier
                    "primitive_ssi"},
                structFieldType = TypePrim
                  (PrimIntegral PrimShort Signed),
                structFieldOffset = 80,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_us",
            fieldType = HsPrimType
              HsPrimCUShort,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:11:20",
                structFieldName = NamePair {
                  nameC = CName "us",
                  nameHsIdent = HsIdentifier
                    "primitive_us"},
                structFieldType = TypePrim
                  (PrimIntegral
                    PrimShort
                    Unsigned),
                structFieldOffset = 96,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_usi",
            fieldType = HsPrimType
              HsPrimCUShort,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:12:24",
                structFieldName = NamePair {
                  nameC = CName "usi",
                  nameHsIdent = HsIdentifier
                    "primitive_usi"},
                structFieldType = TypePrim
                  (PrimIntegral
                    PrimShort
                    Unsigned),
                structFieldOffset = 112,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:14:9",
                structFieldName = NamePair {
                  nameC = CName "i",
                  nameHsIdent = HsIdentifier
                    "primitive_i"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 128,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_s2",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:15:12",
                structFieldName = NamePair {
                  nameC = CName "s2",
                  nameHsIdent = HsIdentifier
                    "primitive_s2"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 160,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_si2",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:16:16",
                structFieldName = NamePair {
                  nameC = CName "si2",
                  nameHsIdent = HsIdentifier
                    "primitive_si2"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 192,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_u",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:18:14",
                structFieldName = NamePair {
                  nameC = CName "u",
                  nameHsIdent = HsIdentifier
                    "primitive_u"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Unsigned),
                structFieldOffset = 224,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ui",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:19:18",
                structFieldName = NamePair {
                  nameC = CName "ui",
                  nameHsIdent = HsIdentifier
                    "primitive_ui"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Unsigned),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_l",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:21:10",
                structFieldName = NamePair {
                  nameC = CName "l",
                  nameHsIdent = HsIdentifier
                    "primitive_l"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 320,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_li",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:22:14",
                structFieldName = NamePair {
                  nameC = CName "li",
                  nameHsIdent = HsIdentifier
                    "primitive_li"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 384,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_sl",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:23:17",
                structFieldName = NamePair {
                  nameC = CName "sl",
                  nameHsIdent = HsIdentifier
                    "primitive_sl"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 448,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_sli",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:24:21",
                structFieldName = NamePair {
                  nameC = CName "sli",
                  nameHsIdent = HsIdentifier
                    "primitive_sli"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 512,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ul",
            fieldType = HsPrimType
              HsPrimCULong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:26:19",
                structFieldName = NamePair {
                  nameC = CName "ul",
                  nameHsIdent = HsIdentifier
                    "primitive_ul"},
                structFieldType = TypePrim
                  (PrimIntegral
                    PrimLong
                    Unsigned),
                structFieldOffset = 576,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_uli",
            fieldType = HsPrimType
              HsPrimCULong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:27:23",
                structFieldName = NamePair {
                  nameC = CName "uli",
                  nameHsIdent = HsIdentifier
                    "primitive_uli"},
                structFieldType = TypePrim
                  (PrimIntegral
                    PrimLong
                    Unsigned),
                structFieldOffset = 640,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ll",
            fieldType = HsPrimType
              HsPrimCLLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:29:15",
                structFieldName = NamePair {
                  nameC = CName "ll",
                  nameHsIdent = HsIdentifier
                    "primitive_ll"},
                structFieldType = TypePrim
                  (PrimIntegral
                    PrimLongLong
                    Signed),
                structFieldOffset = 704,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_lli",
            fieldType = HsPrimType
              HsPrimCLLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:30:19",
                structFieldName = NamePair {
                  nameC = CName "lli",
                  nameHsIdent = HsIdentifier
                    "primitive_lli"},
                structFieldType = TypePrim
                  (PrimIntegral
                    PrimLongLong
                    Signed),
                structFieldOffset = 768,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_sll",
            fieldType = HsPrimType
              HsPrimCLLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:31:22",
                structFieldName = NamePair {
                  nameC = CName "sll",
                  nameHsIdent = HsIdentifier
                    "primitive_sll"},
                structFieldType = TypePrim
                  (PrimIntegral
                    PrimLongLong
                    Signed),
                structFieldOffset = 832,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_slli",
            fieldType = HsPrimType
              HsPrimCLLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:32:26",
                structFieldName = NamePair {
                  nameC = CName "slli",
                  nameHsIdent = HsIdentifier
                    "primitive_slli"},
                structFieldType = TypePrim
                  (PrimIntegral
                    PrimLongLong
                    Signed),
                structFieldOffset = 896,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ull",
            fieldType = HsPrimType
              HsPrimCULLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:34:24",
                structFieldName = NamePair {
                  nameC = CName "ull",
                  nameHsIdent = HsIdentifier
                    "primitive_ull"},
                structFieldType = TypePrim
                  (PrimIntegral
                    PrimLongLong
                    Unsigned),
                structFieldOffset = 960,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_ulli",
            fieldType = HsPrimType
              HsPrimCULLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:35:28",
                structFieldName = NamePair {
                  nameC = CName "ulli",
                  nameHsIdent = HsIdentifier
                    "primitive_ulli"},
                structFieldType = TypePrim
                  (PrimIntegral
                    PrimLongLong
                    Unsigned),
                structFieldOffset = 1024,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_f",
            fieldType = HsPrimType
              HsPrimCFloat,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:37:11",
                structFieldName = NamePair {
                  nameC = CName "f",
                  nameHsIdent = HsIdentifier
                    "primitive_f"},
                structFieldType = TypePrim
                  (PrimFloating PrimFloat),
                structFieldOffset = 1088,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "primitive_d",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "primitive_types.h:38:12",
                structFieldName = NamePair {
                  nameC = CName "d",
                  nameHsIdent = HsIdentifier
                    "primitive_d"},
                structFieldType = TypePrim
                  (PrimFloating PrimDouble),
                structFieldOffset = 1152,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "primitive_types.h:1:8",
              declId = NamePair {
                nameC = CName "primitive",
                nameHsIdent = HsIdentifier
                  "Primitive"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "primitive_types.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Primitive"),
                structSizeof = 152,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:2:10",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "primitive_c"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:3:17",
                    structFieldName = NamePair {
                      nameC = CName "sc",
                      nameHsIdent = HsIdentifier
                        "primitive_sc"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignExplicit Signed)),
                    structFieldOffset = 8,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:4:19",
                    structFieldName = NamePair {
                      nameC = CName "uc",
                      nameHsIdent = HsIdentifier
                        "primitive_uc"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignExplicit Unsigned)),
                    structFieldOffset = 16,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:6:11",
                    structFieldName = NamePair {
                      nameC = CName "s",
                      nameHsIdent = HsIdentifier
                        "primitive_s"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimShort Signed),
                    structFieldOffset = 32,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:7:15",
                    structFieldName = NamePair {
                      nameC = CName "si",
                      nameHsIdent = HsIdentifier
                        "primitive_si"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimShort Signed),
                    structFieldOffset = 48,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:8:18",
                    structFieldName = NamePair {
                      nameC = CName "ss",
                      nameHsIdent = HsIdentifier
                        "primitive_ss"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimShort Signed),
                    structFieldOffset = 64,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:9:22",
                    structFieldName = NamePair {
                      nameC = CName "ssi",
                      nameHsIdent = HsIdentifier
                        "primitive_ssi"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimShort Signed),
                    structFieldOffset = 80,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:11:20",
                    structFieldName = NamePair {
                      nameC = CName "us",
                      nameHsIdent = HsIdentifier
                        "primitive_us"},
                    structFieldType = TypePrim
                      (PrimIntegral
                        PrimShort
                        Unsigned),
                    structFieldOffset = 96,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:12:24",
                    structFieldName = NamePair {
                      nameC = CName "usi",
                      nameHsIdent = HsIdentifier
                        "primitive_usi"},
                    structFieldType = TypePrim
                      (PrimIntegral
                        PrimShort
                        Unsigned),
                    structFieldOffset = 112,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:14:9",
                    structFieldName = NamePair {
                      nameC = CName "i",
                      nameHsIdent = HsIdentifier
                        "primitive_i"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 128,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:15:12",
                    structFieldName = NamePair {
                      nameC = CName "s2",
                      nameHsIdent = HsIdentifier
                        "primitive_s2"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 160,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:16:16",
                    structFieldName = NamePair {
                      nameC = CName "si2",
                      nameHsIdent = HsIdentifier
                        "primitive_si2"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 192,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:18:14",
                    structFieldName = NamePair {
                      nameC = CName "u",
                      nameHsIdent = HsIdentifier
                        "primitive_u"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    structFieldOffset = 224,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:19:18",
                    structFieldName = NamePair {
                      nameC = CName "ui",
                      nameHsIdent = HsIdentifier
                        "primitive_ui"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:21:10",
                    structFieldName = NamePair {
                      nameC = CName "l",
                      nameHsIdent = HsIdentifier
                        "primitive_l"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 320,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:22:14",
                    structFieldName = NamePair {
                      nameC = CName "li",
                      nameHsIdent = HsIdentifier
                        "primitive_li"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 384,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:23:17",
                    structFieldName = NamePair {
                      nameC = CName "sl",
                      nameHsIdent = HsIdentifier
                        "primitive_sl"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 448,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:24:21",
                    structFieldName = NamePair {
                      nameC = CName "sli",
                      nameHsIdent = HsIdentifier
                        "primitive_sli"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 512,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:26:19",
                    structFieldName = NamePair {
                      nameC = CName "ul",
                      nameHsIdent = HsIdentifier
                        "primitive_ul"},
                    structFieldType = TypePrim
                      (PrimIntegral
                        PrimLong
                        Unsigned),
                    structFieldOffset = 576,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:27:23",
                    structFieldName = NamePair {
                      nameC = CName "uli",
                      nameHsIdent = HsIdentifier
                        "primitive_uli"},
                    structFieldType = TypePrim
                      (PrimIntegral
                        PrimLong
                        Unsigned),
                    structFieldOffset = 640,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:29:15",
                    structFieldName = NamePair {
                      nameC = CName "ll",
                      nameHsIdent = HsIdentifier
                        "primitive_ll"},
                    structFieldType = TypePrim
                      (PrimIntegral
                        PrimLongLong
                        Signed),
                    structFieldOffset = 704,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:30:19",
                    structFieldName = NamePair {
                      nameC = CName "lli",
                      nameHsIdent = HsIdentifier
                        "primitive_lli"},
                    structFieldType = TypePrim
                      (PrimIntegral
                        PrimLongLong
                        Signed),
                    structFieldOffset = 768,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:31:22",
                    structFieldName = NamePair {
                      nameC = CName "sll",
                      nameHsIdent = HsIdentifier
                        "primitive_sll"},
                    structFieldType = TypePrim
                      (PrimIntegral
                        PrimLongLong
                        Signed),
                    structFieldOffset = 832,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:32:26",
                    structFieldName = NamePair {
                      nameC = CName "slli",
                      nameHsIdent = HsIdentifier
                        "primitive_slli"},
                    structFieldType = TypePrim
                      (PrimIntegral
                        PrimLongLong
                        Signed),
                    structFieldOffset = 896,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:34:24",
                    structFieldName = NamePair {
                      nameC = CName "ull",
                      nameHsIdent = HsIdentifier
                        "primitive_ull"},
                    structFieldType = TypePrim
                      (PrimIntegral
                        PrimLongLong
                        Unsigned),
                    structFieldOffset = 960,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:35:28",
                    structFieldName = NamePair {
                      nameC = CName "ulli",
                      nameHsIdent = HsIdentifier
                        "primitive_ulli"},
                    structFieldType = TypePrim
                      (PrimIntegral
                        PrimLongLong
                        Unsigned),
                    structFieldOffset = 1024,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:37:11",
                    structFieldName = NamePair {
                      nameC = CName "f",
                      nameHsIdent = HsIdentifier
                        "primitive_f"},
                    structFieldType = TypePrim
                      (PrimFloating PrimFloat),
                    structFieldOffset = 1088,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "primitive_types.h:38:12",
                    structFieldName = NamePair {
                      nameC = CName "d",
                      nameHsIdent = HsIdentifier
                        "primitive_d"},
                    structFieldType = TypePrim
                      (PrimFloating PrimDouble),
                    structFieldOffset = 1152,
                    structFieldWidth = Nothing}],
                structFlam = Nothing},
            declSpec = DeclSpec
              TypeSpec {
                typeSpecModule = Nothing,
                typeSpecIdentifier = Nothing,
                typeSpecInstances = Map.fromList
                  []}},
        structInstances = Set.fromList
          [Eq, Show, Storable]}
      StorableInstance {
        storableSizeOf = 152,
        storableAlignment = 8,
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
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:2:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "primitive_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sc",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:3:17",
                        structFieldName = NamePair {
                          nameC = CName "sc",
                          nameHsIdent = HsIdentifier
                            "primitive_sc"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Signed)),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_uc",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:4:19",
                        structFieldName = NamePair {
                          nameC = CName "uc",
                          nameHsIdent = HsIdentifier
                            "primitive_uc"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        structFieldOffset = 16,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_s",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:6:11",
                        structFieldName = NamePair {
                          nameC = CName "s",
                          nameHsIdent = HsIdentifier
                            "primitive_s"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimShort Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_si",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:7:15",
                        structFieldName = NamePair {
                          nameC = CName "si",
                          nameHsIdent = HsIdentifier
                            "primitive_si"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimShort Signed),
                        structFieldOffset = 48,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ss",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:8:18",
                        structFieldName = NamePair {
                          nameC = CName "ss",
                          nameHsIdent = HsIdentifier
                            "primitive_ss"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimShort Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ssi",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:9:22",
                        structFieldName = NamePair {
                          nameC = CName "ssi",
                          nameHsIdent = HsIdentifier
                            "primitive_ssi"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimShort Signed),
                        structFieldOffset = 80,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_us",
                    fieldType = HsPrimType
                      HsPrimCUShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:11:20",
                        structFieldName = NamePair {
                          nameC = CName "us",
                          nameHsIdent = HsIdentifier
                            "primitive_us"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimShort
                            Unsigned),
                        structFieldOffset = 96,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_usi",
                    fieldType = HsPrimType
                      HsPrimCUShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:12:24",
                        structFieldName = NamePair {
                          nameC = CName "usi",
                          nameHsIdent = HsIdentifier
                            "primitive_usi"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimShort
                            Unsigned),
                        structFieldOffset = 112,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:14:9",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "primitive_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 128,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_s2",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:15:12",
                        structFieldName = NamePair {
                          nameC = CName "s2",
                          nameHsIdent = HsIdentifier
                            "primitive_s2"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 160,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_si2",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:16:16",
                        structFieldName = NamePair {
                          nameC = CName "si2",
                          nameHsIdent = HsIdentifier
                            "primitive_si2"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 192,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_u",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:18:14",
                        structFieldName = NamePair {
                          nameC = CName "u",
                          nameHsIdent = HsIdentifier
                            "primitive_u"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Unsigned),
                        structFieldOffset = 224,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ui",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:19:18",
                        structFieldName = NamePair {
                          nameC = CName "ui",
                          nameHsIdent = HsIdentifier
                            "primitive_ui"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Unsigned),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_l",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:21:10",
                        structFieldName = NamePair {
                          nameC = CName "l",
                          nameHsIdent = HsIdentifier
                            "primitive_l"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 320,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_li",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:22:14",
                        structFieldName = NamePair {
                          nameC = CName "li",
                          nameHsIdent = HsIdentifier
                            "primitive_li"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 384,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sl",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:23:17",
                        structFieldName = NamePair {
                          nameC = CName "sl",
                          nameHsIdent = HsIdentifier
                            "primitive_sl"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 448,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sli",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:24:21",
                        structFieldName = NamePair {
                          nameC = CName "sli",
                          nameHsIdent = HsIdentifier
                            "primitive_sli"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 512,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ul",
                    fieldType = HsPrimType
                      HsPrimCULong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:26:19",
                        structFieldName = NamePair {
                          nameC = CName "ul",
                          nameHsIdent = HsIdentifier
                            "primitive_ul"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLong
                            Unsigned),
                        structFieldOffset = 576,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_uli",
                    fieldType = HsPrimType
                      HsPrimCULong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:27:23",
                        structFieldName = NamePair {
                          nameC = CName "uli",
                          nameHsIdent = HsIdentifier
                            "primitive_uli"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLong
                            Unsigned),
                        structFieldOffset = 640,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ll",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:29:15",
                        structFieldName = NamePair {
                          nameC = CName "ll",
                          nameHsIdent = HsIdentifier
                            "primitive_ll"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Signed),
                        structFieldOffset = 704,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_lli",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:30:19",
                        structFieldName = NamePair {
                          nameC = CName "lli",
                          nameHsIdent = HsIdentifier
                            "primitive_lli"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Signed),
                        structFieldOffset = 768,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sll",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:31:22",
                        structFieldName = NamePair {
                          nameC = CName "sll",
                          nameHsIdent = HsIdentifier
                            "primitive_sll"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Signed),
                        structFieldOffset = 832,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_slli",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:32:26",
                        structFieldName = NamePair {
                          nameC = CName "slli",
                          nameHsIdent = HsIdentifier
                            "primitive_slli"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Signed),
                        structFieldOffset = 896,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ull",
                    fieldType = HsPrimType
                      HsPrimCULLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:34:24",
                        structFieldName = NamePair {
                          nameC = CName "ull",
                          nameHsIdent = HsIdentifier
                            "primitive_ull"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Unsigned),
                        structFieldOffset = 960,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ulli",
                    fieldType = HsPrimType
                      HsPrimCULLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:35:28",
                        structFieldName = NamePair {
                          nameC = CName "ulli",
                          nameHsIdent = HsIdentifier
                            "primitive_ulli"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Unsigned),
                        structFieldOffset = 1024,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_f",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:37:11",
                        structFieldName = NamePair {
                          nameC = CName "f",
                          nameHsIdent = HsIdentifier
                            "primitive_f"},
                        structFieldType = TypePrim
                          (PrimFloating PrimFloat),
                        structFieldOffset = 1088,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_d",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:38:12",
                        structFieldName = NamePair {
                          nameC = CName "d",
                          nameHsIdent = HsIdentifier
                            "primitive_d"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 1152,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "primitive_types.h:1:8",
                      declId = NamePair {
                        nameC = CName "primitive",
                        nameHsIdent = HsIdentifier
                          "Primitive"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "primitive_types.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Primitive"),
                        structSizeof = 152,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:2:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "primitive_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:3:17",
                            structFieldName = NamePair {
                              nameC = CName "sc",
                              nameHsIdent = HsIdentifier
                                "primitive_sc"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignExplicit Signed)),
                            structFieldOffset = 8,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:4:19",
                            structFieldName = NamePair {
                              nameC = CName "uc",
                              nameHsIdent = HsIdentifier
                                "primitive_uc"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignExplicit Unsigned)),
                            structFieldOffset = 16,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:6:11",
                            structFieldName = NamePair {
                              nameC = CName "s",
                              nameHsIdent = HsIdentifier
                                "primitive_s"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimShort Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:7:15",
                            structFieldName = NamePair {
                              nameC = CName "si",
                              nameHsIdent = HsIdentifier
                                "primitive_si"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimShort Signed),
                            structFieldOffset = 48,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:8:18",
                            structFieldName = NamePair {
                              nameC = CName "ss",
                              nameHsIdent = HsIdentifier
                                "primitive_ss"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimShort Signed),
                            structFieldOffset = 64,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:9:22",
                            structFieldName = NamePair {
                              nameC = CName "ssi",
                              nameHsIdent = HsIdentifier
                                "primitive_ssi"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimShort Signed),
                            structFieldOffset = 80,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:11:20",
                            structFieldName = NamePair {
                              nameC = CName "us",
                              nameHsIdent = HsIdentifier
                                "primitive_us"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimShort
                                Unsigned),
                            structFieldOffset = 96,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:12:24",
                            structFieldName = NamePair {
                              nameC = CName "usi",
                              nameHsIdent = HsIdentifier
                                "primitive_usi"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimShort
                                Unsigned),
                            structFieldOffset = 112,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:14:9",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "primitive_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 128,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:15:12",
                            structFieldName = NamePair {
                              nameC = CName "s2",
                              nameHsIdent = HsIdentifier
                                "primitive_s2"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 160,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:16:16",
                            structFieldName = NamePair {
                              nameC = CName "si2",
                              nameHsIdent = HsIdentifier
                                "primitive_si2"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 192,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:18:14",
                            structFieldName = NamePair {
                              nameC = CName "u",
                              nameHsIdent = HsIdentifier
                                "primitive_u"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Unsigned),
                            structFieldOffset = 224,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:19:18",
                            structFieldName = NamePair {
                              nameC = CName "ui",
                              nameHsIdent = HsIdentifier
                                "primitive_ui"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Unsigned),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:21:10",
                            structFieldName = NamePair {
                              nameC = CName "l",
                              nameHsIdent = HsIdentifier
                                "primitive_l"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 320,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:22:14",
                            structFieldName = NamePair {
                              nameC = CName "li",
                              nameHsIdent = HsIdentifier
                                "primitive_li"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 384,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:23:17",
                            structFieldName = NamePair {
                              nameC = CName "sl",
                              nameHsIdent = HsIdentifier
                                "primitive_sl"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 448,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:24:21",
                            structFieldName = NamePair {
                              nameC = CName "sli",
                              nameHsIdent = HsIdentifier
                                "primitive_sli"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 512,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:26:19",
                            structFieldName = NamePair {
                              nameC = CName "ul",
                              nameHsIdent = HsIdentifier
                                "primitive_ul"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLong
                                Unsigned),
                            structFieldOffset = 576,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:27:23",
                            structFieldName = NamePair {
                              nameC = CName "uli",
                              nameHsIdent = HsIdentifier
                                "primitive_uli"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLong
                                Unsigned),
                            structFieldOffset = 640,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:29:15",
                            structFieldName = NamePair {
                              nameC = CName "ll",
                              nameHsIdent = HsIdentifier
                                "primitive_ll"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Signed),
                            structFieldOffset = 704,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:30:19",
                            structFieldName = NamePair {
                              nameC = CName "lli",
                              nameHsIdent = HsIdentifier
                                "primitive_lli"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Signed),
                            structFieldOffset = 768,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:31:22",
                            structFieldName = NamePair {
                              nameC = CName "sll",
                              nameHsIdent = HsIdentifier
                                "primitive_sll"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Signed),
                            structFieldOffset = 832,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:32:26",
                            structFieldName = NamePair {
                              nameC = CName "slli",
                              nameHsIdent = HsIdentifier
                                "primitive_slli"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Signed),
                            structFieldOffset = 896,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:34:24",
                            structFieldName = NamePair {
                              nameC = CName "ull",
                              nameHsIdent = HsIdentifier
                                "primitive_ull"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Unsigned),
                            structFieldOffset = 960,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:35:28",
                            structFieldName = NamePair {
                              nameC = CName "ulli",
                              nameHsIdent = HsIdentifier
                                "primitive_ulli"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Unsigned),
                            structFieldOffset = 1024,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:37:11",
                            structFieldName = NamePair {
                              nameC = CName "f",
                              nameHsIdent = HsIdentifier
                                "primitive_f"},
                            structFieldType = TypePrim
                              (PrimFloating PrimFloat),
                            structFieldOffset = 1088,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:38:12",
                            structFieldName = NamePair {
                              nameC = CName "d",
                              nameHsIdent = HsIdentifier
                                "primitive_d"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 1152,
                            structFieldWidth = Nothing}],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
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
              PeekByteOff (Idx 0) 144]),
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
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:2:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "primitive_c"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sc",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:3:17",
                        structFieldName = NamePair {
                          nameC = CName "sc",
                          nameHsIdent = HsIdentifier
                            "primitive_sc"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Signed)),
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_uc",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:4:19",
                        structFieldName = NamePair {
                          nameC = CName "uc",
                          nameHsIdent = HsIdentifier
                            "primitive_uc"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        structFieldOffset = 16,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_s",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:6:11",
                        structFieldName = NamePair {
                          nameC = CName "s",
                          nameHsIdent = HsIdentifier
                            "primitive_s"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimShort Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_si",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:7:15",
                        structFieldName = NamePair {
                          nameC = CName "si",
                          nameHsIdent = HsIdentifier
                            "primitive_si"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimShort Signed),
                        structFieldOffset = 48,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ss",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:8:18",
                        structFieldName = NamePair {
                          nameC = CName "ss",
                          nameHsIdent = HsIdentifier
                            "primitive_ss"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimShort Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ssi",
                    fieldType = HsPrimType
                      HsPrimCShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:9:22",
                        structFieldName = NamePair {
                          nameC = CName "ssi",
                          nameHsIdent = HsIdentifier
                            "primitive_ssi"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimShort Signed),
                        structFieldOffset = 80,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_us",
                    fieldType = HsPrimType
                      HsPrimCUShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:11:20",
                        structFieldName = NamePair {
                          nameC = CName "us",
                          nameHsIdent = HsIdentifier
                            "primitive_us"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimShort
                            Unsigned),
                        structFieldOffset = 96,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_usi",
                    fieldType = HsPrimType
                      HsPrimCUShort,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:12:24",
                        structFieldName = NamePair {
                          nameC = CName "usi",
                          nameHsIdent = HsIdentifier
                            "primitive_usi"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimShort
                            Unsigned),
                        structFieldOffset = 112,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:14:9",
                        structFieldName = NamePair {
                          nameC = CName "i",
                          nameHsIdent = HsIdentifier
                            "primitive_i"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 128,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_s2",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:15:12",
                        structFieldName = NamePair {
                          nameC = CName "s2",
                          nameHsIdent = HsIdentifier
                            "primitive_s2"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 160,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_si2",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:16:16",
                        structFieldName = NamePair {
                          nameC = CName "si2",
                          nameHsIdent = HsIdentifier
                            "primitive_si2"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 192,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_u",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:18:14",
                        structFieldName = NamePair {
                          nameC = CName "u",
                          nameHsIdent = HsIdentifier
                            "primitive_u"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Unsigned),
                        structFieldOffset = 224,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ui",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:19:18",
                        structFieldName = NamePair {
                          nameC = CName "ui",
                          nameHsIdent = HsIdentifier
                            "primitive_ui"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Unsigned),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_l",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:21:10",
                        structFieldName = NamePair {
                          nameC = CName "l",
                          nameHsIdent = HsIdentifier
                            "primitive_l"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 320,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_li",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:22:14",
                        structFieldName = NamePair {
                          nameC = CName "li",
                          nameHsIdent = HsIdentifier
                            "primitive_li"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 384,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sl",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:23:17",
                        structFieldName = NamePair {
                          nameC = CName "sl",
                          nameHsIdent = HsIdentifier
                            "primitive_sl"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 448,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sli",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:24:21",
                        structFieldName = NamePair {
                          nameC = CName "sli",
                          nameHsIdent = HsIdentifier
                            "primitive_sli"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 512,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ul",
                    fieldType = HsPrimType
                      HsPrimCULong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:26:19",
                        structFieldName = NamePair {
                          nameC = CName "ul",
                          nameHsIdent = HsIdentifier
                            "primitive_ul"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLong
                            Unsigned),
                        structFieldOffset = 576,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_uli",
                    fieldType = HsPrimType
                      HsPrimCULong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:27:23",
                        structFieldName = NamePair {
                          nameC = CName "uli",
                          nameHsIdent = HsIdentifier
                            "primitive_uli"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLong
                            Unsigned),
                        structFieldOffset = 640,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ll",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:29:15",
                        structFieldName = NamePair {
                          nameC = CName "ll",
                          nameHsIdent = HsIdentifier
                            "primitive_ll"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Signed),
                        structFieldOffset = 704,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_lli",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:30:19",
                        structFieldName = NamePair {
                          nameC = CName "lli",
                          nameHsIdent = HsIdentifier
                            "primitive_lli"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Signed),
                        structFieldOffset = 768,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_sll",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:31:22",
                        structFieldName = NamePair {
                          nameC = CName "sll",
                          nameHsIdent = HsIdentifier
                            "primitive_sll"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Signed),
                        structFieldOffset = 832,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_slli",
                    fieldType = HsPrimType
                      HsPrimCLLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:32:26",
                        structFieldName = NamePair {
                          nameC = CName "slli",
                          nameHsIdent = HsIdentifier
                            "primitive_slli"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Signed),
                        structFieldOffset = 896,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ull",
                    fieldType = HsPrimType
                      HsPrimCULLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:34:24",
                        structFieldName = NamePair {
                          nameC = CName "ull",
                          nameHsIdent = HsIdentifier
                            "primitive_ull"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Unsigned),
                        structFieldOffset = 960,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_ulli",
                    fieldType = HsPrimType
                      HsPrimCULLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:35:28",
                        structFieldName = NamePair {
                          nameC = CName "ulli",
                          nameHsIdent = HsIdentifier
                            "primitive_ulli"},
                        structFieldType = TypePrim
                          (PrimIntegral
                            PrimLongLong
                            Unsigned),
                        structFieldOffset = 1024,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_f",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:37:11",
                        structFieldName = NamePair {
                          nameC = CName "f",
                          nameHsIdent = HsIdentifier
                            "primitive_f"},
                        structFieldType = TypePrim
                          (PrimFloating PrimFloat),
                        structFieldOffset = 1088,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "primitive_d",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "primitive_types.h:38:12",
                        structFieldName = NamePair {
                          nameC = CName "d",
                          nameHsIdent = HsIdentifier
                            "primitive_d"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 1152,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "primitive_types.h:1:8",
                      declId = NamePair {
                        nameC = CName "primitive",
                        nameHsIdent = HsIdentifier
                          "Primitive"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "primitive_types.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Primitive"),
                        structSizeof = 152,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:2:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "primitive_c"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:3:17",
                            structFieldName = NamePair {
                              nameC = CName "sc",
                              nameHsIdent = HsIdentifier
                                "primitive_sc"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignExplicit Signed)),
                            structFieldOffset = 8,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:4:19",
                            structFieldName = NamePair {
                              nameC = CName "uc",
                              nameHsIdent = HsIdentifier
                                "primitive_uc"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignExplicit Unsigned)),
                            structFieldOffset = 16,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:6:11",
                            structFieldName = NamePair {
                              nameC = CName "s",
                              nameHsIdent = HsIdentifier
                                "primitive_s"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimShort Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:7:15",
                            structFieldName = NamePair {
                              nameC = CName "si",
                              nameHsIdent = HsIdentifier
                                "primitive_si"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimShort Signed),
                            structFieldOffset = 48,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:8:18",
                            structFieldName = NamePair {
                              nameC = CName "ss",
                              nameHsIdent = HsIdentifier
                                "primitive_ss"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimShort Signed),
                            structFieldOffset = 64,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:9:22",
                            structFieldName = NamePair {
                              nameC = CName "ssi",
                              nameHsIdent = HsIdentifier
                                "primitive_ssi"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimShort Signed),
                            structFieldOffset = 80,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:11:20",
                            structFieldName = NamePair {
                              nameC = CName "us",
                              nameHsIdent = HsIdentifier
                                "primitive_us"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimShort
                                Unsigned),
                            structFieldOffset = 96,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:12:24",
                            structFieldName = NamePair {
                              nameC = CName "usi",
                              nameHsIdent = HsIdentifier
                                "primitive_usi"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimShort
                                Unsigned),
                            structFieldOffset = 112,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:14:9",
                            structFieldName = NamePair {
                              nameC = CName "i",
                              nameHsIdent = HsIdentifier
                                "primitive_i"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 128,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:15:12",
                            structFieldName = NamePair {
                              nameC = CName "s2",
                              nameHsIdent = HsIdentifier
                                "primitive_s2"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 160,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:16:16",
                            structFieldName = NamePair {
                              nameC = CName "si2",
                              nameHsIdent = HsIdentifier
                                "primitive_si2"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 192,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:18:14",
                            structFieldName = NamePair {
                              nameC = CName "u",
                              nameHsIdent = HsIdentifier
                                "primitive_u"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Unsigned),
                            structFieldOffset = 224,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:19:18",
                            structFieldName = NamePair {
                              nameC = CName "ui",
                              nameHsIdent = HsIdentifier
                                "primitive_ui"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Unsigned),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:21:10",
                            structFieldName = NamePair {
                              nameC = CName "l",
                              nameHsIdent = HsIdentifier
                                "primitive_l"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 320,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:22:14",
                            structFieldName = NamePair {
                              nameC = CName "li",
                              nameHsIdent = HsIdentifier
                                "primitive_li"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 384,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:23:17",
                            structFieldName = NamePair {
                              nameC = CName "sl",
                              nameHsIdent = HsIdentifier
                                "primitive_sl"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 448,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:24:21",
                            structFieldName = NamePair {
                              nameC = CName "sli",
                              nameHsIdent = HsIdentifier
                                "primitive_sli"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 512,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:26:19",
                            structFieldName = NamePair {
                              nameC = CName "ul",
                              nameHsIdent = HsIdentifier
                                "primitive_ul"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLong
                                Unsigned),
                            structFieldOffset = 576,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:27:23",
                            structFieldName = NamePair {
                              nameC = CName "uli",
                              nameHsIdent = HsIdentifier
                                "primitive_uli"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLong
                                Unsigned),
                            structFieldOffset = 640,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:29:15",
                            structFieldName = NamePair {
                              nameC = CName "ll",
                              nameHsIdent = HsIdentifier
                                "primitive_ll"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Signed),
                            structFieldOffset = 704,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:30:19",
                            structFieldName = NamePair {
                              nameC = CName "lli",
                              nameHsIdent = HsIdentifier
                                "primitive_lli"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Signed),
                            structFieldOffset = 768,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:31:22",
                            structFieldName = NamePair {
                              nameC = CName "sll",
                              nameHsIdent = HsIdentifier
                                "primitive_sll"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Signed),
                            structFieldOffset = 832,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:32:26",
                            structFieldName = NamePair {
                              nameC = CName "slli",
                              nameHsIdent = HsIdentifier
                                "primitive_slli"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Signed),
                            structFieldOffset = 896,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:34:24",
                            structFieldName = NamePair {
                              nameC = CName "ull",
                              nameHsIdent = HsIdentifier
                                "primitive_ull"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Unsigned),
                            structFieldOffset = 960,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:35:28",
                            structFieldName = NamePair {
                              nameC = CName "ulli",
                              nameHsIdent = HsIdentifier
                                "primitive_ulli"},
                            structFieldType = TypePrim
                              (PrimIntegral
                                PrimLongLong
                                Unsigned),
                            structFieldOffset = 1024,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:37:11",
                            structFieldName = NamePair {
                              nameC = CName "f",
                              nameHsIdent = HsIdentifier
                                "primitive_f"},
                            structFieldType = TypePrim
                              (PrimFloating PrimFloat),
                            structFieldOffset = 1088,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "primitive_types.h:38:12",
                            structFieldName = NamePair {
                              nameC = CName "d",
                              nameHsIdent = HsIdentifier
                                "primitive_d"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 1152,
                            structFieldWidth = Nothing}],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 28)
              (Seq
                [
                  PokeByteOff (Idx 29) 0 (Idx 0),
                  PokeByteOff (Idx 29) 1 (Idx 1),
                  PokeByteOff (Idx 29) 2 (Idx 2),
                  PokeByteOff (Idx 29) 4 (Idx 3),
                  PokeByteOff (Idx 29) 6 (Idx 4),
                  PokeByteOff (Idx 29) 8 (Idx 5),
                  PokeByteOff (Idx 29) 10 (Idx 6),
                  PokeByteOff (Idx 29) 12 (Idx 7),
                  PokeByteOff (Idx 29) 14 (Idx 8),
                  PokeByteOff (Idx 29) 16 (Idx 9),
                  PokeByteOff
                    (Idx 29)
                    20
                    (Idx 10),
                  PokeByteOff
                    (Idx 29)
                    24
                    (Idx 11),
                  PokeByteOff
                    (Idx 29)
                    28
                    (Idx 12),
                  PokeByteOff
                    (Idx 29)
                    32
                    (Idx 13),
                  PokeByteOff
                    (Idx 29)
                    40
                    (Idx 14),
                  PokeByteOff
                    (Idx 29)
                    48
                    (Idx 15),
                  PokeByteOff
                    (Idx 29)
                    56
                    (Idx 16),
                  PokeByteOff
                    (Idx 29)
                    64
                    (Idx 17),
                  PokeByteOff
                    (Idx 29)
                    72
                    (Idx 18),
                  PokeByteOff
                    (Idx 29)
                    80
                    (Idx 19),
                  PokeByteOff
                    (Idx 29)
                    88
                    (Idx 20),
                  PokeByteOff
                    (Idx 29)
                    96
                    (Idx 21),
                  PokeByteOff
                    (Idx 29)
                    104
                    (Idx 22),
                  PokeByteOff
                    (Idx 29)
                    112
                    (Idx 23),
                  PokeByteOff
                    (Idx 29)
                    120
                    (Idx 24),
                  PokeByteOff
                    (Idx 29)
                    128
                    (Idx 25),
                  PokeByteOff
                    (Idx 29)
                    136
                    (Idx 26),
                  PokeByteOff
                    (Idx 29)
                    144
                    (Idx 27)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Primitive"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Primitive")]
