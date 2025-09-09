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
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:2:10",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = HsIdentifier
                    "primitive_c"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "c",
              commentLocation = Just
                "primitive_types.h:2:10",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sc",
          fieldType = HsPrimType
            HsPrimCSChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:3:17",
                fieldName = NamePair {
                  nameC = Name "sc",
                  nameHsIdent = HsIdentifier
                    "primitive_sc"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Signed)),
              structFieldOffset = 8,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "sc",
              commentLocation = Just
                "primitive_types.h:3:17",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_uc",
          fieldType = HsPrimType
            HsPrimCUChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:4:19",
                fieldName = NamePair {
                  nameC = Name "uc",
                  nameHsIdent = HsIdentifier
                    "primitive_uc"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)),
              structFieldOffset = 16,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "uc",
              commentLocation = Just
                "primitive_types.h:4:19",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_s",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:6:11",
                fieldName = NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier
                    "primitive_s"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimShort Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Just
                "primitive_types.h:6:11",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_si",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:7:15",
                fieldName = NamePair {
                  nameC = Name "si",
                  nameHsIdent = HsIdentifier
                    "primitive_si"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimShort Signed),
              structFieldOffset = 48,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "si",
              commentLocation = Just
                "primitive_types.h:7:15",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ss",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:8:18",
                fieldName = NamePair {
                  nameC = Name "ss",
                  nameHsIdent = HsIdentifier
                    "primitive_ss"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimShort Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ss",
              commentLocation = Just
                "primitive_types.h:8:18",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ssi",
          fieldType = HsPrimType
            HsPrimCShort,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:9:22",
                fieldName = NamePair {
                  nameC = Name "ssi",
                  nameHsIdent = HsIdentifier
                    "primitive_ssi"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimShort Signed),
              structFieldOffset = 80,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ssi",
              commentLocation = Just
                "primitive_types.h:9:22",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_us",
          fieldType = HsPrimType
            HsPrimCUShort,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:11:20",
                fieldName = NamePair {
                  nameC = Name "us",
                  nameHsIdent = HsIdentifier
                    "primitive_us"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimShort
                  Unsigned),
              structFieldOffset = 96,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "us",
              commentLocation = Just
                "primitive_types.h:11:20",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_usi",
          fieldType = HsPrimType
            HsPrimCUShort,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:12:24",
                fieldName = NamePair {
                  nameC = Name "usi",
                  nameHsIdent = HsIdentifier
                    "primitive_usi"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimShort
                  Unsigned),
              structFieldOffset = 112,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "usi",
              commentLocation = Just
                "primitive_types.h:12:24",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:14:9",
                fieldName = NamePair {
                  nameC = Name "i",
                  nameHsIdent = HsIdentifier
                    "primitive_i"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 128,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Just
                "primitive_types.h:14:9",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_s2",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:15:12",
                fieldName = NamePair {
                  nameC = Name "s2",
                  nameHsIdent = HsIdentifier
                    "primitive_s2"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 160,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s2",
              commentLocation = Just
                "primitive_types.h:15:12",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_si2",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:16:16",
                fieldName = NamePair {
                  nameC = Name "si2",
                  nameHsIdent = HsIdentifier
                    "primitive_si2"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 192,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "si2",
              commentLocation = Just
                "primitive_types.h:16:16",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_u",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:18:14",
                fieldName = NamePair {
                  nameC = Name "u",
                  nameHsIdent = HsIdentifier
                    "primitive_u"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 224,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "u",
              commentLocation = Just
                "primitive_types.h:18:14",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ui",
          fieldType = HsPrimType
            HsPrimCUInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:19:18",
                fieldName = NamePair {
                  nameC = Name "ui",
                  nameHsIdent = HsIdentifier
                    "primitive_ui"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Unsigned),
              structFieldOffset = 256,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ui",
              commentLocation = Just
                "primitive_types.h:19:18",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_l",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:21:10",
                fieldName = NamePair {
                  nameC = Name "l",
                  nameHsIdent = HsIdentifier
                    "primitive_l"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 320,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "l",
              commentLocation = Just
                "primitive_types.h:21:10",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_li",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:22:14",
                fieldName = NamePair {
                  nameC = Name "li",
                  nameHsIdent = HsIdentifier
                    "primitive_li"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 384,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "li",
              commentLocation = Just
                "primitive_types.h:22:14",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sl",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:23:17",
                fieldName = NamePair {
                  nameC = Name "sl",
                  nameHsIdent = HsIdentifier
                    "primitive_sl"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 448,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "sl",
              commentLocation = Just
                "primitive_types.h:23:17",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sli",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:24:21",
                fieldName = NamePair {
                  nameC = Name "sli",
                  nameHsIdent = HsIdentifier
                    "primitive_sli"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 512,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "sli",
              commentLocation = Just
                "primitive_types.h:24:21",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ul",
          fieldType = HsPrimType
            HsPrimCULong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:26:19",
                fieldName = NamePair {
                  nameC = Name "ul",
                  nameHsIdent = HsIdentifier
                    "primitive_ul"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLong
                  Unsigned),
              structFieldOffset = 576,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ul",
              commentLocation = Just
                "primitive_types.h:26:19",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_uli",
          fieldType = HsPrimType
            HsPrimCULong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:27:23",
                fieldName = NamePair {
                  nameC = Name "uli",
                  nameHsIdent = HsIdentifier
                    "primitive_uli"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLong
                  Unsigned),
              structFieldOffset = 640,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "uli",
              commentLocation = Just
                "primitive_types.h:27:23",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ll",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:29:15",
                fieldName = NamePair {
                  nameC = Name "ll",
                  nameHsIdent = HsIdentifier
                    "primitive_ll"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Signed),
              structFieldOffset = 704,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ll",
              commentLocation = Just
                "primitive_types.h:29:15",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_lli",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:30:19",
                fieldName = NamePair {
                  nameC = Name "lli",
                  nameHsIdent = HsIdentifier
                    "primitive_lli"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Signed),
              structFieldOffset = 768,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "lli",
              commentLocation = Just
                "primitive_types.h:30:19",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_sll",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:31:22",
                fieldName = NamePair {
                  nameC = Name "sll",
                  nameHsIdent = HsIdentifier
                    "primitive_sll"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Signed),
              structFieldOffset = 832,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "sll",
              commentLocation = Just
                "primitive_types.h:31:22",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_slli",
          fieldType = HsPrimType
            HsPrimCLLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:32:26",
                fieldName = NamePair {
                  nameC = Name "slli",
                  nameHsIdent = HsIdentifier
                    "primitive_slli"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Signed),
              structFieldOffset = 896,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "slli",
              commentLocation = Just
                "primitive_types.h:32:26",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ull",
          fieldType = HsPrimType
            HsPrimCULLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:34:24",
                fieldName = NamePair {
                  nameC = Name "ull",
                  nameHsIdent = HsIdentifier
                    "primitive_ull"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Unsigned),
              structFieldOffset = 960,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ull",
              commentLocation = Just
                "primitive_types.h:34:24",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_ulli",
          fieldType = HsPrimType
            HsPrimCULLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:35:28",
                fieldName = NamePair {
                  nameC = Name "ulli",
                  nameHsIdent = HsIdentifier
                    "primitive_ulli"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral
                  PrimLongLong
                  Unsigned),
              structFieldOffset = 1024,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "ulli",
              commentLocation = Just
                "primitive_types.h:35:28",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_f",
          fieldType = HsPrimType
            HsPrimCFloat,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:37:11",
                fieldName = NamePair {
                  nameC = Name "f",
                  nameHsIdent = HsIdentifier
                    "primitive_f"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimFloat),
              structFieldOffset = 1088,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "f",
              commentLocation = Just
                "primitive_types.h:37:11",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "primitive_d",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "primitive_types.h:38:12",
                fieldName = NamePair {
                  nameC = Name "d",
                  nameHsIdent = HsIdentifier
                    "primitive_d"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 1152,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "d",
              commentLocation = Just
                "primitive_types.h:38:12",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "primitive_types.h:1:8",
            declId = NamePair {
              nameC = Name "primitive",
              nameHsIdent = HsIdentifier
                "Primitive"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "primitive_types.h",
            declComment = Nothing},
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
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:2:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = HsIdentifier
                        "primitive_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:3:17",
                    fieldName = NamePair {
                      nameC = Name "sc",
                      nameHsIdent = HsIdentifier
                        "primitive_sc"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Signed)),
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:4:19",
                    fieldName = NamePair {
                      nameC = Name "uc",
                      nameHsIdent = HsIdentifier
                        "primitive_uc"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Unsigned)),
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:6:11",
                    fieldName = NamePair {
                      nameC = Name "s",
                      nameHsIdent = HsIdentifier
                        "primitive_s"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:7:15",
                    fieldName = NamePair {
                      nameC = Name "si",
                      nameHsIdent = HsIdentifier
                        "primitive_si"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 48,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:8:18",
                    fieldName = NamePair {
                      nameC = Name "ss",
                      nameHsIdent = HsIdentifier
                        "primitive_ss"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:9:22",
                    fieldName = NamePair {
                      nameC = Name "ssi",
                      nameHsIdent = HsIdentifier
                        "primitive_ssi"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 80,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:11:20",
                    fieldName = NamePair {
                      nameC = Name "us",
                      nameHsIdent = HsIdentifier
                        "primitive_us"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimShort
                      Unsigned),
                  structFieldOffset = 96,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:12:24",
                    fieldName = NamePair {
                      nameC = Name "usi",
                      nameHsIdent = HsIdentifier
                        "primitive_usi"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimShort
                      Unsigned),
                  structFieldOffset = 112,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:14:9",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = HsIdentifier
                        "primitive_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:15:12",
                    fieldName = NamePair {
                      nameC = Name "s2",
                      nameHsIdent = HsIdentifier
                        "primitive_s2"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 160,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:16:16",
                    fieldName = NamePair {
                      nameC = Name "si2",
                      nameHsIdent = HsIdentifier
                        "primitive_si2"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:18:14",
                    fieldName = NamePair {
                      nameC = Name "u",
                      nameHsIdent = HsIdentifier
                        "primitive_u"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 224,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:19:18",
                    fieldName = NamePair {
                      nameC = Name "ui",
                      nameHsIdent = HsIdentifier
                        "primitive_ui"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:21:10",
                    fieldName = NamePair {
                      nameC = Name "l",
                      nameHsIdent = HsIdentifier
                        "primitive_l"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 320,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:22:14",
                    fieldName = NamePair {
                      nameC = Name "li",
                      nameHsIdent = HsIdentifier
                        "primitive_li"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 384,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:23:17",
                    fieldName = NamePair {
                      nameC = Name "sl",
                      nameHsIdent = HsIdentifier
                        "primitive_sl"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 448,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:24:21",
                    fieldName = NamePair {
                      nameC = Name "sli",
                      nameHsIdent = HsIdentifier
                        "primitive_sli"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 512,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:26:19",
                    fieldName = NamePair {
                      nameC = Name "ul",
                      nameHsIdent = HsIdentifier
                        "primitive_ul"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLong
                      Unsigned),
                  structFieldOffset = 576,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:27:23",
                    fieldName = NamePair {
                      nameC = Name "uli",
                      nameHsIdent = HsIdentifier
                        "primitive_uli"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLong
                      Unsigned),
                  structFieldOffset = 640,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:29:15",
                    fieldName = NamePair {
                      nameC = Name "ll",
                      nameHsIdent = HsIdentifier
                        "primitive_ll"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 704,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:30:19",
                    fieldName = NamePair {
                      nameC = Name "lli",
                      nameHsIdent = HsIdentifier
                        "primitive_lli"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 768,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:31:22",
                    fieldName = NamePair {
                      nameC = Name "sll",
                      nameHsIdent = HsIdentifier
                        "primitive_sll"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 832,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:32:26",
                    fieldName = NamePair {
                      nameC = Name "slli",
                      nameHsIdent = HsIdentifier
                        "primitive_slli"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 896,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:34:24",
                    fieldName = NamePair {
                      nameC = Name "ull",
                      nameHsIdent = HsIdentifier
                        "primitive_ull"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Unsigned),
                  structFieldOffset = 960,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:35:28",
                    fieldName = NamePair {
                      nameC = Name "ulli",
                      nameHsIdent = HsIdentifier
                        "primitive_ulli"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Unsigned),
                  structFieldOffset = 1024,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:37:11",
                    fieldName = NamePair {
                      nameC = Name "f",
                      nameHsIdent = HsIdentifier
                        "primitive_f"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimFloat),
                  structFieldOffset = 1088,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:38:12",
                    fieldName = NamePair {
                      nameC = Name "d",
                      nameHsIdent = HsIdentifier
                        "primitive_d"},
                    fieldComment = Nothing},
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
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "primitive",
          commentLocation = Just
            "primitive_types.h:1:8",
          commentHeader = Just
            "primitive_types.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
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
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:2:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = HsIdentifier
                        "primitive_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "primitive_types.h:2:10",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_sc",
              fieldType = HsPrimType
                HsPrimCSChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:3:17",
                    fieldName = NamePair {
                      nameC = Name "sc",
                      nameHsIdent = HsIdentifier
                        "primitive_sc"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Signed)),
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "sc",
                  commentLocation = Just
                    "primitive_types.h:3:17",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_uc",
              fieldType = HsPrimType
                HsPrimCUChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:4:19",
                    fieldName = NamePair {
                      nameC = Name "uc",
                      nameHsIdent = HsIdentifier
                        "primitive_uc"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Unsigned)),
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "uc",
                  commentLocation = Just
                    "primitive_types.h:4:19",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_s",
              fieldType = HsPrimType
                HsPrimCShort,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:6:11",
                    fieldName = NamePair {
                      nameC = Name "s",
                      nameHsIdent = HsIdentifier
                        "primitive_s"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "s",
                  commentLocation = Just
                    "primitive_types.h:6:11",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_si",
              fieldType = HsPrimType
                HsPrimCShort,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:7:15",
                    fieldName = NamePair {
                      nameC = Name "si",
                      nameHsIdent = HsIdentifier
                        "primitive_si"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 48,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "si",
                  commentLocation = Just
                    "primitive_types.h:7:15",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_ss",
              fieldType = HsPrimType
                HsPrimCShort,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:8:18",
                    fieldName = NamePair {
                      nameC = Name "ss",
                      nameHsIdent = HsIdentifier
                        "primitive_ss"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ss",
                  commentLocation = Just
                    "primitive_types.h:8:18",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_ssi",
              fieldType = HsPrimType
                HsPrimCShort,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:9:22",
                    fieldName = NamePair {
                      nameC = Name "ssi",
                      nameHsIdent = HsIdentifier
                        "primitive_ssi"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimShort Signed),
                  structFieldOffset = 80,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ssi",
                  commentLocation = Just
                    "primitive_types.h:9:22",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_us",
              fieldType = HsPrimType
                HsPrimCUShort,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:11:20",
                    fieldName = NamePair {
                      nameC = Name "us",
                      nameHsIdent = HsIdentifier
                        "primitive_us"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimShort
                      Unsigned),
                  structFieldOffset = 96,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "us",
                  commentLocation = Just
                    "primitive_types.h:11:20",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_usi",
              fieldType = HsPrimType
                HsPrimCUShort,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:12:24",
                    fieldName = NamePair {
                      nameC = Name "usi",
                      nameHsIdent = HsIdentifier
                        "primitive_usi"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimShort
                      Unsigned),
                  structFieldOffset = 112,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "usi",
                  commentLocation = Just
                    "primitive_types.h:12:24",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_i",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:14:9",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = HsIdentifier
                        "primitive_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "i",
                  commentLocation = Just
                    "primitive_types.h:14:9",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_s2",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:15:12",
                    fieldName = NamePair {
                      nameC = Name "s2",
                      nameHsIdent = HsIdentifier
                        "primitive_s2"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 160,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "s2",
                  commentLocation = Just
                    "primitive_types.h:15:12",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_si2",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:16:16",
                    fieldName = NamePair {
                      nameC = Name "si2",
                      nameHsIdent = HsIdentifier
                        "primitive_si2"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "si2",
                  commentLocation = Just
                    "primitive_types.h:16:16",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_u",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:18:14",
                    fieldName = NamePair {
                      nameC = Name "u",
                      nameHsIdent = HsIdentifier
                        "primitive_u"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 224,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "u",
                  commentLocation = Just
                    "primitive_types.h:18:14",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_ui",
              fieldType = HsPrimType
                HsPrimCUInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:19:18",
                    fieldName = NamePair {
                      nameC = Name "ui",
                      nameHsIdent = HsIdentifier
                        "primitive_ui"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Unsigned),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ui",
                  commentLocation = Just
                    "primitive_types.h:19:18",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_l",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:21:10",
                    fieldName = NamePair {
                      nameC = Name "l",
                      nameHsIdent = HsIdentifier
                        "primitive_l"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 320,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "l",
                  commentLocation = Just
                    "primitive_types.h:21:10",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_li",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:22:14",
                    fieldName = NamePair {
                      nameC = Name "li",
                      nameHsIdent = HsIdentifier
                        "primitive_li"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 384,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "li",
                  commentLocation = Just
                    "primitive_types.h:22:14",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_sl",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:23:17",
                    fieldName = NamePair {
                      nameC = Name "sl",
                      nameHsIdent = HsIdentifier
                        "primitive_sl"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 448,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "sl",
                  commentLocation = Just
                    "primitive_types.h:23:17",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_sli",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:24:21",
                    fieldName = NamePair {
                      nameC = Name "sli",
                      nameHsIdent = HsIdentifier
                        "primitive_sli"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 512,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "sli",
                  commentLocation = Just
                    "primitive_types.h:24:21",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_ul",
              fieldType = HsPrimType
                HsPrimCULong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:26:19",
                    fieldName = NamePair {
                      nameC = Name "ul",
                      nameHsIdent = HsIdentifier
                        "primitive_ul"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLong
                      Unsigned),
                  structFieldOffset = 576,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ul",
                  commentLocation = Just
                    "primitive_types.h:26:19",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_uli",
              fieldType = HsPrimType
                HsPrimCULong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:27:23",
                    fieldName = NamePair {
                      nameC = Name "uli",
                      nameHsIdent = HsIdentifier
                        "primitive_uli"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLong
                      Unsigned),
                  structFieldOffset = 640,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "uli",
                  commentLocation = Just
                    "primitive_types.h:27:23",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_ll",
              fieldType = HsPrimType
                HsPrimCLLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:29:15",
                    fieldName = NamePair {
                      nameC = Name "ll",
                      nameHsIdent = HsIdentifier
                        "primitive_ll"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 704,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ll",
                  commentLocation = Just
                    "primitive_types.h:29:15",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_lli",
              fieldType = HsPrimType
                HsPrimCLLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:30:19",
                    fieldName = NamePair {
                      nameC = Name "lli",
                      nameHsIdent = HsIdentifier
                        "primitive_lli"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 768,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "lli",
                  commentLocation = Just
                    "primitive_types.h:30:19",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_sll",
              fieldType = HsPrimType
                HsPrimCLLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:31:22",
                    fieldName = NamePair {
                      nameC = Name "sll",
                      nameHsIdent = HsIdentifier
                        "primitive_sll"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 832,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "sll",
                  commentLocation = Just
                    "primitive_types.h:31:22",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_slli",
              fieldType = HsPrimType
                HsPrimCLLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:32:26",
                    fieldName = NamePair {
                      nameC = Name "slli",
                      nameHsIdent = HsIdentifier
                        "primitive_slli"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Signed),
                  structFieldOffset = 896,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "slli",
                  commentLocation = Just
                    "primitive_types.h:32:26",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_ull",
              fieldType = HsPrimType
                HsPrimCULLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:34:24",
                    fieldName = NamePair {
                      nameC = Name "ull",
                      nameHsIdent = HsIdentifier
                        "primitive_ull"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Unsigned),
                  structFieldOffset = 960,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ull",
                  commentLocation = Just
                    "primitive_types.h:34:24",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_ulli",
              fieldType = HsPrimType
                HsPrimCULLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:35:28",
                    fieldName = NamePair {
                      nameC = Name "ulli",
                      nameHsIdent = HsIdentifier
                        "primitive_ulli"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral
                      PrimLongLong
                      Unsigned),
                  structFieldOffset = 1024,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "ulli",
                  commentLocation = Just
                    "primitive_types.h:35:28",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_f",
              fieldType = HsPrimType
                HsPrimCFloat,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:37:11",
                    fieldName = NamePair {
                      nameC = Name "f",
                      nameHsIdent = HsIdentifier
                        "primitive_f"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimFloat),
                  structFieldOffset = 1088,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "f",
                  commentLocation = Just
                    "primitive_types.h:37:11",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "primitive_d",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:38:12",
                    fieldName = NamePair {
                      nameC = Name "d",
                      nameHsIdent = HsIdentifier
                        "primitive_d"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 1152,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "d",
                  commentLocation = Just
                    "primitive_types.h:38:12",
                  commentHeader = Just
                    "primitive_types.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "primitive_types.h:1:8",
                declId = NamePair {
                  nameC = Name "primitive",
                  nameHsIdent = HsIdentifier
                    "Primitive"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "primitive_types.h",
                declComment = Nothing},
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
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:2:10",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = HsIdentifier
                            "primitive_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:3:17",
                        fieldName = NamePair {
                          nameC = Name "sc",
                          nameHsIdent = HsIdentifier
                            "primitive_sc"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignExplicit Signed)),
                      structFieldOffset = 8,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:4:19",
                        fieldName = NamePair {
                          nameC = Name "uc",
                          nameHsIdent = HsIdentifier
                            "primitive_uc"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignExplicit Unsigned)),
                      structFieldOffset = 16,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:6:11",
                        fieldName = NamePair {
                          nameC = Name "s",
                          nameHsIdent = HsIdentifier
                            "primitive_s"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimShort Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:7:15",
                        fieldName = NamePair {
                          nameC = Name "si",
                          nameHsIdent = HsIdentifier
                            "primitive_si"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimShort Signed),
                      structFieldOffset = 48,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:8:18",
                        fieldName = NamePair {
                          nameC = Name "ss",
                          nameHsIdent = HsIdentifier
                            "primitive_ss"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimShort Signed),
                      structFieldOffset = 64,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:9:22",
                        fieldName = NamePair {
                          nameC = Name "ssi",
                          nameHsIdent = HsIdentifier
                            "primitive_ssi"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimShort Signed),
                      structFieldOffset = 80,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:11:20",
                        fieldName = NamePair {
                          nameC = Name "us",
                          nameHsIdent = HsIdentifier
                            "primitive_us"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral
                          PrimShort
                          Unsigned),
                      structFieldOffset = 96,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:12:24",
                        fieldName = NamePair {
                          nameC = Name "usi",
                          nameHsIdent = HsIdentifier
                            "primitive_usi"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral
                          PrimShort
                          Unsigned),
                      structFieldOffset = 112,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:14:9",
                        fieldName = NamePair {
                          nameC = Name "i",
                          nameHsIdent = HsIdentifier
                            "primitive_i"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 128,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:15:12",
                        fieldName = NamePair {
                          nameC = Name "s2",
                          nameHsIdent = HsIdentifier
                            "primitive_s2"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 160,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:16:16",
                        fieldName = NamePair {
                          nameC = Name "si2",
                          nameHsIdent = HsIdentifier
                            "primitive_si2"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 192,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:18:14",
                        fieldName = NamePair {
                          nameC = Name "u",
                          nameHsIdent = HsIdentifier
                            "primitive_u"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 224,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:19:18",
                        fieldName = NamePair {
                          nameC = Name "ui",
                          nameHsIdent = HsIdentifier
                            "primitive_ui"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Unsigned),
                      structFieldOffset = 256,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:21:10",
                        fieldName = NamePair {
                          nameC = Name "l",
                          nameHsIdent = HsIdentifier
                            "primitive_l"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 320,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:22:14",
                        fieldName = NamePair {
                          nameC = Name "li",
                          nameHsIdent = HsIdentifier
                            "primitive_li"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 384,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:23:17",
                        fieldName = NamePair {
                          nameC = Name "sl",
                          nameHsIdent = HsIdentifier
                            "primitive_sl"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 448,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:24:21",
                        fieldName = NamePair {
                          nameC = Name "sli",
                          nameHsIdent = HsIdentifier
                            "primitive_sli"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 512,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:26:19",
                        fieldName = NamePair {
                          nameC = Name "ul",
                          nameHsIdent = HsIdentifier
                            "primitive_ul"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral
                          PrimLong
                          Unsigned),
                      structFieldOffset = 576,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:27:23",
                        fieldName = NamePair {
                          nameC = Name "uli",
                          nameHsIdent = HsIdentifier
                            "primitive_uli"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral
                          PrimLong
                          Unsigned),
                      structFieldOffset = 640,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:29:15",
                        fieldName = NamePair {
                          nameC = Name "ll",
                          nameHsIdent = HsIdentifier
                            "primitive_ll"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral
                          PrimLongLong
                          Signed),
                      structFieldOffset = 704,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:30:19",
                        fieldName = NamePair {
                          nameC = Name "lli",
                          nameHsIdent = HsIdentifier
                            "primitive_lli"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral
                          PrimLongLong
                          Signed),
                      structFieldOffset = 768,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:31:22",
                        fieldName = NamePair {
                          nameC = Name "sll",
                          nameHsIdent = HsIdentifier
                            "primitive_sll"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral
                          PrimLongLong
                          Signed),
                      structFieldOffset = 832,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:32:26",
                        fieldName = NamePair {
                          nameC = Name "slli",
                          nameHsIdent = HsIdentifier
                            "primitive_slli"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral
                          PrimLongLong
                          Signed),
                      structFieldOffset = 896,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:34:24",
                        fieldName = NamePair {
                          nameC = Name "ull",
                          nameHsIdent = HsIdentifier
                            "primitive_ull"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral
                          PrimLongLong
                          Unsigned),
                      structFieldOffset = 960,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:35:28",
                        fieldName = NamePair {
                          nameC = Name "ulli",
                          nameHsIdent = HsIdentifier
                            "primitive_ulli"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral
                          PrimLongLong
                          Unsigned),
                      structFieldOffset = 1024,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:37:11",
                        fieldName = NamePair {
                          nameC = Name "f",
                          nameHsIdent = HsIdentifier
                            "primitive_f"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimFloat),
                      structFieldOffset = 1088,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:38:12",
                        fieldName = NamePair {
                          nameC = Name "d",
                          nameHsIdent = HsIdentifier
                            "primitive_d"},
                        fieldComment = Nothing},
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
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "primitive",
              commentLocation = Just
                "primitive_types.h:1:8",
              commentHeader = Just
                "primitive_types.h",
              commentChildren = []}}
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:2:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = HsIdentifier
                                "primitive_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "primitive_types.h:2:10",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_sc",
                      fieldType = HsPrimType
                        HsPrimCSChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:3:17",
                            fieldName = NamePair {
                              nameC = Name "sc",
                              nameHsIdent = HsIdentifier
                                "primitive_sc"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignExplicit Signed)),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "sc",
                          commentLocation = Just
                            "primitive_types.h:3:17",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_uc",
                      fieldType = HsPrimType
                        HsPrimCUChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:4:19",
                            fieldName = NamePair {
                              nameC = Name "uc",
                              nameHsIdent = HsIdentifier
                                "primitive_uc"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignExplicit Unsigned)),
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "uc",
                          commentLocation = Just
                            "primitive_types.h:4:19",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_s",
                      fieldType = HsPrimType
                        HsPrimCShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:6:11",
                            fieldName = NamePair {
                              nameC = Name "s",
                              nameHsIdent = HsIdentifier
                                "primitive_s"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimShort Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "s",
                          commentLocation = Just
                            "primitive_types.h:6:11",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_si",
                      fieldType = HsPrimType
                        HsPrimCShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:7:15",
                            fieldName = NamePair {
                              nameC = Name "si",
                              nameHsIdent = HsIdentifier
                                "primitive_si"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimShort Signed),
                          structFieldOffset = 48,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "si",
                          commentLocation = Just
                            "primitive_types.h:7:15",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ss",
                      fieldType = HsPrimType
                        HsPrimCShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:8:18",
                            fieldName = NamePair {
                              nameC = Name "ss",
                              nameHsIdent = HsIdentifier
                                "primitive_ss"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimShort Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ss",
                          commentLocation = Just
                            "primitive_types.h:8:18",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ssi",
                      fieldType = HsPrimType
                        HsPrimCShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:9:22",
                            fieldName = NamePair {
                              nameC = Name "ssi",
                              nameHsIdent = HsIdentifier
                                "primitive_ssi"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimShort Signed),
                          structFieldOffset = 80,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ssi",
                          commentLocation = Just
                            "primitive_types.h:9:22",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_us",
                      fieldType = HsPrimType
                        HsPrimCUShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:11:20",
                            fieldName = NamePair {
                              nameC = Name "us",
                              nameHsIdent = HsIdentifier
                                "primitive_us"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimShort
                              Unsigned),
                          structFieldOffset = 96,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "us",
                          commentLocation = Just
                            "primitive_types.h:11:20",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_usi",
                      fieldType = HsPrimType
                        HsPrimCUShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:12:24",
                            fieldName = NamePair {
                              nameC = Name "usi",
                              nameHsIdent = HsIdentifier
                                "primitive_usi"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimShort
                              Unsigned),
                          structFieldOffset = 112,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "usi",
                          commentLocation = Just
                            "primitive_types.h:12:24",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:14:9",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = HsIdentifier
                                "primitive_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "primitive_types.h:14:9",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_s2",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:15:12",
                            fieldName = NamePair {
                              nameC = Name "s2",
                              nameHsIdent = HsIdentifier
                                "primitive_s2"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 160,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "s2",
                          commentLocation = Just
                            "primitive_types.h:15:12",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_si2",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:16:16",
                            fieldName = NamePair {
                              nameC = Name "si2",
                              nameHsIdent = HsIdentifier
                                "primitive_si2"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 192,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "si2",
                          commentLocation = Just
                            "primitive_types.h:16:16",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_u",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:18:14",
                            fieldName = NamePair {
                              nameC = Name "u",
                              nameHsIdent = HsIdentifier
                                "primitive_u"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 224,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "u",
                          commentLocation = Just
                            "primitive_types.h:18:14",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ui",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:19:18",
                            fieldName = NamePair {
                              nameC = Name "ui",
                              nameHsIdent = HsIdentifier
                                "primitive_ui"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 256,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ui",
                          commentLocation = Just
                            "primitive_types.h:19:18",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_l",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:21:10",
                            fieldName = NamePair {
                              nameC = Name "l",
                              nameHsIdent = HsIdentifier
                                "primitive_l"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 320,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "l",
                          commentLocation = Just
                            "primitive_types.h:21:10",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_li",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:22:14",
                            fieldName = NamePair {
                              nameC = Name "li",
                              nameHsIdent = HsIdentifier
                                "primitive_li"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 384,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "li",
                          commentLocation = Just
                            "primitive_types.h:22:14",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_sl",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:23:17",
                            fieldName = NamePair {
                              nameC = Name "sl",
                              nameHsIdent = HsIdentifier
                                "primitive_sl"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 448,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "sl",
                          commentLocation = Just
                            "primitive_types.h:23:17",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_sli",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:24:21",
                            fieldName = NamePair {
                              nameC = Name "sli",
                              nameHsIdent = HsIdentifier
                                "primitive_sli"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 512,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "sli",
                          commentLocation = Just
                            "primitive_types.h:24:21",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ul",
                      fieldType = HsPrimType
                        HsPrimCULong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:26:19",
                            fieldName = NamePair {
                              nameC = Name "ul",
                              nameHsIdent = HsIdentifier
                                "primitive_ul"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLong
                              Unsigned),
                          structFieldOffset = 576,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ul",
                          commentLocation = Just
                            "primitive_types.h:26:19",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_uli",
                      fieldType = HsPrimType
                        HsPrimCULong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:27:23",
                            fieldName = NamePair {
                              nameC = Name "uli",
                              nameHsIdent = HsIdentifier
                                "primitive_uli"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLong
                              Unsigned),
                          structFieldOffset = 640,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "uli",
                          commentLocation = Just
                            "primitive_types.h:27:23",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ll",
                      fieldType = HsPrimType
                        HsPrimCLLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:29:15",
                            fieldName = NamePair {
                              nameC = Name "ll",
                              nameHsIdent = HsIdentifier
                                "primitive_ll"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Signed),
                          structFieldOffset = 704,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ll",
                          commentLocation = Just
                            "primitive_types.h:29:15",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_lli",
                      fieldType = HsPrimType
                        HsPrimCLLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:30:19",
                            fieldName = NamePair {
                              nameC = Name "lli",
                              nameHsIdent = HsIdentifier
                                "primitive_lli"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Signed),
                          structFieldOffset = 768,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "lli",
                          commentLocation = Just
                            "primitive_types.h:30:19",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_sll",
                      fieldType = HsPrimType
                        HsPrimCLLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:31:22",
                            fieldName = NamePair {
                              nameC = Name "sll",
                              nameHsIdent = HsIdentifier
                                "primitive_sll"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Signed),
                          structFieldOffset = 832,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "sll",
                          commentLocation = Just
                            "primitive_types.h:31:22",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_slli",
                      fieldType = HsPrimType
                        HsPrimCLLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:32:26",
                            fieldName = NamePair {
                              nameC = Name "slli",
                              nameHsIdent = HsIdentifier
                                "primitive_slli"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Signed),
                          structFieldOffset = 896,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "slli",
                          commentLocation = Just
                            "primitive_types.h:32:26",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ull",
                      fieldType = HsPrimType
                        HsPrimCULLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:34:24",
                            fieldName = NamePair {
                              nameC = Name "ull",
                              nameHsIdent = HsIdentifier
                                "primitive_ull"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Unsigned),
                          structFieldOffset = 960,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ull",
                          commentLocation = Just
                            "primitive_types.h:34:24",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ulli",
                      fieldType = HsPrimType
                        HsPrimCULLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:35:28",
                            fieldName = NamePair {
                              nameC = Name "ulli",
                              nameHsIdent = HsIdentifier
                                "primitive_ulli"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Unsigned),
                          structFieldOffset = 1024,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ulli",
                          commentLocation = Just
                            "primitive_types.h:35:28",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_f",
                      fieldType = HsPrimType
                        HsPrimCFloat,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:37:11",
                            fieldName = NamePair {
                              nameC = Name "f",
                              nameHsIdent = HsIdentifier
                                "primitive_f"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimFloat),
                          structFieldOffset = 1088,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "f",
                          commentLocation = Just
                            "primitive_types.h:37:11",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_d",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:38:12",
                            fieldName = NamePair {
                              nameC = Name "d",
                              nameHsIdent = HsIdentifier
                                "primitive_d"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 1152,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "d",
                          commentLocation = Just
                            "primitive_types.h:38:12",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "primitive_types.h:1:8",
                        declId = NamePair {
                          nameC = Name "primitive",
                          nameHsIdent = HsIdentifier
                            "Primitive"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "primitive_types.h",
                        declComment = Nothing},
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
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:2:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = HsIdentifier
                                    "primitive_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:3:17",
                                fieldName = NamePair {
                                  nameC = Name "sc",
                                  nameHsIdent = HsIdentifier
                                    "primitive_sc"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignExplicit Signed)),
                              structFieldOffset = 8,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:4:19",
                                fieldName = NamePair {
                                  nameC = Name "uc",
                                  nameHsIdent = HsIdentifier
                                    "primitive_uc"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignExplicit Unsigned)),
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:6:11",
                                fieldName = NamePair {
                                  nameC = Name "s",
                                  nameHsIdent = HsIdentifier
                                    "primitive_s"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimShort Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:7:15",
                                fieldName = NamePair {
                                  nameC = Name "si",
                                  nameHsIdent = HsIdentifier
                                    "primitive_si"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimShort Signed),
                              structFieldOffset = 48,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:8:18",
                                fieldName = NamePair {
                                  nameC = Name "ss",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ss"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimShort Signed),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:9:22",
                                fieldName = NamePair {
                                  nameC = Name "ssi",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ssi"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimShort Signed),
                              structFieldOffset = 80,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:11:20",
                                fieldName = NamePair {
                                  nameC = Name "us",
                                  nameHsIdent = HsIdentifier
                                    "primitive_us"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Unsigned),
                              structFieldOffset = 96,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:12:24",
                                fieldName = NamePair {
                                  nameC = Name "usi",
                                  nameHsIdent = HsIdentifier
                                    "primitive_usi"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Unsigned),
                              structFieldOffset = 112,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:14:9",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = HsIdentifier
                                    "primitive_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 128,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:15:12",
                                fieldName = NamePair {
                                  nameC = Name "s2",
                                  nameHsIdent = HsIdentifier
                                    "primitive_s2"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 160,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:16:16",
                                fieldName = NamePair {
                                  nameC = Name "si2",
                                  nameHsIdent = HsIdentifier
                                    "primitive_si2"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 192,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:18:14",
                                fieldName = NamePair {
                                  nameC = Name "u",
                                  nameHsIdent = HsIdentifier
                                    "primitive_u"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 224,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:19:18",
                                fieldName = NamePair {
                                  nameC = Name "ui",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ui"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 256,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:21:10",
                                fieldName = NamePair {
                                  nameC = Name "l",
                                  nameHsIdent = HsIdentifier
                                    "primitive_l"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 320,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:22:14",
                                fieldName = NamePair {
                                  nameC = Name "li",
                                  nameHsIdent = HsIdentifier
                                    "primitive_li"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 384,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:23:17",
                                fieldName = NamePair {
                                  nameC = Name "sl",
                                  nameHsIdent = HsIdentifier
                                    "primitive_sl"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 448,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:24:21",
                                fieldName = NamePair {
                                  nameC = Name "sli",
                                  nameHsIdent = HsIdentifier
                                    "primitive_sli"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 512,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:26:19",
                                fieldName = NamePair {
                                  nameC = Name "ul",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ul"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLong
                                  Unsigned),
                              structFieldOffset = 576,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:27:23",
                                fieldName = NamePair {
                                  nameC = Name "uli",
                                  nameHsIdent = HsIdentifier
                                    "primitive_uli"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLong
                                  Unsigned),
                              structFieldOffset = 640,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:29:15",
                                fieldName = NamePair {
                                  nameC = Name "ll",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ll"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Signed),
                              structFieldOffset = 704,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:30:19",
                                fieldName = NamePair {
                                  nameC = Name "lli",
                                  nameHsIdent = HsIdentifier
                                    "primitive_lli"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Signed),
                              structFieldOffset = 768,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:31:22",
                                fieldName = NamePair {
                                  nameC = Name "sll",
                                  nameHsIdent = HsIdentifier
                                    "primitive_sll"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Signed),
                              structFieldOffset = 832,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:32:26",
                                fieldName = NamePair {
                                  nameC = Name "slli",
                                  nameHsIdent = HsIdentifier
                                    "primitive_slli"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Signed),
                              structFieldOffset = 896,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:34:24",
                                fieldName = NamePair {
                                  nameC = Name "ull",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ull"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Unsigned),
                              structFieldOffset = 960,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:35:28",
                                fieldName = NamePair {
                                  nameC = Name "ulli",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ulli"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Unsigned),
                              structFieldOffset = 1024,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:37:11",
                                fieldName = NamePair {
                                  nameC = Name "f",
                                  nameHsIdent = HsIdentifier
                                    "primitive_f"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimFloat),
                              structFieldOffset = 1088,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:38:12",
                                fieldName = NamePair {
                                  nameC = Name "d",
                                  nameHsIdent = HsIdentifier
                                    "primitive_d"},
                                fieldComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "primitive",
                      commentLocation = Just
                        "primitive_types.h:1:8",
                      commentHeader = Just
                        "primitive_types.h",
                      commentChildren = []}})
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:2:10",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = HsIdentifier
                                "primitive_c"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "primitive_types.h:2:10",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_sc",
                      fieldType = HsPrimType
                        HsPrimCSChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:3:17",
                            fieldName = NamePair {
                              nameC = Name "sc",
                              nameHsIdent = HsIdentifier
                                "primitive_sc"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignExplicit Signed)),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "sc",
                          commentLocation = Just
                            "primitive_types.h:3:17",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_uc",
                      fieldType = HsPrimType
                        HsPrimCUChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:4:19",
                            fieldName = NamePair {
                              nameC = Name "uc",
                              nameHsIdent = HsIdentifier
                                "primitive_uc"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignExplicit Unsigned)),
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "uc",
                          commentLocation = Just
                            "primitive_types.h:4:19",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_s",
                      fieldType = HsPrimType
                        HsPrimCShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:6:11",
                            fieldName = NamePair {
                              nameC = Name "s",
                              nameHsIdent = HsIdentifier
                                "primitive_s"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimShort Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "s",
                          commentLocation = Just
                            "primitive_types.h:6:11",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_si",
                      fieldType = HsPrimType
                        HsPrimCShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:7:15",
                            fieldName = NamePair {
                              nameC = Name "si",
                              nameHsIdent = HsIdentifier
                                "primitive_si"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimShort Signed),
                          structFieldOffset = 48,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "si",
                          commentLocation = Just
                            "primitive_types.h:7:15",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ss",
                      fieldType = HsPrimType
                        HsPrimCShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:8:18",
                            fieldName = NamePair {
                              nameC = Name "ss",
                              nameHsIdent = HsIdentifier
                                "primitive_ss"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimShort Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ss",
                          commentLocation = Just
                            "primitive_types.h:8:18",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ssi",
                      fieldType = HsPrimType
                        HsPrimCShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:9:22",
                            fieldName = NamePair {
                              nameC = Name "ssi",
                              nameHsIdent = HsIdentifier
                                "primitive_ssi"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimShort Signed),
                          structFieldOffset = 80,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ssi",
                          commentLocation = Just
                            "primitive_types.h:9:22",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_us",
                      fieldType = HsPrimType
                        HsPrimCUShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:11:20",
                            fieldName = NamePair {
                              nameC = Name "us",
                              nameHsIdent = HsIdentifier
                                "primitive_us"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimShort
                              Unsigned),
                          structFieldOffset = 96,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "us",
                          commentLocation = Just
                            "primitive_types.h:11:20",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_usi",
                      fieldType = HsPrimType
                        HsPrimCUShort,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:12:24",
                            fieldName = NamePair {
                              nameC = Name "usi",
                              nameHsIdent = HsIdentifier
                                "primitive_usi"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimShort
                              Unsigned),
                          structFieldOffset = 112,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "usi",
                          commentLocation = Just
                            "primitive_types.h:12:24",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:14:9",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = HsIdentifier
                                "primitive_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 128,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "primitive_types.h:14:9",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_s2",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:15:12",
                            fieldName = NamePair {
                              nameC = Name "s2",
                              nameHsIdent = HsIdentifier
                                "primitive_s2"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 160,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "s2",
                          commentLocation = Just
                            "primitive_types.h:15:12",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_si2",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:16:16",
                            fieldName = NamePair {
                              nameC = Name "si2",
                              nameHsIdent = HsIdentifier
                                "primitive_si2"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 192,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "si2",
                          commentLocation = Just
                            "primitive_types.h:16:16",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_u",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:18:14",
                            fieldName = NamePair {
                              nameC = Name "u",
                              nameHsIdent = HsIdentifier
                                "primitive_u"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 224,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "u",
                          commentLocation = Just
                            "primitive_types.h:18:14",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ui",
                      fieldType = HsPrimType
                        HsPrimCUInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:19:18",
                            fieldName = NamePair {
                              nameC = Name "ui",
                              nameHsIdent = HsIdentifier
                                "primitive_ui"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Unsigned),
                          structFieldOffset = 256,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ui",
                          commentLocation = Just
                            "primitive_types.h:19:18",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_l",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:21:10",
                            fieldName = NamePair {
                              nameC = Name "l",
                              nameHsIdent = HsIdentifier
                                "primitive_l"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 320,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "l",
                          commentLocation = Just
                            "primitive_types.h:21:10",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_li",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:22:14",
                            fieldName = NamePair {
                              nameC = Name "li",
                              nameHsIdent = HsIdentifier
                                "primitive_li"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 384,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "li",
                          commentLocation = Just
                            "primitive_types.h:22:14",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_sl",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:23:17",
                            fieldName = NamePair {
                              nameC = Name "sl",
                              nameHsIdent = HsIdentifier
                                "primitive_sl"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 448,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "sl",
                          commentLocation = Just
                            "primitive_types.h:23:17",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_sli",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:24:21",
                            fieldName = NamePair {
                              nameC = Name "sli",
                              nameHsIdent = HsIdentifier
                                "primitive_sli"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 512,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "sli",
                          commentLocation = Just
                            "primitive_types.h:24:21",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ul",
                      fieldType = HsPrimType
                        HsPrimCULong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:26:19",
                            fieldName = NamePair {
                              nameC = Name "ul",
                              nameHsIdent = HsIdentifier
                                "primitive_ul"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLong
                              Unsigned),
                          structFieldOffset = 576,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ul",
                          commentLocation = Just
                            "primitive_types.h:26:19",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_uli",
                      fieldType = HsPrimType
                        HsPrimCULong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:27:23",
                            fieldName = NamePair {
                              nameC = Name "uli",
                              nameHsIdent = HsIdentifier
                                "primitive_uli"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLong
                              Unsigned),
                          structFieldOffset = 640,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "uli",
                          commentLocation = Just
                            "primitive_types.h:27:23",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ll",
                      fieldType = HsPrimType
                        HsPrimCLLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:29:15",
                            fieldName = NamePair {
                              nameC = Name "ll",
                              nameHsIdent = HsIdentifier
                                "primitive_ll"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Signed),
                          structFieldOffset = 704,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ll",
                          commentLocation = Just
                            "primitive_types.h:29:15",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_lli",
                      fieldType = HsPrimType
                        HsPrimCLLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:30:19",
                            fieldName = NamePair {
                              nameC = Name "lli",
                              nameHsIdent = HsIdentifier
                                "primitive_lli"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Signed),
                          structFieldOffset = 768,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "lli",
                          commentLocation = Just
                            "primitive_types.h:30:19",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_sll",
                      fieldType = HsPrimType
                        HsPrimCLLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:31:22",
                            fieldName = NamePair {
                              nameC = Name "sll",
                              nameHsIdent = HsIdentifier
                                "primitive_sll"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Signed),
                          structFieldOffset = 832,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "sll",
                          commentLocation = Just
                            "primitive_types.h:31:22",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_slli",
                      fieldType = HsPrimType
                        HsPrimCLLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:32:26",
                            fieldName = NamePair {
                              nameC = Name "slli",
                              nameHsIdent = HsIdentifier
                                "primitive_slli"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Signed),
                          structFieldOffset = 896,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "slli",
                          commentLocation = Just
                            "primitive_types.h:32:26",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ull",
                      fieldType = HsPrimType
                        HsPrimCULLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:34:24",
                            fieldName = NamePair {
                              nameC = Name "ull",
                              nameHsIdent = HsIdentifier
                                "primitive_ull"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Unsigned),
                          structFieldOffset = 960,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ull",
                          commentLocation = Just
                            "primitive_types.h:34:24",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_ulli",
                      fieldType = HsPrimType
                        HsPrimCULLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:35:28",
                            fieldName = NamePair {
                              nameC = Name "ulli",
                              nameHsIdent = HsIdentifier
                                "primitive_ulli"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral
                              PrimLongLong
                              Unsigned),
                          structFieldOffset = 1024,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "ulli",
                          commentLocation = Just
                            "primitive_types.h:35:28",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_f",
                      fieldType = HsPrimType
                        HsPrimCFloat,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:37:11",
                            fieldName = NamePair {
                              nameC = Name "f",
                              nameHsIdent = HsIdentifier
                                "primitive_f"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimFloat),
                          structFieldOffset = 1088,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "f",
                          commentLocation = Just
                            "primitive_types.h:37:11",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "primitive_d",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "primitive_types.h:38:12",
                            fieldName = NamePair {
                              nameC = Name "d",
                              nameHsIdent = HsIdentifier
                                "primitive_d"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 1152,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "d",
                          commentLocation = Just
                            "primitive_types.h:38:12",
                          commentHeader = Just
                            "primitive_types.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "primitive_types.h:1:8",
                        declId = NamePair {
                          nameC = Name "primitive",
                          nameHsIdent = HsIdentifier
                            "Primitive"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "primitive_types.h",
                        declComment = Nothing},
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
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:2:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = HsIdentifier
                                    "primitive_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:3:17",
                                fieldName = NamePair {
                                  nameC = Name "sc",
                                  nameHsIdent = HsIdentifier
                                    "primitive_sc"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignExplicit Signed)),
                              structFieldOffset = 8,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:4:19",
                                fieldName = NamePair {
                                  nameC = Name "uc",
                                  nameHsIdent = HsIdentifier
                                    "primitive_uc"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignExplicit Unsigned)),
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:6:11",
                                fieldName = NamePair {
                                  nameC = Name "s",
                                  nameHsIdent = HsIdentifier
                                    "primitive_s"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimShort Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:7:15",
                                fieldName = NamePair {
                                  nameC = Name "si",
                                  nameHsIdent = HsIdentifier
                                    "primitive_si"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimShort Signed),
                              structFieldOffset = 48,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:8:18",
                                fieldName = NamePair {
                                  nameC = Name "ss",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ss"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimShort Signed),
                              structFieldOffset = 64,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:9:22",
                                fieldName = NamePair {
                                  nameC = Name "ssi",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ssi"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimShort Signed),
                              structFieldOffset = 80,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:11:20",
                                fieldName = NamePair {
                                  nameC = Name "us",
                                  nameHsIdent = HsIdentifier
                                    "primitive_us"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Unsigned),
                              structFieldOffset = 96,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:12:24",
                                fieldName = NamePair {
                                  nameC = Name "usi",
                                  nameHsIdent = HsIdentifier
                                    "primitive_usi"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimShort
                                  Unsigned),
                              structFieldOffset = 112,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:14:9",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = HsIdentifier
                                    "primitive_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 128,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:15:12",
                                fieldName = NamePair {
                                  nameC = Name "s2",
                                  nameHsIdent = HsIdentifier
                                    "primitive_s2"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 160,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:16:16",
                                fieldName = NamePair {
                                  nameC = Name "si2",
                                  nameHsIdent = HsIdentifier
                                    "primitive_si2"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 192,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:18:14",
                                fieldName = NamePair {
                                  nameC = Name "u",
                                  nameHsIdent = HsIdentifier
                                    "primitive_u"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 224,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:19:18",
                                fieldName = NamePair {
                                  nameC = Name "ui",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ui"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Unsigned),
                              structFieldOffset = 256,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:21:10",
                                fieldName = NamePair {
                                  nameC = Name "l",
                                  nameHsIdent = HsIdentifier
                                    "primitive_l"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 320,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:22:14",
                                fieldName = NamePair {
                                  nameC = Name "li",
                                  nameHsIdent = HsIdentifier
                                    "primitive_li"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 384,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:23:17",
                                fieldName = NamePair {
                                  nameC = Name "sl",
                                  nameHsIdent = HsIdentifier
                                    "primitive_sl"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 448,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:24:21",
                                fieldName = NamePair {
                                  nameC = Name "sli",
                                  nameHsIdent = HsIdentifier
                                    "primitive_sli"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 512,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:26:19",
                                fieldName = NamePair {
                                  nameC = Name "ul",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ul"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLong
                                  Unsigned),
                              structFieldOffset = 576,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:27:23",
                                fieldName = NamePair {
                                  nameC = Name "uli",
                                  nameHsIdent = HsIdentifier
                                    "primitive_uli"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLong
                                  Unsigned),
                              structFieldOffset = 640,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:29:15",
                                fieldName = NamePair {
                                  nameC = Name "ll",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ll"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Signed),
                              structFieldOffset = 704,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:30:19",
                                fieldName = NamePair {
                                  nameC = Name "lli",
                                  nameHsIdent = HsIdentifier
                                    "primitive_lli"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Signed),
                              structFieldOffset = 768,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:31:22",
                                fieldName = NamePair {
                                  nameC = Name "sll",
                                  nameHsIdent = HsIdentifier
                                    "primitive_sll"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Signed),
                              structFieldOffset = 832,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:32:26",
                                fieldName = NamePair {
                                  nameC = Name "slli",
                                  nameHsIdent = HsIdentifier
                                    "primitive_slli"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Signed),
                              structFieldOffset = 896,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:34:24",
                                fieldName = NamePair {
                                  nameC = Name "ull",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ull"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Unsigned),
                              structFieldOffset = 960,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:35:28",
                                fieldName = NamePair {
                                  nameC = Name "ulli",
                                  nameHsIdent = HsIdentifier
                                    "primitive_ulli"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral
                                  PrimLongLong
                                  Unsigned),
                              structFieldOffset = 1024,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:37:11",
                                fieldName = NamePair {
                                  nameC = Name "f",
                                  nameHsIdent = HsIdentifier
                                    "primitive_f"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimFloat),
                              structFieldOffset = 1088,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:38:12",
                                fieldName = NamePair {
                                  nameC = Name "d",
                                  nameHsIdent = HsIdentifier
                                    "primitive_d"},
                                fieldComment = Nothing},
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "primitive",
                      commentLocation = Just
                        "primitive_types.h:1:8",
                      commentHeader = Just
                        "primitive_types.h",
                      commentChildren = []}}
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
                      (Idx 27)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Primitive",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Primitive",
      deriveInstanceComment =
      Nothing}]
