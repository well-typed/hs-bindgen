[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Primitive",
      structConstr = Name
        "@NsConstr"
        "Primitive",
      structFields = [
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
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
                  nameHsIdent = Identifier
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
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "primitive_types.h:1:8",
            declId = NamePair {
              nameC = Name "primitive",
              nameHsIdent = Identifier
                "Primitive"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  [
                    "types/primitives/primitive_types.h"],
                headerInclude =
                "types/primitives/primitive_types.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Primitive"),
              structSizeof = 152,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "primitive_types.h:2:10",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
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
                      nameHsIdent = Identifier
                        "primitive_d"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 1152,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "primitive",
          commentLocation = Just
            "primitive_types.h:1:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                [
                  "types/primitives/primitive_types.h"],
              headerInclude =
              "types/primitives/primitive_types.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Primitive",
          structConstr = Name
            "@NsConstr"
            "Primitive",
          structFields = [
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
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
                      nameHsIdent = Identifier
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
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        [
                          "types/primitives/primitive_types.h"],
                      headerInclude =
                      "types/primitives/primitive_types.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "primitive_types.h:1:8",
                declId = NamePair {
                  nameC = Name "primitive",
                  nameHsIdent = Identifier
                    "Primitive"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      [
                        "types/primitives/primitive_types.h"],
                    headerInclude =
                    "types/primitives/primitive_types.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Primitive"),
                  structSizeof = 152,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "primitive_types.h:2:10",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
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
                          nameHsIdent = Identifier
                            "primitive_d"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 1152,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "primitive",
              commentLocation = Just
                "primitive_types.h:1:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    [
                      "types/primitives/primitive_types.h"],
                  headerInclude =
                  "types/primitives/primitive_types.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 152,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Primitive",
                  structConstr = Name
                    "@NsConstr"
                    "Primitive",
                  structFields = [
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "primitive_types.h:1:8",
                        declId = NamePair {
                          nameC = Name "primitive",
                          nameHsIdent = Identifier
                            "Primitive"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "types/primitives/primitive_types.h"],
                            headerInclude =
                            "types/primitives/primitive_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Primitive"),
                          structSizeof = 152,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:2:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
                                    "primitive_d"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 1152,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "primitive",
                      commentLocation = Just
                        "primitive_types.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "types/primitives/primitive_types.h"],
                          headerInclude =
                          "types/primitives/primitive_types.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "primitive_c")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_sc")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_uc")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_s")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_si")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_ss")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_ssi")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_us")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_usi")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_i")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_s2")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_si2")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_u")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_ui")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_l")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_li")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_sl")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_sli")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_ul")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_uli")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_ll")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_lli")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_sll")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_slli")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_ull")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_ulli")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_f")
                  (Idx 0),
                PeekCField
                  (HsStrLit "primitive_d")
                  (Idx 0)]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Primitive",
                  structConstr = Name
                    "@NsConstr"
                    "Primitive",
                  structFields = [
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
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
                              nameHsIdent = Identifier
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
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                [
                                  "types/primitives/primitive_types.h"],
                              headerInclude =
                              "types/primitives/primitive_types.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "primitive_types.h:1:8",
                        declId = NamePair {
                          nameC = Name "primitive",
                          nameHsIdent = Identifier
                            "Primitive"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              [
                                "types/primitives/primitive_types.h"],
                            headerInclude =
                            "types/primitives/primitive_types.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Primitive"),
                          structSizeof = 152,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "primitive_types.h:2:10",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
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
                                  nameHsIdent = Identifier
                                    "primitive_d"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 1152,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "primitive",
                      commentLocation = Just
                        "primitive_types.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            [
                              "types/primitives/primitive_types.h"],
                          headerInclude =
                          "types/primitives/primitive_types.h"},
                      commentChildren = []}}
                (Add 28)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "primitive_c")
                      (Idx 29)
                      (Idx 0),
                    PokeCField
                      (HsStrLit "primitive_sc")
                      (Idx 29)
                      (Idx 1),
                    PokeCField
                      (HsStrLit "primitive_uc")
                      (Idx 29)
                      (Idx 2),
                    PokeCField
                      (HsStrLit "primitive_s")
                      (Idx 29)
                      (Idx 3),
                    PokeCField
                      (HsStrLit "primitive_si")
                      (Idx 29)
                      (Idx 4),
                    PokeCField
                      (HsStrLit "primitive_ss")
                      (Idx 29)
                      (Idx 5),
                    PokeCField
                      (HsStrLit "primitive_ssi")
                      (Idx 29)
                      (Idx 6),
                    PokeCField
                      (HsStrLit "primitive_us")
                      (Idx 29)
                      (Idx 7),
                    PokeCField
                      (HsStrLit "primitive_usi")
                      (Idx 29)
                      (Idx 8),
                    PokeCField
                      (HsStrLit "primitive_i")
                      (Idx 29)
                      (Idx 9),
                    PokeCField
                      (HsStrLit "primitive_s2")
                      (Idx 29)
                      (Idx 10),
                    PokeCField
                      (HsStrLit "primitive_si2")
                      (Idx 29)
                      (Idx 11),
                    PokeCField
                      (HsStrLit "primitive_u")
                      (Idx 29)
                      (Idx 12),
                    PokeCField
                      (HsStrLit "primitive_ui")
                      (Idx 29)
                      (Idx 13),
                    PokeCField
                      (HsStrLit "primitive_l")
                      (Idx 29)
                      (Idx 14),
                    PokeCField
                      (HsStrLit "primitive_li")
                      (Idx 29)
                      (Idx 15),
                    PokeCField
                      (HsStrLit "primitive_sl")
                      (Idx 29)
                      (Idx 16),
                    PokeCField
                      (HsStrLit "primitive_sli")
                      (Idx 29)
                      (Idx 17),
                    PokeCField
                      (HsStrLit "primitive_ul")
                      (Idx 29)
                      (Idx 18),
                    PokeCField
                      (HsStrLit "primitive_uli")
                      (Idx 29)
                      (Idx 19),
                    PokeCField
                      (HsStrLit "primitive_ll")
                      (Idx 29)
                      (Idx 20),
                    PokeCField
                      (HsStrLit "primitive_lli")
                      (Idx 29)
                      (Idx 21),
                    PokeCField
                      (HsStrLit "primitive_sll")
                      (Idx 29)
                      (Idx 22),
                    PokeCField
                      (HsStrLit "primitive_slli")
                      (Idx 29)
                      (Idx 23),
                    PokeCField
                      (HsStrLit "primitive_ull")
                      (Idx 29)
                      (Idx 24),
                    PokeCField
                      (HsStrLit "primitive_ulli")
                      (Idx 29)
                      (Idx 25),
                    PokeCField
                      (HsStrLit "primitive_f")
                      (Idx 29)
                      (Idx 26),
                    PokeCField
                      (HsStrLit "primitive_d")
                      (Idx 29)
                      (Idx 27)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Primitive",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Primitive",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_c",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCChar,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_c",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_sc",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCSChar,
          hasCFieldInstanceFieldOffset =
          1},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_sc",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCSChar,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_uc",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCUChar,
          hasCFieldInstanceFieldOffset =
          2},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_uc",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUChar,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_s",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCShort,
          hasCFieldInstanceFieldOffset =
          4},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_s",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCShort,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_si",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCShort,
          hasCFieldInstanceFieldOffset =
          6},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_si",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCShort,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_ss",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCShort,
          hasCFieldInstanceFieldOffset =
          8},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_ss",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCShort,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_ssi",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCShort,
          hasCFieldInstanceFieldOffset =
          10},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_ssi",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCShort,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_us",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCUShort,
          hasCFieldInstanceFieldOffset =
          12},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_us",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUShort,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_usi",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCUShort,
          hasCFieldInstanceFieldOffset =
          14},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_usi",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUShort,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_i",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          16},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_i",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_s2",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          20},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_s2",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_si2",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          24},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_si2",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_u",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCUInt,
          hasCFieldInstanceFieldOffset =
          28},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_u",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_ui",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCUInt,
          hasCFieldInstanceFieldOffset =
          32},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_ui",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_l",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCLong,
          hasCFieldInstanceFieldOffset =
          40},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_l",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_li",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCLong,
          hasCFieldInstanceFieldOffset =
          48},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_li",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_sl",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCLong,
          hasCFieldInstanceFieldOffset =
          56},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_sl",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_sli",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCLong,
          hasCFieldInstanceFieldOffset =
          64},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_sli",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_ul",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCULong,
          hasCFieldInstanceFieldOffset =
          72},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_ul",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCULong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_uli",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCULong,
          hasCFieldInstanceFieldOffset =
          80},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_uli",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCULong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_ll",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCLLong,
          hasCFieldInstanceFieldOffset =
          88},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_ll",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_lli",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCLLong,
          hasCFieldInstanceFieldOffset =
          96},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_lli",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_sll",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCLLong,
          hasCFieldInstanceFieldOffset =
          104},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_sll",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_slli",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCLLong,
          hasCFieldInstanceFieldOffset =
          112},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_slli",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_ull",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCULLong,
          hasCFieldInstanceFieldOffset =
          120},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_ull",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCULLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_ulli",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCULLong,
          hasCFieldInstanceFieldOffset =
          128},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_ulli",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCULLong,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_f",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCFloat,
          hasCFieldInstanceFieldOffset =
          136},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_f",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCFloat,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "primitive_d",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCDouble,
          hasCFieldInstanceFieldOffset =
          144},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Primitive"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "primitive_d",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCDouble,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing}]
