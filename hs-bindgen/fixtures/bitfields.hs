[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Flags",
      structConstr = Name
        "@NsConstr"
        "Flags",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "flags_fieldX",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:2:10",
                fieldName = NamePair {
                  nameC = Name "fieldX",
                  nameHsIdent = Identifier
                    "flags_fieldX"},
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
              commentOrigin = Just "fieldX",
              commentLocation = Just
                "bitfields.h:2:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "flags_flagA",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:3:9",
                fieldName = NamePair {
                  nameC = Name "flagA",
                  nameHsIdent = Identifier
                    "flags_flagA"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Just 1},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "flagA",
              commentLocation = Just
                "bitfields.h:3:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "flags_flagB",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:4:9",
                fieldName = NamePair {
                  nameC = Name "flagB",
                  nameHsIdent = Identifier
                    "flags_flagB"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 9,
              structFieldWidth = Just 1},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "flagB",
              commentLocation = Just
                "bitfields.h:4:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "flags_flagC",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:5:9",
                fieldName = NamePair {
                  nameC = Name "flagC",
                  nameHsIdent = Identifier
                    "flags_flagC"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 10,
              structFieldWidth = Just 1},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "flagC",
              commentLocation = Just
                "bitfields.h:5:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "flags_fieldY",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:6:10",
                fieldName = NamePair {
                  nameC = Name "fieldY",
                  nameHsIdent = Identifier
                    "flags_fieldY"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 16,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "fieldY",
              commentLocation = Just
                "bitfields.h:6:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "flags_bits",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:7:9",
                fieldName = NamePair {
                  nameC = Name "bits",
                  nameHsIdent = Identifier
                    "flags_bits"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 24,
              structFieldWidth = Just 2},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "bits",
              commentLocation = Just
                "bitfields.h:7:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:1:8",
            declId = NamePair {
              nameC = Name "flags",
              nameHsIdent = Identifier
                "Flags"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["bitfields.h"],
                headerInclude = "bitfields.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Flags"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:2:10",
                    fieldName = NamePair {
                      nameC = Name "fieldX",
                      nameHsIdent = Identifier
                        "flags_fieldX"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "flagA",
                      nameHsIdent = Identifier
                        "flags_flagA"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
                  structFieldWidth = Just 1},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:4:9",
                    fieldName = NamePair {
                      nameC = Name "flagB",
                      nameHsIdent = Identifier
                        "flags_flagB"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 9,
                  structFieldWidth = Just 1},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:5:9",
                    fieldName = NamePair {
                      nameC = Name "flagC",
                      nameHsIdent = Identifier
                        "flags_flagC"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 10,
                  structFieldWidth = Just 1},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:6:10",
                    fieldName = NamePair {
                      nameC = Name "fieldY",
                      nameHsIdent = Identifier
                        "flags_fieldY"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:7:9",
                    fieldName = NamePair {
                      nameC = Name "bits",
                      nameHsIdent = Identifier
                        "flags_bits"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 24,
                  structFieldWidth = Just 2}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "flags",
          commentLocation = Just
            "bitfields.h:1:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bitfields.h"],
              headerInclude = "bitfields.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Flags",
          structConstr = Name
            "@NsConstr"
            "Flags",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "flags_fieldX",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:2:10",
                    fieldName = NamePair {
                      nameC = Name "fieldX",
                      nameHsIdent = Identifier
                        "flags_fieldX"},
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
                  commentOrigin = Just "fieldX",
                  commentLocation = Just
                    "bitfields.h:2:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "flags_flagA",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "flagA",
                      nameHsIdent = Identifier
                        "flags_flagA"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
                  structFieldWidth = Just 1},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "flagA",
                  commentLocation = Just
                    "bitfields.h:3:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "flags_flagB",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:4:9",
                    fieldName = NamePair {
                      nameC = Name "flagB",
                      nameHsIdent = Identifier
                        "flags_flagB"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 9,
                  structFieldWidth = Just 1},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "flagB",
                  commentLocation = Just
                    "bitfields.h:4:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "flags_flagC",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:5:9",
                    fieldName = NamePair {
                      nameC = Name "flagC",
                      nameHsIdent = Identifier
                        "flags_flagC"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 10,
                  structFieldWidth = Just 1},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "flagC",
                  commentLocation = Just
                    "bitfields.h:5:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "flags_fieldY",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:6:10",
                    fieldName = NamePair {
                      nameC = Name "fieldY",
                      nameHsIdent = Identifier
                        "flags_fieldY"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "fieldY",
                  commentLocation = Just
                    "bitfields.h:6:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "flags_bits",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:7:9",
                    fieldName = NamePair {
                      nameC = Name "bits",
                      nameHsIdent = Identifier
                        "flags_bits"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 24,
                  structFieldWidth = Just 2},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "bits",
                  commentLocation = Just
                    "bitfields.h:7:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bitfields.h:1:8",
                declId = NamePair {
                  nameC = Name "flags",
                  nameHsIdent = Identifier
                    "Flags"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["bitfields.h"],
                    headerInclude = "bitfields.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Flags"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:2:10",
                        fieldName = NamePair {
                          nameC = Name "fieldX",
                          nameHsIdent = Identifier
                            "flags_fieldX"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:3:9",
                        fieldName = NamePair {
                          nameC = Name "flagA",
                          nameHsIdent = Identifier
                            "flags_flagA"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 8,
                      structFieldWidth = Just 1},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:4:9",
                        fieldName = NamePair {
                          nameC = Name "flagB",
                          nameHsIdent = Identifier
                            "flags_flagB"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 9,
                      structFieldWidth = Just 1},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:5:9",
                        fieldName = NamePair {
                          nameC = Name "flagC",
                          nameHsIdent = Identifier
                            "flags_flagC"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 10,
                      structFieldWidth = Just 1},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:6:10",
                        fieldName = NamePair {
                          nameC = Name "fieldY",
                          nameHsIdent = Identifier
                            "flags_fieldY"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 16,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:7:9",
                        fieldName = NamePair {
                          nameC = Name "bits",
                          nameHsIdent = Identifier
                            "flags_bits"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 24,
                      structFieldWidth = Just 2}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "flags",
              commentLocation = Just
                "bitfields.h:1:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Flags",
                  structConstr = Name
                    "@NsConstr"
                    "Flags",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_fieldX",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:2:10",
                            fieldName = NamePair {
                              nameC = Name "fieldX",
                              nameHsIdent = Identifier
                                "flags_fieldX"},
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
                          commentOrigin = Just "fieldX",
                          commentLocation = Just
                            "bitfields.h:2:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_flagA",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "flagA",
                              nameHsIdent = Identifier
                                "flags_flagA"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flagA",
                          commentLocation = Just
                            "bitfields.h:3:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_flagB",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:4:9",
                            fieldName = NamePair {
                              nameC = Name "flagB",
                              nameHsIdent = Identifier
                                "flags_flagB"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 9,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flagB",
                          commentLocation = Just
                            "bitfields.h:4:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_flagC",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:5:9",
                            fieldName = NamePair {
                              nameC = Name "flagC",
                              nameHsIdent = Identifier
                                "flags_flagC"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 10,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flagC",
                          commentLocation = Just
                            "bitfields.h:5:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_fieldY",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:6:10",
                            fieldName = NamePair {
                              nameC = Name "fieldY",
                              nameHsIdent = Identifier
                                "flags_fieldY"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "fieldY",
                          commentLocation = Just
                            "bitfields.h:6:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_bits",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:7:9",
                            fieldName = NamePair {
                              nameC = Name "bits",
                              nameHsIdent = Identifier
                                "flags_bits"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 24,
                          structFieldWidth = Just 2},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "bits",
                          commentLocation = Just
                            "bitfields.h:7:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:1:8",
                        declId = NamePair {
                          nameC = Name "flags",
                          nameHsIdent = Identifier
                            "Flags"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Flags"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:2:10",
                                fieldName = NamePair {
                                  nameC = Name "fieldX",
                                  nameHsIdent = Identifier
                                    "flags_fieldX"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "flagA",
                                  nameHsIdent = Identifier
                                    "flags_flagA"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:4:9",
                                fieldName = NamePair {
                                  nameC = Name "flagB",
                                  nameHsIdent = Identifier
                                    "flags_flagB"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 9,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:5:9",
                                fieldName = NamePair {
                                  nameC = Name "flagC",
                                  nameHsIdent = Identifier
                                    "flags_flagC"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 10,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:6:10",
                                fieldName = NamePair {
                                  nameC = Name "fieldY",
                                  nameHsIdent = Identifier
                                    "flags_fieldY"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:7:9",
                                fieldName = NamePair {
                                  nameC = Name "bits",
                                  nameHsIdent = Identifier
                                    "flags_bits"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 24,
                              structFieldWidth = Just 2}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "flags",
                      commentLocation = Just
                        "bitfields.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekBitOffWidth (Idx 0) 8 1,
                PeekBitOffWidth (Idx 0) 9 1,
                PeekBitOffWidth (Idx 0) 10 1,
                PeekByteOff (Idx 0) 2,
                PeekBitOffWidth (Idx 0) 24 2]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Flags",
                  structConstr = Name
                    "@NsConstr"
                    "Flags",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_fieldX",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:2:10",
                            fieldName = NamePair {
                              nameC = Name "fieldX",
                              nameHsIdent = Identifier
                                "flags_fieldX"},
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
                          commentOrigin = Just "fieldX",
                          commentLocation = Just
                            "bitfields.h:2:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_flagA",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "flagA",
                              nameHsIdent = Identifier
                                "flags_flagA"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flagA",
                          commentLocation = Just
                            "bitfields.h:3:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_flagB",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:4:9",
                            fieldName = NamePair {
                              nameC = Name "flagB",
                              nameHsIdent = Identifier
                                "flags_flagB"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 9,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flagB",
                          commentLocation = Just
                            "bitfields.h:4:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_flagC",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:5:9",
                            fieldName = NamePair {
                              nameC = Name "flagC",
                              nameHsIdent = Identifier
                                "flags_flagC"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 10,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "flagC",
                          commentLocation = Just
                            "bitfields.h:5:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_fieldY",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:6:10",
                            fieldName = NamePair {
                              nameC = Name "fieldY",
                              nameHsIdent = Identifier
                                "flags_fieldY"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignImplicit
                                (Just Signed))),
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "fieldY",
                          commentLocation = Just
                            "bitfields.h:6:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "flags_bits",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:7:9",
                            fieldName = NamePair {
                              nameC = Name "bits",
                              nameHsIdent = Identifier
                                "flags_bits"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 24,
                          structFieldWidth = Just 2},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "bits",
                          commentLocation = Just
                            "bitfields.h:7:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:1:8",
                        declId = NamePair {
                          nameC = Name "flags",
                          nameHsIdent = Identifier
                            "Flags"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Flags"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:2:10",
                                fieldName = NamePair {
                                  nameC = Name "fieldX",
                                  nameHsIdent = Identifier
                                    "flags_fieldX"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "flagA",
                                  nameHsIdent = Identifier
                                    "flags_flagA"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:4:9",
                                fieldName = NamePair {
                                  nameC = Name "flagB",
                                  nameHsIdent = Identifier
                                    "flags_flagB"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 9,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:5:9",
                                fieldName = NamePair {
                                  nameC = Name "flagC",
                                  nameHsIdent = Identifier
                                    "flags_flagC"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 10,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:6:10",
                                fieldName = NamePair {
                                  nameC = Name "fieldY",
                                  nameHsIdent = Identifier
                                    "flags_fieldY"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:7:9",
                                fieldName = NamePair {
                                  nameC = Name "bits",
                                  nameHsIdent = Identifier
                                    "flags_bits"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 24,
                              structFieldWidth = Just 2}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "flags",
                      commentLocation = Just
                        "bitfields.h:1:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}}
                (Add 6)
                (Seq
                  [
                    PokeByteOff (Idx 7) 0 (Idx 0),
                    PokeBitOffWidth
                      (Idx 7)
                      8
                      1
                      (Idx 1),
                    PokeBitOffWidth
                      (Idx 7)
                      9
                      1
                      (Idx 2),
                    PokeBitOffWidth
                      (Idx 7)
                      10
                      1
                      (Idx 3),
                    PokeByteOff (Idx 7) 2 (Idx 4),
                    PokeBitOffWidth
                      (Idx 7)
                      24
                      2
                      (Idx 5)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Flags",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Flags",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Overflow32",
      structConstr = Name
        "@NsConstr"
        "Overflow32",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "overflow32_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:13:9",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "overflow32_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Just 17},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "bitfields.h:13:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "overflow32_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:14:9",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "overflow32_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Just 17},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "bitfields.h:14:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "overflow32_z",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:15:9",
                fieldName = NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier
                    "overflow32_z"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Just 17},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Just
                "bitfields.h:15:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:12:8",
            declId = NamePair {
              nameC = Name "overflow32",
              nameHsIdent = Identifier
                "Overflow32"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["bitfields.h"],
                headerInclude = "bitfields.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Overflow32"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:13:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "overflow32_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:14:9",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "overflow32_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:15:9",
                    fieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = Identifier
                        "overflow32_z"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Just 17}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "overflow32",
          commentLocation = Just
            "bitfields.h:12:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bitfields.h"],
              headerInclude = "bitfields.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Overflow32",
          structConstr = Name
            "@NsConstr"
            "Overflow32",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "overflow32_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:13:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "overflow32_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 17},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "bitfields.h:13:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "overflow32_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:14:9",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "overflow32_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Just 17},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "bitfields.h:14:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "overflow32_z",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:15:9",
                    fieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = Identifier
                        "overflow32_z"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Just 17},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "z",
                  commentLocation = Just
                    "bitfields.h:15:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bitfields.h:12:8",
                declId = NamePair {
                  nameC = Name "overflow32",
                  nameHsIdent = Identifier
                    "Overflow32"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["bitfields.h"],
                    headerInclude = "bitfields.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Overflow32"),
                  structSizeof = 12,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:13:9",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "overflow32_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Just 17},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:14:9",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "overflow32_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Just 17},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:15:9",
                        fieldName = NamePair {
                          nameC = Name "z",
                          nameHsIdent = Identifier
                            "overflow32_z"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 64,
                      structFieldWidth = Just 17}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "overflow32",
              commentLocation = Just
                "bitfields.h:12:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 12,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Overflow32",
                  structConstr = Name
                    "@NsConstr"
                    "Overflow32",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:13:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "overflow32_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:13:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:14:9",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "overflow32_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:14:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32_z",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:15:9",
                            fieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = Identifier
                                "overflow32_z"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "z",
                          commentLocation = Just
                            "bitfields.h:15:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:12:8",
                        declId = NamePair {
                          nameC = Name "overflow32",
                          nameHsIdent = Identifier
                            "Overflow32"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Overflow32"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:13:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "overflow32_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:14:9",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "overflow32_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:15:9",
                                fieldName = NamePair {
                                  nameC = Name "z",
                                  nameHsIdent = Identifier
                                    "overflow32_z"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 64,
                              structFieldWidth = Just 17}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "overflow32",
                      commentLocation = Just
                        "bitfields.h:12:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}})
              [
                PeekBitOffWidth (Idx 0) 0 17,
                PeekBitOffWidth (Idx 0) 32 17,
                PeekBitOffWidth (Idx 0) 64 17]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Overflow32",
                  structConstr = Name
                    "@NsConstr"
                    "Overflow32",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:13:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "overflow32_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:13:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:14:9",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "overflow32_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:14:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32_z",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:15:9",
                            fieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = Identifier
                                "overflow32_z"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "z",
                          commentLocation = Just
                            "bitfields.h:15:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:12:8",
                        declId = NamePair {
                          nameC = Name "overflow32",
                          nameHsIdent = Identifier
                            "Overflow32"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Overflow32"),
                          structSizeof = 12,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:13:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "overflow32_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:14:9",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "overflow32_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:15:9",
                                fieldName = NamePair {
                                  nameC = Name "z",
                                  nameHsIdent = Identifier
                                    "overflow32_z"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 64,
                              structFieldWidth = Just 17}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "overflow32",
                      commentLocation = Just
                        "bitfields.h:12:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeBitOffWidth
                      (Idx 4)
                      0
                      17
                      (Idx 0),
                    PokeBitOffWidth
                      (Idx 4)
                      32
                      17
                      (Idx 1),
                    PokeBitOffWidth
                      (Idx 4)
                      64
                      17
                      (Idx 2)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Overflow32",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Overflow32",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Overflow32b",
      structConstr = Name
        "@NsConstr"
        "Overflow32b",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "overflow32b_x",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:19:10",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "overflow32b_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 0,
              structFieldWidth = Just 17},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "bitfields.h:19:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "overflow32b_y",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:20:10",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "overflow32b_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 17,
              structFieldWidth = Just 17},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "bitfields.h:20:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "overflow32b_z",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:21:10",
                fieldName = NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier
                    "overflow32b_z"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 34,
              structFieldWidth = Just 17},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Just
                "bitfields.h:21:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:18:8",
            declId = NamePair {
              nameC = Name "overflow32b",
              nameHsIdent = Identifier
                "Overflow32b"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["bitfields.h"],
                headerInclude = "bitfields.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Overflow32b"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:19:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "overflow32b_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:20:10",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "overflow32b_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 17,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:21:10",
                    fieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = Identifier
                        "overflow32b_z"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 34,
                  structFieldWidth = Just 17}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "overflow32b",
          commentLocation = Just
            "bitfields.h:18:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bitfields.h"],
              headerInclude = "bitfields.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Overflow32b",
          structConstr = Name
            "@NsConstr"
            "Overflow32b",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "overflow32b_x",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:19:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "overflow32b_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 17},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "bitfields.h:19:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "overflow32b_y",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:20:10",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "overflow32b_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 17,
                  structFieldWidth = Just 17},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "bitfields.h:20:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "overflow32b_z",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:21:10",
                    fieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = Identifier
                        "overflow32b_z"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 34,
                  structFieldWidth = Just 17},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "z",
                  commentLocation = Just
                    "bitfields.h:21:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bitfields.h:18:8",
                declId = NamePair {
                  nameC = Name "overflow32b",
                  nameHsIdent = Identifier
                    "Overflow32b"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["bitfields.h"],
                    headerInclude = "bitfields.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Overflow32b"),
                  structSizeof = 8,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:19:10",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "overflow32b_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Just 17},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:20:10",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "overflow32b_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 17,
                      structFieldWidth = Just 17},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:21:10",
                        fieldName = NamePair {
                          nameC = Name "z",
                          nameHsIdent = Identifier
                            "overflow32b_z"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 34,
                      structFieldWidth = Just 17}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "overflow32b",
              commentLocation = Just
                "bitfields.h:18:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Overflow32b",
                  structConstr = Name
                    "@NsConstr"
                    "Overflow32b",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32b_x",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:19:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "overflow32b_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:19:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32b_y",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:20:10",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "overflow32b_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 17,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:20:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32b_z",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:21:10",
                            fieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = Identifier
                                "overflow32b_z"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 34,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "z",
                          commentLocation = Just
                            "bitfields.h:21:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:18:8",
                        declId = NamePair {
                          nameC = Name "overflow32b",
                          nameHsIdent = Identifier
                            "Overflow32b"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Overflow32b"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:19:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "overflow32b_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:20:10",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "overflow32b_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 17,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:21:10",
                                fieldName = NamePair {
                                  nameC = Name "z",
                                  nameHsIdent = Identifier
                                    "overflow32b_z"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 34,
                              structFieldWidth = Just 17}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "overflow32b",
                      commentLocation = Just
                        "bitfields.h:18:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}})
              [
                PeekBitOffWidth (Idx 0) 0 17,
                PeekBitOffWidth (Idx 0) 17 17,
                PeekBitOffWidth (Idx 0) 34 17]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Overflow32b",
                  structConstr = Name
                    "@NsConstr"
                    "Overflow32b",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32b_x",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:19:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "overflow32b_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:19:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32b_y",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:20:10",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "overflow32b_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 17,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:20:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32b_z",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:21:10",
                            fieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = Identifier
                                "overflow32b_z"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 34,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "z",
                          commentLocation = Just
                            "bitfields.h:21:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:18:8",
                        declId = NamePair {
                          nameC = Name "overflow32b",
                          nameHsIdent = Identifier
                            "Overflow32b"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Overflow32b"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:19:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "overflow32b_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:20:10",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "overflow32b_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 17,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:21:10",
                                fieldName = NamePair {
                                  nameC = Name "z",
                                  nameHsIdent = Identifier
                                    "overflow32b_z"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 34,
                              structFieldWidth = Just 17}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "overflow32b",
                      commentLocation = Just
                        "bitfields.h:18:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeBitOffWidth
                      (Idx 4)
                      0
                      17
                      (Idx 0),
                    PokeBitOffWidth
                      (Idx 4)
                      17
                      17
                      (Idx 1),
                    PokeBitOffWidth
                      (Idx 4)
                      34
                      17
                      (Idx 2)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Overflow32b",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Overflow32b",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Overflow32c",
      structConstr = Name
        "@NsConstr"
        "Overflow32c",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "overflow32c_x",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:25:10",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "overflow32c_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 0,
              structFieldWidth = Just 17},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "bitfields.h:25:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "overflow32c_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:26:10",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "overflow32c_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Just 17},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "bitfields.h:26:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "overflow32c_z",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:27:10",
                fieldName = NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier
                    "overflow32c_z"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 64,
              structFieldWidth = Just 17},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Just
                "bitfields.h:27:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:24:8",
            declId = NamePair {
              nameC = Name "overflow32c",
              nameHsIdent = Identifier
                "Overflow32c"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["bitfields.h"],
                headerInclude = "bitfields.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "Overflow32c"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:25:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "overflow32c_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:26:10",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "overflow32c_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:27:10",
                    fieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = Identifier
                        "overflow32c_z"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Just 17}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "overflow32c",
          commentLocation = Just
            "bitfields.h:24:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bitfields.h"],
              headerInclude = "bitfields.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Overflow32c",
          structConstr = Name
            "@NsConstr"
            "Overflow32c",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "overflow32c_x",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:25:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "overflow32c_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 17},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "bitfields.h:25:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "overflow32c_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:26:10",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "overflow32c_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Just 17},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "bitfields.h:26:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "overflow32c_z",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:27:10",
                    fieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = Identifier
                        "overflow32c_z"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Just 17},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "z",
                  commentLocation = Just
                    "bitfields.h:27:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bitfields.h:24:8",
                declId = NamePair {
                  nameC = Name "overflow32c",
                  nameHsIdent = Identifier
                    "Overflow32c"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["bitfields.h"],
                    headerInclude = "bitfields.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "Overflow32c"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:25:10",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "overflow32c_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Just 17},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:26:10",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "overflow32c_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Just 17},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:27:10",
                        fieldName = NamePair {
                          nameC = Name "z",
                          nameHsIdent = Identifier
                            "overflow32c_z"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 64,
                      structFieldWidth = Just 17}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "overflow32c",
              commentLocation = Just
                "bitfields.h:24:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Overflow32c",
                  structConstr = Name
                    "@NsConstr"
                    "Overflow32c",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32c_x",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:25:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "overflow32c_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:25:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32c_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:26:10",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "overflow32c_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:26:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32c_z",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:27:10",
                            fieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = Identifier
                                "overflow32c_z"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "z",
                          commentLocation = Just
                            "bitfields.h:27:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:24:8",
                        declId = NamePair {
                          nameC = Name "overflow32c",
                          nameHsIdent = Identifier
                            "Overflow32c"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Overflow32c"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:25:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "overflow32c_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:26:10",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "overflow32c_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:27:10",
                                fieldName = NamePair {
                                  nameC = Name "z",
                                  nameHsIdent = Identifier
                                    "overflow32c_z"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 64,
                              structFieldWidth = Just 17}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "overflow32c",
                      commentLocation = Just
                        "bitfields.h:24:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}})
              [
                PeekBitOffWidth (Idx 0) 0 17,
                PeekBitOffWidth (Idx 0) 32 17,
                PeekBitOffWidth (Idx 0) 64 17]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Overflow32c",
                  structConstr = Name
                    "@NsConstr"
                    "Overflow32c",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32c_x",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:25:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "overflow32c_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:25:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32c_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:26:10",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "overflow32c_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:26:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow32c_z",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:27:10",
                            fieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = Identifier
                                "overflow32c_z"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Just 17},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "z",
                          commentLocation = Just
                            "bitfields.h:27:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:24:8",
                        declId = NamePair {
                          nameC = Name "overflow32c",
                          nameHsIdent = Identifier
                            "Overflow32c"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "Overflow32c"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:25:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "overflow32c_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:26:10",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "overflow32c_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Just 17},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:27:10",
                                fieldName = NamePair {
                                  nameC = Name "z",
                                  nameHsIdent = Identifier
                                    "overflow32c_z"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 64,
                              structFieldWidth = Just 17}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "overflow32c",
                      commentLocation = Just
                        "bitfields.h:24:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeBitOffWidth
                      (Idx 4)
                      0
                      17
                      (Idx 0),
                    PokeBitOffWidth
                      (Idx 4)
                      32
                      17
                      (Idx 1),
                    PokeBitOffWidth
                      (Idx 4)
                      64
                      17
                      (Idx 2)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Overflow32c",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Overflow32c",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Overflow64",
      structConstr = Name
        "@NsConstr"
        "Overflow64",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "overflow64_x",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:31:10",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "overflow64_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 0,
              structFieldWidth = Just 33},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "bitfields.h:31:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "overflow64_y",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:32:10",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "overflow64_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 64,
              structFieldWidth = Just 33},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "bitfields.h:32:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:30:8",
            declId = NamePair {
              nameC = Name "overflow64",
              nameHsIdent = Identifier
                "Overflow64"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["bitfields.h"],
                headerInclude = "bitfields.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Overflow64"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:31:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "overflow64_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 33},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:32:10",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "overflow64_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Just 33}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "overflow64",
          commentLocation = Just
            "bitfields.h:30:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bitfields.h"],
              headerInclude = "bitfields.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Overflow64",
          structConstr = Name
            "@NsConstr"
            "Overflow64",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "overflow64_x",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:31:10",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "overflow64_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 33},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "bitfields.h:31:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "overflow64_y",
              fieldType = HsPrimType
                HsPrimCLong,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:32:10",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "overflow64_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Just 33},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "bitfields.h:32:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bitfields.h:30:8",
                declId = NamePair {
                  nameC = Name "overflow64",
                  nameHsIdent = Identifier
                    "Overflow64"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["bitfields.h"],
                    headerInclude = "bitfields.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Overflow64"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:31:10",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "overflow64_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Just 33},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:32:10",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "overflow64_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimLong Signed),
                      structFieldOffset = 64,
                      structFieldWidth = Just 33}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "overflow64",
              commentLocation = Just
                "bitfields.h:30:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Overflow64",
                  structConstr = Name
                    "@NsConstr"
                    "Overflow64",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow64_x",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:31:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "overflow64_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Just 33},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:31:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow64_y",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:32:10",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "overflow64_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Just 33},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:32:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:30:8",
                        declId = NamePair {
                          nameC = Name "overflow64",
                          nameHsIdent = Identifier
                            "Overflow64"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Overflow64"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:31:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "overflow64_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Just 33},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:32:10",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "overflow64_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 64,
                              structFieldWidth = Just 33}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "overflow64",
                      commentLocation = Just
                        "bitfields.h:30:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}})
              [
                PeekBitOffWidth (Idx 0) 0 33,
                PeekBitOffWidth (Idx 0) 64 33]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Overflow64",
                  structConstr = Name
                    "@NsConstr"
                    "Overflow64",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow64_x",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:31:10",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "overflow64_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Just 33},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:31:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "overflow64_y",
                      fieldType = HsPrimType
                        HsPrimCLong,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:32:10",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "overflow64_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          structFieldOffset = 64,
                          structFieldWidth = Just 33},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:32:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:30:8",
                        declId = NamePair {
                          nameC = Name "overflow64",
                          nameHsIdent = Identifier
                            "Overflow64"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Overflow64"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:31:10",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "overflow64_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Just 33},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:32:10",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "overflow64_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimLong Signed),
                              structFieldOffset = 64,
                              structFieldWidth = Just 33}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "overflow64",
                      commentLocation = Just
                        "bitfields.h:30:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeBitOffWidth
                      (Idx 3)
                      0
                      33
                      (Idx 0),
                    PokeBitOffWidth
                      (Idx 3)
                      64
                      33
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Overflow64",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Overflow64",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "AlignA",
      structConstr = Name
        "@NsConstr"
        "AlignA",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "alignA_x",
          fieldType = HsPrimType
            HsPrimCUChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:37:16",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "alignA_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)),
              structFieldOffset = 0,
              structFieldWidth = Just 1},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "bitfields.h:37:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "alignA_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:38:6",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "alignA_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 1,
              structFieldWidth = Just 10},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "bitfields.h:38:6",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:36:8",
            declId = NamePair {
              nameC = Name "alignA",
              nameHsIdent = Identifier
                "AlignA"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["bitfields.h"],
                headerInclude = "bitfields.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "AlignA"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:37:16",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "alignA_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Unsigned)),
                  structFieldOffset = 0,
                  structFieldWidth = Just 1},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:38:6",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "alignA_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 1,
                  structFieldWidth = Just 10}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "alignA",
          commentLocation = Just
            "bitfields.h:36:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bitfields.h"],
              headerInclude = "bitfields.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "AlignA",
          structConstr = Name
            "@NsConstr"
            "AlignA",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "alignA_x",
              fieldType = HsPrimType
                HsPrimCUChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:37:16",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "alignA_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Unsigned)),
                  structFieldOffset = 0,
                  structFieldWidth = Just 1},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "bitfields.h:37:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "alignA_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:38:6",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "alignA_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 1,
                  structFieldWidth = Just 10},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "bitfields.h:38:6",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bitfields.h:36:8",
                declId = NamePair {
                  nameC = Name "alignA",
                  nameHsIdent = Identifier
                    "AlignA"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["bitfields.h"],
                    headerInclude = "bitfields.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "AlignA"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:37:16",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "alignA_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignExplicit Unsigned)),
                      structFieldOffset = 0,
                      structFieldWidth = Just 1},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:38:6",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "alignA_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 1,
                      structFieldWidth = Just 10}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "alignA",
              commentLocation = Just
                "bitfields.h:36:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "AlignA",
                  structConstr = Name
                    "@NsConstr"
                    "AlignA",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "alignA_x",
                      fieldType = HsPrimType
                        HsPrimCUChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:37:16",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "alignA_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignExplicit Unsigned)),
                          structFieldOffset = 0,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:37:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "alignA_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:38:6",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "alignA_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 1,
                          structFieldWidth = Just 10},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:38:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:36:8",
                        declId = NamePair {
                          nameC = Name "alignA",
                          nameHsIdent = Identifier
                            "AlignA"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "AlignA"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:37:16",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "alignA_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignExplicit Unsigned)),
                              structFieldOffset = 0,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:38:6",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "alignA_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 1,
                              structFieldWidth = Just 10}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "alignA",
                      commentLocation = Just
                        "bitfields.h:36:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}})
              [
                PeekBitOffWidth (Idx 0) 0 1,
                PeekBitOffWidth (Idx 0) 1 10]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "AlignA",
                  structConstr = Name
                    "@NsConstr"
                    "AlignA",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "alignA_x",
                      fieldType = HsPrimType
                        HsPrimCUChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:37:16",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "alignA_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignExplicit Unsigned)),
                          structFieldOffset = 0,
                          structFieldWidth = Just 1},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:37:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "alignA_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:38:6",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "alignA_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 1,
                          structFieldWidth = Just 10},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:38:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:36:8",
                        declId = NamePair {
                          nameC = Name "alignA",
                          nameHsIdent = Identifier
                            "AlignA"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "AlignA"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:37:16",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "alignA_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignExplicit Unsigned)),
                              structFieldOffset = 0,
                              structFieldWidth = Just 1},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:38:6",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "alignA_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 1,
                              structFieldWidth = Just 10}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "alignA",
                      commentLocation = Just
                        "bitfields.h:36:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeBitOffWidth
                      (Idx 3)
                      0
                      1
                      (Idx 0),
                    PokeBitOffWidth
                      (Idx 3)
                      1
                      10
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "AlignA",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "AlignA",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "AlignB",
      structConstr = Name
        "@NsConstr"
        "AlignB",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "alignB_x",
          fieldType = HsPrimType
            HsPrimCUChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:42:16",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "alignB_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)),
              structFieldOffset = 0,
              structFieldWidth = Just 7},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "bitfields.h:42:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "alignB_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "bitfields.h:43:6",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier
                    "alignB_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Just 31},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Just
                "bitfields.h:43:6",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:41:8",
            declId = NamePair {
              nameC = Name "alignB",
              nameHsIdent = Identifier
                "AlignB"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["bitfields.h"],
                headerInclude = "bitfields.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "AlignB"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:42:16",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "alignB_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Unsigned)),
                  structFieldOffset = 0,
                  structFieldWidth = Just 7},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:43:6",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "alignB_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Just 31}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecModule = Nothing,
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "alignB",
          commentLocation = Just
            "bitfields.h:41:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bitfields.h"],
              headerInclude = "bitfields.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "AlignB",
          structConstr = Name
            "@NsConstr"
            "AlignB",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "alignB_x",
              fieldType = HsPrimType
                HsPrimCUChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:42:16",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "alignB_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Unsigned)),
                  structFieldOffset = 0,
                  structFieldWidth = Just 7},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "bitfields.h:42:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "alignB_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "bitfields.h:43:6",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = Identifier
                        "alignB_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Just 31},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "y",
                  commentLocation = Just
                    "bitfields.h:43:6",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["bitfields.h"],
                      headerInclude = "bitfields.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "bitfields.h:41:8",
                declId = NamePair {
                  nameC = Name "alignB",
                  nameHsIdent = Identifier
                    "AlignB"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["bitfields.h"],
                    headerInclude = "bitfields.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "AlignB"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:42:16",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "alignB_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignExplicit Unsigned)),
                      structFieldOffset = 0,
                      structFieldWidth = Just 7},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "bitfields.h:43:6",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = Identifier
                            "alignB_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
                      structFieldWidth = Just 31}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecModule = Nothing,
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "alignB",
              commentLocation = Just
                "bitfields.h:41:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["bitfields.h"],
                  headerInclude = "bitfields.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "AlignB",
                  structConstr = Name
                    "@NsConstr"
                    "AlignB",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "alignB_x",
                      fieldType = HsPrimType
                        HsPrimCUChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:42:16",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "alignB_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignExplicit Unsigned)),
                          structFieldOffset = 0,
                          structFieldWidth = Just 7},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:42:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "alignB_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:43:6",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "alignB_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Just 31},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:43:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:41:8",
                        declId = NamePair {
                          nameC = Name "alignB",
                          nameHsIdent = Identifier
                            "AlignB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "AlignB"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:42:16",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "alignB_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignExplicit Unsigned)),
                              structFieldOffset = 0,
                              structFieldWidth = Just 7},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:43:6",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "alignB_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Just 31}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "alignB",
                      commentLocation = Just
                        "bitfields.h:41:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}})
              [
                PeekBitOffWidth (Idx 0) 0 7,
                PeekBitOffWidth (Idx 0) 32 31]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "AlignB",
                  structConstr = Name
                    "@NsConstr"
                    "AlignB",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "alignB_x",
                      fieldType = HsPrimType
                        HsPrimCUChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:42:16",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "alignB_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimChar
                              (PrimSignExplicit Unsigned)),
                          structFieldOffset = 0,
                          structFieldWidth = Just 7},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "bitfields.h:42:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "alignB_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "bitfields.h:43:6",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = Identifier
                                "alignB_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Just 31},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "y",
                          commentLocation = Just
                            "bitfields.h:43:6",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["bitfields.h"],
                              headerInclude = "bitfields.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "bitfields.h:41:8",
                        declId = NamePair {
                          nameC = Name "alignB",
                          nameHsIdent = Identifier
                            "AlignB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["bitfields.h"],
                            headerInclude = "bitfields.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "AlignB"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:42:16",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "alignB_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignExplicit Unsigned)),
                              structFieldOffset = 0,
                              structFieldWidth = Just 7},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "bitfields.h:43:6",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = Identifier
                                    "alignB_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
                              structFieldWidth = Just 31}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecModule = Nothing,
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "alignB",
                      commentLocation = Just
                        "bitfields.h:41:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["bitfields.h"],
                          headerInclude = "bitfields.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeBitOffWidth
                      (Idx 3)
                      0
                      7
                      (Idx 0),
                    PokeBitOffWidth
                      (Idx 3)
                      32
                      31
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "AlignB",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "AlignB",
      deriveInstanceComment =
      Nothing}]
