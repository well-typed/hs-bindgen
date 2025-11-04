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
                PeekCField
                  (HsStrLit "flags_fieldX")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "flags_flagA")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "flags_flagB")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "flags_flagC")
                  (Idx 0),
                PeekCField
                  (HsStrLit "flags_fieldY")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "flags_bits")
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
                    PokeCField
                      (HsStrLit "flags_fieldX")
                      (Idx 7)
                      (Idx 0),
                    PokeCBitfield
                      (HsStrLit "flags_flagA")
                      (Idx 7)
                      (Idx 1),
                    PokeCBitfield
                      (HsStrLit "flags_flagB")
                      (Idx 7)
                      (Idx 2),
                    PokeCBitfield
                      (HsStrLit "flags_flagC")
                      (Idx 7)
                      (Idx 3),
                    PokeCField
                      (HsStrLit "flags_fieldY")
                      (Idx 7)
                      (Idx 4),
                    PokeCBitfield
                      (HsStrLit "flags_bits")
                      (Idx 7)
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Flags"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "flags_fieldX",
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
            (Name "@NsTypeConstr" "Flags"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "flags_fieldX",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Flags"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "flags_flagA",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCInt,
          hasCBitfieldInstanceBitOffset =
          8,
          hasCBitfieldInstanceBitWidth =
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
            (Name "@NsTypeConstr" "Flags"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "flags_flagA",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Flags"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "flags_flagB",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCInt,
          hasCBitfieldInstanceBitOffset =
          9,
          hasCBitfieldInstanceBitWidth =
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
            (Name "@NsTypeConstr" "Flags"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "flags_flagB",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Flags"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "flags_flagC",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCInt,
          hasCBitfieldInstanceBitOffset =
          10,
          hasCBitfieldInstanceBitWidth =
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
            (Name "@NsTypeConstr" "Flags"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "flags_flagC",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Flags"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "flags_fieldY",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCChar,
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
            (Name "@NsTypeConstr" "Flags"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "flags_fieldY",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCChar,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Flags"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "flags_bits",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCInt,
          hasCBitfieldInstanceBitOffset =
          24,
          hasCBitfieldInstanceBitWidth =
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
            (Name "@NsTypeConstr" "Flags"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "flags_bits",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
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
                PeekCBitfield
                  (HsStrLit "overflow32_x")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "overflow32_y")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "overflow32_z")
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
                    PokeCBitfield
                      (HsStrLit "overflow32_x")
                      (Idx 4)
                      (Idx 0),
                    PokeCBitfield
                      (HsStrLit "overflow32_y")
                      (Idx 4)
                      (Idx 1),
                    PokeCBitfield
                      (HsStrLit "overflow32_z")
                      (Idx 4)
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow32"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow32_x",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCInt,
          hasCBitfieldInstanceBitOffset =
          0,
          hasCBitfieldInstanceBitWidth =
          17},
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
              "Overflow32"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow32_x",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow32"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow32_y",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCInt,
          hasCBitfieldInstanceBitOffset =
          32,
          hasCBitfieldInstanceBitWidth =
          17},
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
              "Overflow32"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow32_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow32"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow32_z",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCInt,
          hasCBitfieldInstanceBitOffset =
          64,
          hasCBitfieldInstanceBitWidth =
          17},
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
              "Overflow32"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow32_z",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
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
                PeekCBitfield
                  (HsStrLit "overflow32b_x")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "overflow32b_y")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "overflow32b_z")
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
                    PokeCBitfield
                      (HsStrLit "overflow32b_x")
                      (Idx 4)
                      (Idx 0),
                    PokeCBitfield
                      (HsStrLit "overflow32b_y")
                      (Idx 4)
                      (Idx 1),
                    PokeCBitfield
                      (HsStrLit "overflow32b_z")
                      (Idx 4)
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow32b"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow32b_x",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCLong,
          hasCBitfieldInstanceBitOffset =
          0,
          hasCBitfieldInstanceBitWidth =
          17},
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
              "Overflow32b"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow32b_x",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow32b"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow32b_y",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCLong,
          hasCBitfieldInstanceBitOffset =
          17,
          hasCBitfieldInstanceBitWidth =
          17},
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
              "Overflow32b"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow32b_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow32b"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow32b_z",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCLong,
          hasCBitfieldInstanceBitOffset =
          34,
          hasCBitfieldInstanceBitWidth =
          17},
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
              "Overflow32b"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow32b_z",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
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
                PeekCBitfield
                  (HsStrLit "overflow32c_x")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "overflow32c_y")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "overflow32c_z")
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
                    PokeCBitfield
                      (HsStrLit "overflow32c_x")
                      (Idx 4)
                      (Idx 0),
                    PokeCBitfield
                      (HsStrLit "overflow32c_y")
                      (Idx 4)
                      (Idx 1),
                    PokeCBitfield
                      (HsStrLit "overflow32c_z")
                      (Idx 4)
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow32c"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow32c_x",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCLong,
          hasCBitfieldInstanceBitOffset =
          0,
          hasCBitfieldInstanceBitWidth =
          17},
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
              "Overflow32c"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow32c_x",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow32c"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow32c_y",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCInt,
          hasCBitfieldInstanceBitOffset =
          32,
          hasCBitfieldInstanceBitWidth =
          17},
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
              "Overflow32c"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow32c_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow32c"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow32c_z",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCLong,
          hasCBitfieldInstanceBitOffset =
          64,
          hasCBitfieldInstanceBitWidth =
          17},
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
              "Overflow32c"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow32c_z",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
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
                PeekCBitfield
                  (HsStrLit "overflow64_x")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "overflow64_y")
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
                    PokeCBitfield
                      (HsStrLit "overflow64_x")
                      (Idx 3)
                      (Idx 0),
                    PokeCBitfield
                      (HsStrLit "overflow64_y")
                      (Idx 3)
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow64"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow64_x",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCLong,
          hasCBitfieldInstanceBitOffset =
          0,
          hasCBitfieldInstanceBitWidth =
          33},
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
              "Overflow64"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow64_x",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Overflow64"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "overflow64_y",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCLong,
          hasCBitfieldInstanceBitOffset =
          64,
          hasCBitfieldInstanceBitWidth =
          33},
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
              "Overflow64"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "overflow64_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCLong,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
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
                PeekCBitfield
                  (HsStrLit "alignA_x")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "alignA_y")
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
                    PokeCBitfield
                      (HsStrLit "alignA_x")
                      (Idx 3)
                      (Idx 0),
                    PokeCBitfield
                      (HsStrLit "alignA_y")
                      (Idx 3)
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "AlignA"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "alignA_x",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCUChar,
          hasCBitfieldInstanceBitOffset =
          0,
          hasCBitfieldInstanceBitWidth =
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
            (Name "@NsTypeConstr" "AlignA"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "alignA_x",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUChar,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "AlignA"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "alignA_y",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCInt,
          hasCBitfieldInstanceBitOffset =
          1,
          hasCBitfieldInstanceBitWidth =
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
            (Name "@NsTypeConstr" "AlignA"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "alignA_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
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
                PeekCBitfield
                  (HsStrLit "alignB_x")
                  (Idx 0),
                PeekCBitfield
                  (HsStrLit "alignB_y")
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
                    PokeCBitfield
                      (HsStrLit "alignB_x")
                      (Idx 3)
                      (Idx 0),
                    PokeCBitfield
                      (HsStrLit "alignB_y")
                      (Idx 3)
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
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "AlignB"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "alignB_x",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCUChar,
          hasCBitfieldInstanceBitOffset =
          0,
          hasCBitfieldInstanceBitWidth =
          7},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "AlignB"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "alignB_x",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCUChar,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCBitfield
        HasCBitfieldInstance {
          hasCBitfieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "AlignB"),
          hasCBitfieldInstanceFieldName =
          Name "@NsVar" "alignB_y",
          hasCBitfieldInstanceCBitfieldType =
          HsPrimType HsPrimCInt,
          hasCBitfieldInstanceBitOffset =
          32,
          hasCBitfieldInstanceBitWidth =
          31},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "AlignB"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "alignB_y",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCBitfield},
      defineInstanceComment =
      Nothing}]
