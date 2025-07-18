[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Triple",
      newtypeConstr = HsName
        "@NsConstr"
        "Triple",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Triple",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "fixedarray.h:1:13",
          declId = NamePair {
            nameC = Name "triple",
            nameHsIdent = HsIdentifier
              "Triple"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "fixedarray.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Triple",
              newtypeField = HsName
                "@NsVar"
                "un_Triple"},
            typedefType = TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable],
      newtypeComment = Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Triple",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Triple",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Triple",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Example",
      structConstr = HsName
        "@NsConstr"
        "Example",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "example_triple",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCInt),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "fixedarray.h:4:9",
              structFieldName = NamePair {
                nameC = Name "triple",
                nameHsIdent = HsIdentifier
                  "example_triple"},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "example_sudoku",
          fieldType = HsConstArray
            3
            (HsConstArray
              3
              (HsPrimType HsPrimCInt)),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "fixedarray.h:5:9",
              structFieldName = NamePair {
                nameC = Name "sudoku",
                nameHsIdent = HsIdentifier
                  "example_sudoku"},
              structFieldType = TypeConstArray
                3
                (TypeConstArray
                  3
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              structFieldOffset = 96,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "fixedarray.h:3:8",
            declId = NamePair {
              nameC = Name "Example",
              nameHsIdent = HsIdentifier
                "Example"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "fixedarray.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Example"),
              structSizeof = 48,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "fixedarray.h:4:9",
                  structFieldName = NamePair {
                    nameC = Name "triple",
                    nameHsIdent = HsIdentifier
                      "example_triple"},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc =
                  "fixedarray.h:5:9",
                  structFieldName = NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = HsIdentifier
                      "example_sudoku"},
                  structFieldType = TypeConstArray
                    3
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 96,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Example",
          structConstr = HsName
            "@NsConstr"
            "Example",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "example_triple",
              fieldType = HsConstArray
                3
                (HsPrimType HsPrimCInt),
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "fixedarray.h:4:9",
                  structFieldName = NamePair {
                    nameC = Name "triple",
                    nameHsIdent = HsIdentifier
                      "example_triple"},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "example_sudoku",
              fieldType = HsConstArray
                3
                (HsConstArray
                  3
                  (HsPrimType HsPrimCInt)),
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "fixedarray.h:5:9",
                  structFieldName = NamePair {
                    nameC = Name "sudoku",
                    nameHsIdent = HsIdentifier
                      "example_sudoku"},
                  structFieldType = TypeConstArray
                    3
                    (TypeConstArray
                      3
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 96,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "fixedarray.h:3:8",
                declId = NamePair {
                  nameC = Name "Example",
                  nameHsIdent = HsIdentifier
                    "Example"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "fixedarray.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Example"),
                  structSizeof = 48,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "fixedarray.h:4:9",
                      structFieldName = NamePair {
                        nameC = Name "triple",
                        nameHsIdent = HsIdentifier
                          "example_triple"},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc =
                      "fixedarray.h:5:9",
                      structFieldName = NamePair {
                        nameC = Name "sudoku",
                        nameHsIdent = HsIdentifier
                          "example_sudoku"},
                      structFieldType = TypeConstArray
                        3
                        (TypeConstArray
                          3
                          (TypePrim
                            (PrimIntegral PrimInt Signed))),
                      structFieldOffset = 96,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Nothing}
        StorableInstance {
          storableSizeOf = 48,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Example",
                  structConstr = HsName
                    "@NsConstr"
                    "Example",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_triple",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "fixedarray.h:4:9",
                          structFieldName = NamePair {
                            nameC = Name "triple",
                            nameHsIdent = HsIdentifier
                              "example_triple"},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_sudoku",
                      fieldType = HsConstArray
                        3
                        (HsConstArray
                          3
                          (HsPrimType HsPrimCInt)),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "fixedarray.h:5:9",
                          structFieldName = NamePair {
                            nameC = Name "sudoku",
                            nameHsIdent = HsIdentifier
                              "example_sudoku"},
                          structFieldType = TypeConstArray
                            3
                            (TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 96,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "fixedarray.h:3:8",
                        declId = NamePair {
                          nameC = Name "Example",
                          nameHsIdent = HsIdentifier
                            "Example"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "fixedarray.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Example"),
                          structSizeof = 48,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "fixedarray.h:4:9",
                              structFieldName = NamePair {
                                nameC = Name "triple",
                                nameHsIdent = HsIdentifier
                                  "example_triple"},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "fixedarray.h:5:9",
                              structFieldName = NamePair {
                                nameC = Name "sudoku",
                                nameHsIdent = HsIdentifier
                                  "example_sudoku"},
                              structFieldType = TypeConstArray
                                3
                                (TypeConstArray
                                  3
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              structFieldOffset = 96,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Nothing})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 12]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Example",
                  structConstr = HsName
                    "@NsConstr"
                    "Example",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_triple",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCInt),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "fixedarray.h:4:9",
                          structFieldName = NamePair {
                            nameC = Name "triple",
                            nameHsIdent = HsIdentifier
                              "example_triple"},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "example_sudoku",
                      fieldType = HsConstArray
                        3
                        (HsConstArray
                          3
                          (HsPrimType HsPrimCInt)),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "fixedarray.h:5:9",
                          structFieldName = NamePair {
                            nameC = Name "sudoku",
                            nameHsIdent = HsIdentifier
                              "example_sudoku"},
                          structFieldType = TypeConstArray
                            3
                            (TypeConstArray
                              3
                              (TypePrim
                                (PrimIntegral PrimInt Signed))),
                          structFieldOffset = 96,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "fixedarray.h:3:8",
                        declId = NamePair {
                          nameC = Name "Example",
                          nameHsIdent = HsIdentifier
                            "Example"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "fixedarray.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Example"),
                          structSizeof = 48,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "fixedarray.h:4:9",
                              structFieldName = NamePair {
                                nameC = Name "triple",
                                nameHsIdent = HsIdentifier
                                  "example_triple"},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "fixedarray.h:5:9",
                              structFieldName = NamePair {
                                nameC = Name "sudoku",
                                nameHsIdent = HsIdentifier
                                  "example_sudoku"},
                              structFieldType = TypeConstArray
                                3
                                (TypeConstArray
                                  3
                                  (TypePrim
                                    (PrimIntegral PrimInt Signed))),
                              structFieldOffset = 96,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Nothing}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      12
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Example",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Example",
      deriveInstanceComment =
      Nothing}]
