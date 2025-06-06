[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "C__SFILE",
      structConstr = HsName
        "@NsConstr"
        "C__SFILE",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "__sFILE__r",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:9:9",
              structFieldName = NamePair {
                nameC = CName "_r",
                nameHsIdent = HsIdentifier
                  "__sFILE__r"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "__sFILE__w",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:10:9",
              structFieldName = NamePair {
                nameC = CName "_w",
                nameHsIdent = HsIdentifier
                  "__sFILE__w"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "__sFILE__close",
          fieldType = HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsIO (HsPrimType HsPrimCInt))),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "attributes.h:11:19",
              structFieldName = NamePair {
                nameC = CName "_close",
                nameHsIdent = HsIdentifier
                  "__sFILE__close"},
              structFieldType = TypePointer
                (TypeFun
                  [TypePointer TypeVoid]
                  (TypePrim
                    (PrimIntegral PrimInt Signed))),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "attributes.h:8:16",
            declId = NamePair {
              nameC = CName "__sFILE",
              nameHsIdent = HsIdentifier
                "C__SFILE"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "C__SFILE"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "attributes.h:9:9",
                  structFieldName = NamePair {
                    nameC = CName "_r",
                    nameHsIdent = HsIdentifier
                      "__sFILE__r"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "attributes.h:10:9",
                  structFieldName = NamePair {
                    nameC = CName "_w",
                    nameHsIdent = HsIdentifier
                      "__sFILE__w"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "attributes.h:11:19",
                  structFieldName = NamePair {
                    nameC = CName "_close",
                    nameHsIdent = HsIdentifier
                      "__sFILE__close"},
                  structFieldType = TypePointer
                    (TypeFun
                      [TypePointer TypeVoid]
                      (TypePrim
                        (PrimIntegral PrimInt Signed))),
                  structFieldOffset = 64,
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
          "C__SFILE",
        structConstr = HsName
          "@NsConstr"
          "C__SFILE",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "__sFILE__r",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:9:9",
                structFieldName = NamePair {
                  nameC = CName "_r",
                  nameHsIdent = HsIdentifier
                    "__sFILE__r"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "__sFILE__w",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:10:9",
                structFieldName = NamePair {
                  nameC = CName "_w",
                  nameHsIdent = HsIdentifier
                    "__sFILE__w"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "__sFILE__close",
            fieldType = HsFunPtr
              (HsFun
                (HsPtr (HsPrimType HsPrimVoid))
                (HsIO (HsPrimType HsPrimCInt))),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "attributes.h:11:19",
                structFieldName = NamePair {
                  nameC = CName "_close",
                  nameHsIdent = HsIdentifier
                    "__sFILE__close"},
                structFieldType = TypePointer
                  (TypeFun
                    [TypePointer TypeVoid]
                    (TypePrim
                      (PrimIntegral PrimInt Signed))),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "attributes.h:8:16",
              declId = NamePair {
                nameC = CName "__sFILE",
                nameHsIdent = HsIdentifier
                  "C__SFILE"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "C__SFILE"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "attributes.h:9:9",
                    structFieldName = NamePair {
                      nameC = CName "_r",
                      nameHsIdent = HsIdentifier
                        "__sFILE__r"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "attributes.h:10:9",
                    structFieldName = NamePair {
                      nameC = CName "_w",
                      nameHsIdent = HsIdentifier
                        "__sFILE__w"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "attributes.h:11:19",
                    structFieldName = NamePair {
                      nameC = CName "_close",
                      nameHsIdent = HsIdentifier
                        "__sFILE__close"},
                    structFieldType = TypePointer
                      (TypeFun
                        [TypePointer TypeVoid]
                        (TypePrim
                          (PrimIntegral PrimInt Signed))),
                    structFieldOffset = 64,
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
        storableSizeOf = 16,
        storableAlignment = 8,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "C__SFILE",
                structConstr = HsName
                  "@NsConstr"
                  "C__SFILE",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "__sFILE__r",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:9:9",
                        structFieldName = NamePair {
                          nameC = CName "_r",
                          nameHsIdent = HsIdentifier
                            "__sFILE__r"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "__sFILE__w",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:10:9",
                        structFieldName = NamePair {
                          nameC = CName "_w",
                          nameHsIdent = HsIdentifier
                            "__sFILE__w"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "__sFILE__close",
                    fieldType = HsFunPtr
                      (HsFun
                        (HsPtr (HsPrimType HsPrimVoid))
                        (HsIO (HsPrimType HsPrimCInt))),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:11:19",
                        structFieldName = NamePair {
                          nameC = CName "_close",
                          nameHsIdent = HsIdentifier
                            "__sFILE__close"},
                        structFieldType = TypePointer
                          (TypeFun
                            [TypePointer TypeVoid]
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:8:16",
                      declId = NamePair {
                        nameC = CName "__sFILE",
                        nameHsIdent = HsIdentifier
                          "C__SFILE"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "C__SFILE"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:9:9",
                            structFieldName = NamePair {
                              nameC = CName "_r",
                              nameHsIdent = HsIdentifier
                                "__sFILE__r"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:10:9",
                            structFieldName = NamePair {
                              nameC = CName "_w",
                              nameHsIdent = HsIdentifier
                                "__sFILE__w"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:11:19",
                            structFieldName = NamePair {
                              nameC = CName "_close",
                              nameHsIdent = HsIdentifier
                                "__sFILE__close"},
                            structFieldType = TypePointer
                              (TypeFun
                                [TypePointer TypeVoid]
                                (TypePrim
                                  (PrimIntegral PrimInt Signed))),
                            structFieldOffset = 64,
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
              PeekByteOff (Idx 0) 4,
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
                  "C__SFILE",
                structConstr = HsName
                  "@NsConstr"
                  "C__SFILE",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "__sFILE__r",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:9:9",
                        structFieldName = NamePair {
                          nameC = CName "_r",
                          nameHsIdent = HsIdentifier
                            "__sFILE__r"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "__sFILE__w",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:10:9",
                        structFieldName = NamePair {
                          nameC = CName "_w",
                          nameHsIdent = HsIdentifier
                            "__sFILE__w"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "__sFILE__close",
                    fieldType = HsFunPtr
                      (HsFun
                        (HsPtr (HsPrimType HsPrimVoid))
                        (HsIO (HsPrimType HsPrimCInt))),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "attributes.h:11:19",
                        structFieldName = NamePair {
                          nameC = CName "_close",
                          nameHsIdent = HsIdentifier
                            "__sFILE__close"},
                        structFieldType = TypePointer
                          (TypeFun
                            [TypePointer TypeVoid]
                            (TypePrim
                              (PrimIntegral PrimInt Signed))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "attributes.h:8:16",
                      declId = NamePair {
                        nameC = CName "__sFILE",
                        nameHsIdent = HsIdentifier
                          "C__SFILE"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "C__SFILE"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "attributes.h:9:9",
                            structFieldName = NamePair {
                              nameC = CName "_r",
                              nameHsIdent = HsIdentifier
                                "__sFILE__r"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:10:9",
                            structFieldName = NamePair {
                              nameC = CName "_w",
                              nameHsIdent = HsIdentifier
                                "__sFILE__w"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "attributes.h:11:19",
                            structFieldName = NamePair {
                              nameC = CName "_close",
                              nameHsIdent = HsIdentifier
                                "__sFILE__close"},
                            structFieldType = TypePointer
                              (TypeFun
                                [TypePointer TypeVoid]
                                (TypePrim
                                  (PrimIntegral PrimInt Signed))),
                            structFieldOffset = 64,
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
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "C__SFILE"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "C__SFILE"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "FILE",
      newtypeConstr = HsName
        "@NsConstr"
        "FILE",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_FILE",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "C__SFILE"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "attributes.h:12:3",
          declId = NamePair {
            nameC = CName "FILE",
            nameHsIdent = HsIdentifier
              "FILE"}},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "FILE",
              newtypeField = HsName
                "@NsVar"
                "un_FILE"},
            typedefType = TypeStruct
              NamePair {
                nameC = CName "__sFILE",
                nameHsIdent = HsIdentifier
                  "C__SFILE"}},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "FILE"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "FILE"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "FILE")]
