[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "An_pchar",
      newtypeConstr = HsName
        "@NsConstr"
        "An_pchar",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_An_pchar",
        fieldType = HsPtr
          (HsPrimType HsPrimCChar),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "c2hsc.h:8:21",
          declId = NamePair {
            nameC = CName "an_pchar",
            nameHsIdent = HsIdentifier
              "An_pchar"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "An_pchar",
              newtypeField = HsName
                "@NsVar"
                "un_An_pchar"},
            typedefType = TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "An_pchar"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "An_pchar"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "An_pchar"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "An_pchar"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "MyCoolStruct",
      structConstr = HsName
        "@NsConstr"
        "MyCoolStruct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "myCoolStruct_listOfNames",
          fieldType = HsConstArray
            8
            (HsConstArray
              255
              (HsPrimType HsPrimCChar)),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "c2hsc.h:12:10",
              structFieldName = NamePair {
                nameC = CName "listOfNames",
                nameHsIdent = HsIdentifier
                  "myCoolStruct_listOfNames"},
              structFieldType = TypeConstArray
                8
                (TypeConstArray
                  255
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "c2hsc.h:11:9",
            declId = NamePair {
              nameC = CName "MyCoolStruct",
              nameHsIdent = HsIdentifier
                "MyCoolStruct"},
            declOrigin = NameOriginGenerated
              (AnonId "c2hsc.h:11:9"),
            declAliases = [
              CName "MyCoolStruct"]},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "MyCoolStruct"),
              structSizeof = 2040,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc =
                  "c2hsc.h:12:10",
                  structFieldName = NamePair {
                    nameC = CName "listOfNames",
                    nameHsIdent = HsIdentifier
                      "myCoolStruct_listOfNames"},
                  structFieldType = TypeConstArray
                    8
                    (TypeConstArray
                      255
                      (TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))))),
                  structFieldOffset = 0,
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
          "MyCoolStruct",
        structConstr = HsName
          "@NsConstr"
          "MyCoolStruct",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "myCoolStruct_listOfNames",
            fieldType = HsConstArray
              8
              (HsConstArray
                255
                (HsPrimType HsPrimCChar)),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "c2hsc.h:12:10",
                structFieldName = NamePair {
                  nameC = CName "listOfNames",
                  nameHsIdent = HsIdentifier
                    "myCoolStruct_listOfNames"},
                structFieldType = TypeConstArray
                  8
                  (TypeConstArray
                    255
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "c2hsc.h:11:9",
              declId = NamePair {
                nameC = CName "MyCoolStruct",
                nameHsIdent = HsIdentifier
                  "MyCoolStruct"},
              declOrigin = NameOriginGenerated
                (AnonId "c2hsc.h:11:9"),
              declAliases = [
                CName "MyCoolStruct"]},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "MyCoolStruct"),
                structSizeof = 2040,
                structAlignment = 1,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "c2hsc.h:12:10",
                    structFieldName = NamePair {
                      nameC = CName "listOfNames",
                      nameHsIdent = HsIdentifier
                        "myCoolStruct_listOfNames"},
                    structFieldType = TypeConstArray
                      8
                      (TypeConstArray
                        255
                        (TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))))),
                    structFieldOffset = 0,
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
        storableSizeOf = 2040,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "MyCoolStruct",
                structConstr = HsName
                  "@NsConstr"
                  "MyCoolStruct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "myCoolStruct_listOfNames",
                    fieldType = HsConstArray
                      8
                      (HsConstArray
                        255
                        (HsPrimType HsPrimCChar)),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "c2hsc.h:12:10",
                        structFieldName = NamePair {
                          nameC = CName "listOfNames",
                          nameHsIdent = HsIdentifier
                            "myCoolStruct_listOfNames"},
                        structFieldType = TypeConstArray
                          8
                          (TypeConstArray
                            255
                            (TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "c2hsc.h:11:9",
                      declId = NamePair {
                        nameC = CName "MyCoolStruct",
                        nameHsIdent = HsIdentifier
                          "MyCoolStruct"},
                      declOrigin = NameOriginGenerated
                        (AnonId "c2hsc.h:11:9"),
                      declAliases = [
                        CName "MyCoolStruct"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "MyCoolStruct"),
                        structSizeof = 2040,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "c2hsc.h:12:10",
                            structFieldName = NamePair {
                              nameC = CName "listOfNames",
                              nameHsIdent = HsIdentifier
                                "myCoolStruct_listOfNames"},
                            structFieldType = TypeConstArray
                              8
                              (TypeConstArray
                                255
                                (TypePrim
                                  (PrimChar
                                    (PrimSignImplicit
                                      (Just Signed))))),
                            structFieldOffset = 0,
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
            [PeekByteOff (Idx 0) 0]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "MyCoolStruct",
                structConstr = HsName
                  "@NsConstr"
                  "MyCoolStruct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "myCoolStruct_listOfNames",
                    fieldType = HsConstArray
                      8
                      (HsConstArray
                        255
                        (HsPrimType HsPrimCChar)),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "c2hsc.h:12:10",
                        structFieldName = NamePair {
                          nameC = CName "listOfNames",
                          nameHsIdent = HsIdentifier
                            "myCoolStruct_listOfNames"},
                        structFieldType = TypeConstArray
                          8
                          (TypeConstArray
                            255
                            (TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "c2hsc.h:11:9",
                      declId = NamePair {
                        nameC = CName "MyCoolStruct",
                        nameHsIdent = HsIdentifier
                          "MyCoolStruct"},
                      declOrigin = NameOriginGenerated
                        (AnonId "c2hsc.h:11:9"),
                      declAliases = [
                        CName "MyCoolStruct"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "MyCoolStruct"),
                        structSizeof = 2040,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "c2hsc.h:12:10",
                            structFieldName = NamePair {
                              nameC = CName "listOfNames",
                              nameHsIdent = HsIdentifier
                                "myCoolStruct_listOfNames"},
                            structFieldType = TypeConstArray
                              8
                              (TypeConstArray
                                255
                                (TypePrim
                                  (PrimChar
                                    (PrimSignImplicit
                                      (Just Signed))))),
                            structFieldOffset = 0,
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
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "MyCoolStruct"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "MyCoolStruct"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Foo",
      newtypeConstr = HsName
        "@NsConstr"
        "Foo",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Foo",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimCInt))),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "c2hsc.h:16:15",
          declId = NamePair {
            nameC = CName "foo",
            nameHsIdent = HsIdentifier
              "Foo"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Foo",
              newtypeField = HsName
                "@NsVar"
                "un_Foo"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Foo")]
