[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Another_typedef_struct_t",
      structConstr = HsName
        "@NsConstr"
        "Another_typedef_struct_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "another_typedef_struct_t_foo",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:9:22",
              structFieldName = NamePair {
                nameC = CName "foo",
                nameHsIdent = HsIdentifier
                  "another_typedef_struct_t_foo"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "another_typedef_struct_t_bar",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:9:32",
              structFieldName = NamePair {
                nameC = CName "bar",
                nameHsIdent = HsIdentifier
                  "another_typedef_struct_t_bar"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "distilled_lib_1.h:9:9",
            declId = NamePair {
              nameC = CName
                "another_typedef_struct_t",
              nameHsIdent = HsIdentifier
                "Another_typedef_struct_t"},
            declOrigin = NameOriginGenerated
              (AnonId
                "distilled_lib_1.h:9:9"),
            declAliases = [
              CName
                "another_typedef_struct_t"],
            declHeader =
            "distilled_lib_1.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Another_typedef_struct_t"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:9:22",
                  structFieldName = NamePair {
                    nameC = CName "foo",
                    nameHsIdent = HsIdentifier
                      "another_typedef_struct_t_foo"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:9:32",
                  structFieldName = NamePair {
                    nameC = CName "bar",
                    nameHsIdent = HsIdentifier
                      "another_typedef_struct_t_bar"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 32,
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
          "Another_typedef_struct_t",
        structConstr = HsName
          "@NsConstr"
          "Another_typedef_struct_t",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "another_typedef_struct_t_foo",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:9:22",
                structFieldName = NamePair {
                  nameC = CName "foo",
                  nameHsIdent = HsIdentifier
                    "another_typedef_struct_t_foo"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "another_typedef_struct_t_bar",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:9:32",
                structFieldName = NamePair {
                  nameC = CName "bar",
                  nameHsIdent = HsIdentifier
                    "another_typedef_struct_t_bar"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "distilled_lib_1.h:9:9",
              declId = NamePair {
                nameC = CName
                  "another_typedef_struct_t",
                nameHsIdent = HsIdentifier
                  "Another_typedef_struct_t"},
              declOrigin = NameOriginGenerated
                (AnonId
                  "distilled_lib_1.h:9:9"),
              declAliases = [
                CName
                  "another_typedef_struct_t"],
              declHeader =
              "distilled_lib_1.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Another_typedef_struct_t"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:9:22",
                    structFieldName = NamePair {
                      nameC = CName "foo",
                      nameHsIdent = HsIdentifier
                        "another_typedef_struct_t_foo"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:9:32",
                    structFieldName = NamePair {
                      nameC = CName "bar",
                      nameHsIdent = HsIdentifier
                        "another_typedef_struct_t_bar"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 32,
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
        storableSizeOf = 8,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Another_typedef_struct_t",
                structConstr = HsName
                  "@NsConstr"
                  "Another_typedef_struct_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "another_typedef_struct_t_foo",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:9:22",
                        structFieldName = NamePair {
                          nameC = CName "foo",
                          nameHsIdent = HsIdentifier
                            "another_typedef_struct_t_foo"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "another_typedef_struct_t_bar",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:9:32",
                        structFieldName = NamePair {
                          nameC = CName "bar",
                          nameHsIdent = HsIdentifier
                            "another_typedef_struct_t_bar"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "distilled_lib_1.h:9:9",
                      declId = NamePair {
                        nameC = CName
                          "another_typedef_struct_t",
                        nameHsIdent = HsIdentifier
                          "Another_typedef_struct_t"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "distilled_lib_1.h:9:9"),
                      declAliases = [
                        CName
                          "another_typedef_struct_t"],
                      declHeader =
                      "distilled_lib_1.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Another_typedef_struct_t"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:9:22",
                            structFieldName = NamePair {
                              nameC = CName "foo",
                              nameHsIdent = HsIdentifier
                                "another_typedef_struct_t_foo"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:9:32",
                            structFieldName = NamePair {
                              nameC = CName "bar",
                              nameHsIdent = HsIdentifier
                                "another_typedef_struct_t_bar"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 32,
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
              PeekByteOff (Idx 0) 4]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Another_typedef_struct_t",
                structConstr = HsName
                  "@NsConstr"
                  "Another_typedef_struct_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "another_typedef_struct_t_foo",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:9:22",
                        structFieldName = NamePair {
                          nameC = CName "foo",
                          nameHsIdent = HsIdentifier
                            "another_typedef_struct_t_foo"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "another_typedef_struct_t_bar",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:9:32",
                        structFieldName = NamePair {
                          nameC = CName "bar",
                          nameHsIdent = HsIdentifier
                            "another_typedef_struct_t_bar"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "distilled_lib_1.h:9:9",
                      declId = NamePair {
                        nameC = CName
                          "another_typedef_struct_t",
                        nameHsIdent = HsIdentifier
                          "Another_typedef_struct_t"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "distilled_lib_1.h:9:9"),
                      declAliases = [
                        CName
                          "another_typedef_struct_t"],
                      declHeader =
                      "distilled_lib_1.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Another_typedef_struct_t"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:9:22",
                            structFieldName = NamePair {
                              nameC = CName "foo",
                              nameHsIdent = HsIdentifier
                                "another_typedef_struct_t_foo"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:9:32",
                            structFieldName = NamePair {
                              nameC = CName "bar",
                              nameHsIdent = HsIdentifier
                                "another_typedef_struct_t_bar"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 32,
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
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Another_typedef_struct_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Another_typedef_struct_t"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      newtypeConstr = HsName
        "@NsConstr"
        "Another_typedef_enum_e",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Another_typedef_enum_e",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:10:9",
          declId = NamePair {
            nameC = CName
              "another_typedef_enum_e",
            nameHsIdent = HsIdentifier
              "Another_typedef_enum_e"},
          declOrigin = NameOriginGenerated
            (AnonId
              "distilled_lib_1.h:10:9"),
          declAliases = [
            CName "another_typedef_enum_e"],
          declHeader =
          "distilled_lib_1.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Another_typedef_enum_e",
              newtypeField = HsName
                "@NsVar"
                "un_Another_typedef_enum_e"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "distilled_lib_1.h:10:16",
                enumConstantName = NamePair {
                  nameC = CName "FOO",
                  nameHsIdent = HsIdentifier
                    "FOO"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "distilled_lib_1.h:10:21",
                enumConstantName = NamePair {
                  nameC = CName "BAR",
                  nameHsIdent = HsIdentifier
                    "BAR"},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Another_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "Another_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Another_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      StorableInstance {
        storableSizeOf = 4,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Another_typedef_enum_e",
                structConstr = HsName
                  "@NsConstr"
                  "Another_typedef_enum_e",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Another_typedef_enum_e",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "Another_typedef_enum_e",
                structConstr = HsName
                  "@NsConstr"
                  "Another_typedef_enum_e",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Another_typedef_enum_e",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Another_typedef_enum_e"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Another_typedef_enum_e"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Another_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "Another_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Another_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["FOO"]),
          _×_ 1 (NE.fromList ["BAR"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Another_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "Another_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Another_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName "@NsConstr" "FOO")
      (HsName "@NsConstr" "BAR")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Another_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "Another_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Another_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Another_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "Another_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Another_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "FOO",
      patSynType = HsName
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "Another_typedef_enum_e",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "distilled_lib_1.h:10:16",
          enumConstantName = NamePair {
            nameC = CName "FOO",
            nameHsIdent = HsIdentifier
              "FOO"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "BAR",
      patSynType = HsName
        "@NsTypeConstr"
        "Another_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "Another_typedef_enum_e",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "distilled_lib_1.h:10:21",
          enumConstantName = NamePair {
            nameC = CName "BAR",
            nameHsIdent = HsIdentifier
              "BAR"},
          enumConstantValue = 1}},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        5
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "b",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        3
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "sOME_DEFINED_CONSTANT",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        4
        HsPrimCInt},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "A_type_t",
      newtypeConstr = HsName
        "@NsConstr"
        "A_type_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_A_type_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:14:13",
          declId = NamePair {
            nameC = CName "a_type_t",
            nameHsIdent = HsIdentifier
              "A_type_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "distilled_lib_1.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "A_type_t",
              newtypeField = HsName
                "@NsVar"
                "un_A_type_t"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "A_type_t"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Var_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Var_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Var_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:15:13",
          declId = NamePair {
            nameC = CName "var_t",
            nameHsIdent = HsIdentifier
              "Var_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "distilled_lib_1.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Var_t",
              newtypeField = HsName
                "@NsVar"
                "un_Var_t"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Var_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "A_typedef_struct_t",
      structConstr = HsName
        "@NsConstr"
        "A_typedef_struct_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_0",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:37:31",
              structFieldName = NamePair {
                nameC = CName "field_0",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_0"},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_1",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word8"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word8"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:38:31",
              structFieldName = NamePair {
                nameC = CName "field_1",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_1"},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = CName "uint8_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word8"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word8"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 8,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_2",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word16"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word16"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:39:31",
              structFieldName = NamePair {
                nameC = CName "field_2",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_2"},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = CName "uint16_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word16"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word16"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 16,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_3",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word32"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word32"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:40:31",
              structFieldName = NamePair {
                nameC = CName "field_3",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_3"},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = CName "uint32_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word32"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word32"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 32,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_4",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Another_typedef_struct_t"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:41:31",
              structFieldName = NamePair {
                nameC = CName "field_4",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_4"},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (CName
                    "another_typedef_struct_t")
                  (TypeStruct
                    NamePair {
                      nameC = CName
                        "another_typedef_struct_t",
                      nameHsIdent = HsIdentifier
                        "Another_typedef_struct_t"}
                    (NameOriginGenerated
                      (AnonId
                        "distilled_lib_1.h:9:9")))),
              structFieldOffset = 64,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_5",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Another_typedef_struct_t")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:42:31",
              structFieldName = NamePair {
                nameC = CName "field_5",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_5"},
              structFieldType = TypePointer
                (TypeTypedef
                  (TypedefSquashed
                    (CName
                      "another_typedef_struct_t")
                    (TypeStruct
                      NamePair {
                        nameC = CName
                          "another_typedef_struct_t",
                        nameHsIdent = HsIdentifier
                          "Another_typedef_struct_t"}
                      (NameOriginGenerated
                        (AnonId
                          "distilled_lib_1.h:9:9"))))),
              structFieldOffset = 128,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_6",
          fieldType = HsPtr
            (HsPrimType HsPrimVoid),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:43:31",
              structFieldName = NamePair {
                nameC = CName "field_6",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_6"},
              structFieldType = TypePointer
                TypeVoid,
              structFieldOffset = 192,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_7",
          fieldType = HsConstArray
            7
            (HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Word32"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Word32"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:44:31",
              structFieldName = NamePair {
                nameC = CName "field_7",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_7"},
              structFieldType = TypeConstArray
                7
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = CName "uint32_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Word32"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Word32"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}}),
              structFieldOffset = 256,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_8",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Another_typedef_enum_e"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:45:31",
              structFieldName = NamePair {
                nameC = CName "field_8",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_8"},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (CName "another_typedef_enum_e")
                  (TypeEnum
                    NamePair {
                      nameC = CName
                        "another_typedef_enum_e",
                      nameHsIdent = HsIdentifier
                        "Another_typedef_enum_e"}
                    (NameOriginGenerated
                      (AnonId
                        "distilled_lib_1.h:10:9")))),
              structFieldOffset = 480,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_9",
          fieldType = HsConstArray
            4
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Another_typedef_enum_e")),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:46:31",
              structFieldName = NamePair {
                nameC = CName "field_9",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_9"},
              structFieldType = TypeConstArray
                4
                (TypeTypedef
                  (TypedefSquashed
                    (CName "another_typedef_enum_e")
                    (TypeEnum
                      NamePair {
                        nameC = CName
                          "another_typedef_enum_e",
                        nameHsIdent = HsIdentifier
                          "Another_typedef_enum_e"}
                      (NameOriginGenerated
                        (AnonId
                          "distilled_lib_1.h:10:9"))))),
              structFieldOffset = 512,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "a_typedef_struct_t_field_10",
          fieldType = HsConstArray
            5
            (HsConstArray
              3
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Another_typedef_enum_e"))),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "distilled_lib_1.h:47:31",
              structFieldName = NamePair {
                nameC = CName "field_10",
                nameHsIdent = HsIdentifier
                  "a_typedef_struct_t_field_10"},
              structFieldType = TypeConstArray
                5
                (TypeConstArray
                  3
                  (TypeTypedef
                    (TypedefSquashed
                      (CName "another_typedef_enum_e")
                      (TypeEnum
                        NamePair {
                          nameC = CName
                            "another_typedef_enum_e",
                          nameHsIdent = HsIdentifier
                            "Another_typedef_enum_e"}
                        (NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:10:9")))))),
              structFieldOffset = 640,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "distilled_lib_1.h:35:16",
            declId = NamePair {
              nameC = CName
                "a_typedef_struct_t",
              nameHsIdent = HsIdentifier
                "A_typedef_struct_t"},
            declOrigin =
            NameOriginRenamedFrom
              (CName "a_typedef_struct"),
            declAliases = [
              CName "a_typedef_struct_t"],
            declHeader =
            "distilled_lib_1.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "A_typedef_struct_t"),
              structSizeof = 140,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:37:31",
                  structFieldName = NamePair {
                    nameC = CName "field_0",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_0"},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:38:31",
                  structFieldName = NamePair {
                    nameC = CName "field_1",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_1"},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = CName "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word8"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word8"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:39:31",
                  structFieldName = NamePair {
                    nameC = CName "field_2",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_2"},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = CName "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:40:31",
                  structFieldName = NamePair {
                    nameC = CName "field_3",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_3"},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = CName "uint32_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word32"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word32"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:41:31",
                  structFieldName = NamePair {
                    nameC = CName "field_4",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_4"},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (CName
                        "another_typedef_struct_t")
                      (TypeStruct
                        NamePair {
                          nameC = CName
                            "another_typedef_struct_t",
                          nameHsIdent = HsIdentifier
                            "Another_typedef_struct_t"}
                        (NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:9:9")))),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:42:31",
                  structFieldName = NamePair {
                    nameC = CName "field_5",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_5"},
                  structFieldType = TypePointer
                    (TypeTypedef
                      (TypedefSquashed
                        (CName
                          "another_typedef_struct_t")
                        (TypeStruct
                          NamePair {
                            nameC = CName
                              "another_typedef_struct_t",
                            nameHsIdent = HsIdentifier
                              "Another_typedef_struct_t"}
                          (NameOriginGenerated
                            (AnonId
                              "distilled_lib_1.h:9:9"))))),
                  structFieldOffset = 128,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:43:31",
                  structFieldName = NamePair {
                    nameC = CName "field_6",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_6"},
                  structFieldType = TypePointer
                    TypeVoid,
                  structFieldOffset = 192,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:44:31",
                  structFieldName = NamePair {
                    nameC = CName "field_7",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_7"},
                  structFieldType = TypeConstArray
                    7
                    (TypeExtBinding
                      ResolvedExtBinding {
                        extCName = QualName {
                          qualNameName = CName "uint32_t",
                          qualNameKind =
                          NameKindOrdinary},
                        extHsRef = ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word32"},
                        extHsSpec = TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word32"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]}}),
                  structFieldOffset = 256,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:45:31",
                  structFieldName = NamePair {
                    nameC = CName "field_8",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_8"},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (CName "another_typedef_enum_e")
                      (TypeEnum
                        NamePair {
                          nameC = CName
                            "another_typedef_enum_e",
                          nameHsIdent = HsIdentifier
                            "Another_typedef_enum_e"}
                        (NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:10:9")))),
                  structFieldOffset = 480,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:46:31",
                  structFieldName = NamePair {
                    nameC = CName "field_9",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_9"},
                  structFieldType = TypeConstArray
                    4
                    (TypeTypedef
                      (TypedefSquashed
                        (CName "another_typedef_enum_e")
                        (TypeEnum
                          NamePair {
                            nameC = CName
                              "another_typedef_enum_e",
                            nameHsIdent = HsIdentifier
                              "Another_typedef_enum_e"}
                          (NameOriginGenerated
                            (AnonId
                              "distilled_lib_1.h:10:9"))))),
                  structFieldOffset = 512,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "distilled_lib_1.h:47:31",
                  structFieldName = NamePair {
                    nameC = CName "field_10",
                    nameHsIdent = HsIdentifier
                      "a_typedef_struct_t_field_10"},
                  structFieldType = TypeConstArray
                    5
                    (TypeConstArray
                      3
                      (TypeTypedef
                        (TypedefSquashed
                          (CName "another_typedef_enum_e")
                          (TypeEnum
                            NamePair {
                              nameC = CName
                                "another_typedef_enum_e",
                              nameHsIdent = HsIdentifier
                                "Another_typedef_enum_e"}
                            (NameOriginGenerated
                              (AnonId
                                "distilled_lib_1.h:10:9")))))),
                  structFieldOffset = 640,
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
          "A_typedef_struct_t",
        structConstr = HsName
          "@NsConstr"
          "A_typedef_struct_t",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_0",
            fieldType = HsPrimType
              HsPrimCBool,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:37:31",
                structFieldName = NamePair {
                  nameC = CName "field_0",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_0"},
                structFieldType = TypePrim
                  PrimBool,
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_1",
            fieldType = HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Word8"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Word8"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]},
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:38:31",
                structFieldName = NamePair {
                  nameC = CName "field_1",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_1"},
                structFieldType = TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = CName "uint8_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Word8"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Word8"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}},
                structFieldOffset = 8,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_2",
            fieldType = HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Word16"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Word16"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]},
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:39:31",
                structFieldName = NamePair {
                  nameC = CName "field_2",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_2"},
                structFieldType = TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = CName "uint16_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Word16"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Word16"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}},
                structFieldOffset = 16,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_3",
            fieldType = HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Word32"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Word32"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]},
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:40:31",
                structFieldName = NamePair {
                  nameC = CName "field_3",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_3"},
                structFieldType = TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = CName "uint32_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Word32"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Word32"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}},
                structFieldOffset = 32,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_4",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Another_typedef_struct_t"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:41:31",
                structFieldName = NamePair {
                  nameC = CName "field_4",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_4"},
                structFieldType = TypeTypedef
                  (TypedefSquashed
                    (CName
                      "another_typedef_struct_t")
                    (TypeStruct
                      NamePair {
                        nameC = CName
                          "another_typedef_struct_t",
                        nameHsIdent = HsIdentifier
                          "Another_typedef_struct_t"}
                      (NameOriginGenerated
                        (AnonId
                          "distilled_lib_1.h:9:9")))),
                structFieldOffset = 64,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_5",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Another_typedef_struct_t")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:42:31",
                structFieldName = NamePair {
                  nameC = CName "field_5",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_5"},
                structFieldType = TypePointer
                  (TypeTypedef
                    (TypedefSquashed
                      (CName
                        "another_typedef_struct_t")
                      (TypeStruct
                        NamePair {
                          nameC = CName
                            "another_typedef_struct_t",
                          nameHsIdent = HsIdentifier
                            "Another_typedef_struct_t"}
                        (NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:9:9"))))),
                structFieldOffset = 128,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_6",
            fieldType = HsPtr
              (HsPrimType HsPrimVoid),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:43:31",
                structFieldName = NamePair {
                  nameC = CName "field_6",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_6"},
                structFieldType = TypePointer
                  TypeVoid,
                structFieldOffset = 192,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_7",
            fieldType = HsConstArray
              7
              (HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word32"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word32"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:44:31",
                structFieldName = NamePair {
                  nameC = CName "field_7",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_7"},
                structFieldType = TypeConstArray
                  7
                  (TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = CName "uint32_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word32"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word32"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}}),
                structFieldOffset = 256,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_8",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Another_typedef_enum_e"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:45:31",
                structFieldName = NamePair {
                  nameC = CName "field_8",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_8"},
                structFieldType = TypeTypedef
                  (TypedefSquashed
                    (CName "another_typedef_enum_e")
                    (TypeEnum
                      NamePair {
                        nameC = CName
                          "another_typedef_enum_e",
                        nameHsIdent = HsIdentifier
                          "Another_typedef_enum_e"}
                      (NameOriginGenerated
                        (AnonId
                          "distilled_lib_1.h:10:9")))),
                structFieldOffset = 480,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_9",
            fieldType = HsConstArray
              4
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Another_typedef_enum_e")),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:46:31",
                structFieldName = NamePair {
                  nameC = CName "field_9",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_9"},
                structFieldType = TypeConstArray
                  4
                  (TypeTypedef
                    (TypedefSquashed
                      (CName "another_typedef_enum_e")
                      (TypeEnum
                        NamePair {
                          nameC = CName
                            "another_typedef_enum_e",
                          nameHsIdent = HsIdentifier
                            "Another_typedef_enum_e"}
                        (NameOriginGenerated
                          (AnonId
                            "distilled_lib_1.h:10:9"))))),
                structFieldOffset = 512,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "a_typedef_struct_t_field_10",
            fieldType = HsConstArray
              5
              (HsConstArray
                3
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Another_typedef_enum_e"))),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "distilled_lib_1.h:47:31",
                structFieldName = NamePair {
                  nameC = CName "field_10",
                  nameHsIdent = HsIdentifier
                    "a_typedef_struct_t_field_10"},
                structFieldType = TypeConstArray
                  5
                  (TypeConstArray
                    3
                    (TypeTypedef
                      (TypedefSquashed
                        (CName "another_typedef_enum_e")
                        (TypeEnum
                          NamePair {
                            nameC = CName
                              "another_typedef_enum_e",
                            nameHsIdent = HsIdentifier
                              "Another_typedef_enum_e"}
                          (NameOriginGenerated
                            (AnonId
                              "distilled_lib_1.h:10:9")))))),
                structFieldOffset = 640,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "distilled_lib_1.h:35:16",
              declId = NamePair {
                nameC = CName
                  "a_typedef_struct_t",
                nameHsIdent = HsIdentifier
                  "A_typedef_struct_t"},
              declOrigin =
              NameOriginRenamedFrom
                (CName "a_typedef_struct"),
              declAliases = [
                CName "a_typedef_struct_t"],
              declHeader =
              "distilled_lib_1.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "A_typedef_struct_t"),
                structSizeof = 140,
                structAlignment = 1,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:37:31",
                    structFieldName = NamePair {
                      nameC = CName "field_0",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_0"},
                    structFieldType = TypePrim
                      PrimBool,
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:38:31",
                    structFieldName = NamePair {
                      nameC = CName "field_1",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_1"},
                    structFieldType = TypeExtBinding
                      ResolvedExtBinding {
                        extCName = QualName {
                          qualNameName = CName "uint8_t",
                          qualNameKind =
                          NameKindOrdinary},
                        extHsRef = ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word8"},
                        extHsSpec = TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word8"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]}},
                    structFieldOffset = 8,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:39:31",
                    structFieldName = NamePair {
                      nameC = CName "field_2",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_2"},
                    structFieldType = TypeExtBinding
                      ResolvedExtBinding {
                        extCName = QualName {
                          qualNameName = CName "uint16_t",
                          qualNameKind =
                          NameKindOrdinary},
                        extHsRef = ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word16"},
                        extHsSpec = TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word16"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]}},
                    structFieldOffset = 16,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:40:31",
                    structFieldName = NamePair {
                      nameC = CName "field_3",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_3"},
                    structFieldType = TypeExtBinding
                      ResolvedExtBinding {
                        extCName = QualName {
                          qualNameName = CName "uint32_t",
                          qualNameKind =
                          NameKindOrdinary},
                        extHsRef = ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word32"},
                        extHsSpec = TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word32"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]}},
                    structFieldOffset = 32,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:41:31",
                    structFieldName = NamePair {
                      nameC = CName "field_4",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_4"},
                    structFieldType = TypeTypedef
                      (TypedefSquashed
                        (CName
                          "another_typedef_struct_t")
                        (TypeStruct
                          NamePair {
                            nameC = CName
                              "another_typedef_struct_t",
                            nameHsIdent = HsIdentifier
                              "Another_typedef_struct_t"}
                          (NameOriginGenerated
                            (AnonId
                              "distilled_lib_1.h:9:9")))),
                    structFieldOffset = 64,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:42:31",
                    structFieldName = NamePair {
                      nameC = CName "field_5",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_5"},
                    structFieldType = TypePointer
                      (TypeTypedef
                        (TypedefSquashed
                          (CName
                            "another_typedef_struct_t")
                          (TypeStruct
                            NamePair {
                              nameC = CName
                                "another_typedef_struct_t",
                              nameHsIdent = HsIdentifier
                                "Another_typedef_struct_t"}
                            (NameOriginGenerated
                              (AnonId
                                "distilled_lib_1.h:9:9"))))),
                    structFieldOffset = 128,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:43:31",
                    structFieldName = NamePair {
                      nameC = CName "field_6",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_6"},
                    structFieldType = TypePointer
                      TypeVoid,
                    structFieldOffset = 192,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:44:31",
                    structFieldName = NamePair {
                      nameC = CName "field_7",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_7"},
                    structFieldType = TypeConstArray
                      7
                      (TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = CName "uint32_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "Word32"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "Word32"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}}),
                    structFieldOffset = 256,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:45:31",
                    structFieldName = NamePair {
                      nameC = CName "field_8",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_8"},
                    structFieldType = TypeTypedef
                      (TypedefSquashed
                        (CName "another_typedef_enum_e")
                        (TypeEnum
                          NamePair {
                            nameC = CName
                              "another_typedef_enum_e",
                            nameHsIdent = HsIdentifier
                              "Another_typedef_enum_e"}
                          (NameOriginGenerated
                            (AnonId
                              "distilled_lib_1.h:10:9")))),
                    structFieldOffset = 480,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:46:31",
                    structFieldName = NamePair {
                      nameC = CName "field_9",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_9"},
                    structFieldType = TypeConstArray
                      4
                      (TypeTypedef
                        (TypedefSquashed
                          (CName "another_typedef_enum_e")
                          (TypeEnum
                            NamePair {
                              nameC = CName
                                "another_typedef_enum_e",
                              nameHsIdent = HsIdentifier
                                "Another_typedef_enum_e"}
                            (NameOriginGenerated
                              (AnonId
                                "distilled_lib_1.h:10:9"))))),
                    structFieldOffset = 512,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "distilled_lib_1.h:47:31",
                    structFieldName = NamePair {
                      nameC = CName "field_10",
                      nameHsIdent = HsIdentifier
                        "a_typedef_struct_t_field_10"},
                    structFieldType = TypeConstArray
                      5
                      (TypeConstArray
                        3
                        (TypeTypedef
                          (TypedefSquashed
                            (CName "another_typedef_enum_e")
                            (TypeEnum
                              NamePair {
                                nameC = CName
                                  "another_typedef_enum_e",
                                nameHsIdent = HsIdentifier
                                  "Another_typedef_enum_e"}
                              (NameOriginGenerated
                                (AnonId
                                  "distilled_lib_1.h:10:9")))))),
                    structFieldOffset = 640,
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
        storableSizeOf = 140,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "A_typedef_struct_t",
                structConstr = HsName
                  "@NsConstr"
                  "A_typedef_struct_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_0",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:37:31",
                        structFieldName = NamePair {
                          nameC = CName "field_0",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_0"},
                        structFieldType = TypePrim
                          PrimBool,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_1",
                    fieldType = HsExtBinding
                      ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word8"}
                      TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word8"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]},
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:38:31",
                        structFieldName = NamePair {
                          nameC = CName "field_1",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_1"},
                        structFieldType = TypeExtBinding
                          ResolvedExtBinding {
                            extCName = QualName {
                              qualNameName = CName "uint8_t",
                              qualNameKind =
                              NameKindOrdinary},
                            extHsRef = ExtHsRef {
                              extHsRefModule = HsModuleName
                                "HsBindgen.Runtime.Prelude",
                              extHsRefIdentifier =
                              HsIdentifier "Word8"},
                            extHsSpec = TypeSpec {
                              typeSpecModule = Just
                                (HsModuleName
                                  "HsBindgen.Runtime.Prelude"),
                              typeSpecIdentifier = Just
                                (HsIdentifier "Word8"),
                              typeSpecInstances = Map.fromList
                                [
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]}},
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_2",
                    fieldType = HsExtBinding
                      ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"}
                      TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]},
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:39:31",
                        structFieldName = NamePair {
                          nameC = CName "field_2",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_2"},
                        structFieldType = TypeExtBinding
                          ResolvedExtBinding {
                            extCName = QualName {
                              qualNameName = CName "uint16_t",
                              qualNameKind =
                              NameKindOrdinary},
                            extHsRef = ExtHsRef {
                              extHsRefModule = HsModuleName
                                "HsBindgen.Runtime.Prelude",
                              extHsRefIdentifier =
                              HsIdentifier "Word16"},
                            extHsSpec = TypeSpec {
                              typeSpecModule = Just
                                (HsModuleName
                                  "HsBindgen.Runtime.Prelude"),
                              typeSpecIdentifier = Just
                                (HsIdentifier "Word16"),
                              typeSpecInstances = Map.fromList
                                [
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]}},
                        structFieldOffset = 16,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_3",
                    fieldType = HsExtBinding
                      ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word32"}
                      TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word32"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]},
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:40:31",
                        structFieldName = NamePair {
                          nameC = CName "field_3",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_3"},
                        structFieldType = TypeExtBinding
                          ResolvedExtBinding {
                            extCName = QualName {
                              qualNameName = CName "uint32_t",
                              qualNameKind =
                              NameKindOrdinary},
                            extHsRef = ExtHsRef {
                              extHsRefModule = HsModuleName
                                "HsBindgen.Runtime.Prelude",
                              extHsRefIdentifier =
                              HsIdentifier "Word32"},
                            extHsSpec = TypeSpec {
                              typeSpecModule = Just
                                (HsModuleName
                                  "HsBindgen.Runtime.Prelude"),
                              typeSpecIdentifier = Just
                                (HsIdentifier "Word32"),
                              typeSpecInstances = Map.fromList
                                [
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]}},
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_4",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_struct_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:41:31",
                        structFieldName = NamePair {
                          nameC = CName "field_4",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_4"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (CName
                              "another_typedef_struct_t")
                            (TypeStruct
                              NamePair {
                                nameC = CName
                                  "another_typedef_struct_t",
                                nameHsIdent = HsIdentifier
                                  "Another_typedef_struct_t"}
                              (NameOriginGenerated
                                (AnonId
                                  "distilled_lib_1.h:9:9")))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_5",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Another_typedef_struct_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:42:31",
                        structFieldName = NamePair {
                          nameC = CName "field_5",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_5"},
                        structFieldType = TypePointer
                          (TypeTypedef
                            (TypedefSquashed
                              (CName
                                "another_typedef_struct_t")
                              (TypeStruct
                                NamePair {
                                  nameC = CName
                                    "another_typedef_struct_t",
                                  nameHsIdent = HsIdentifier
                                    "Another_typedef_struct_t"}
                                (NameOriginGenerated
                                  (AnonId
                                    "distilled_lib_1.h:9:9"))))),
                        structFieldOffset = 128,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_6",
                    fieldType = HsPtr
                      (HsPrimType HsPrimVoid),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:43:31",
                        structFieldName = NamePair {
                          nameC = CName "field_6",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_6"},
                        structFieldType = TypePointer
                          TypeVoid,
                        structFieldOffset = 192,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_7",
                    fieldType = HsConstArray
                      7
                      (HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word32"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word32"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]}),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:44:31",
                        structFieldName = NamePair {
                          nameC = CName "field_7",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_7"},
                        structFieldType = TypeConstArray
                          7
                          (TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = CName "uint32_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word32"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word32"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_8",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_enum_e"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:45:31",
                        structFieldName = NamePair {
                          nameC = CName "field_8",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_8"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (CName "another_typedef_enum_e")
                            (TypeEnum
                              NamePair {
                                nameC = CName
                                  "another_typedef_enum_e",
                                nameHsIdent = HsIdentifier
                                  "Another_typedef_enum_e"}
                              (NameOriginGenerated
                                (AnonId
                                  "distilled_lib_1.h:10:9")))),
                        structFieldOffset = 480,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_9",
                    fieldType = HsConstArray
                      4
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Another_typedef_enum_e")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:46:31",
                        structFieldName = NamePair {
                          nameC = CName "field_9",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_9"},
                        structFieldType = TypeConstArray
                          4
                          (TypeTypedef
                            (TypedefSquashed
                              (CName "another_typedef_enum_e")
                              (TypeEnum
                                NamePair {
                                  nameC = CName
                                    "another_typedef_enum_e",
                                  nameHsIdent = HsIdentifier
                                    "Another_typedef_enum_e"}
                                (NameOriginGenerated
                                  (AnonId
                                    "distilled_lib_1.h:10:9"))))),
                        structFieldOffset = 512,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_10",
                    fieldType = HsConstArray
                      5
                      (HsConstArray
                        3
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "Another_typedef_enum_e"))),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:47:31",
                        structFieldName = NamePair {
                          nameC = CName "field_10",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_10"},
                        structFieldType = TypeConstArray
                          5
                          (TypeConstArray
                            3
                            (TypeTypedef
                              (TypedefSquashed
                                (CName "another_typedef_enum_e")
                                (TypeEnum
                                  NamePair {
                                    nameC = CName
                                      "another_typedef_enum_e",
                                    nameHsIdent = HsIdentifier
                                      "Another_typedef_enum_e"}
                                  (NameOriginGenerated
                                    (AnonId
                                      "distilled_lib_1.h:10:9")))))),
                        structFieldOffset = 640,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "distilled_lib_1.h:35:16",
                      declId = NamePair {
                        nameC = CName
                          "a_typedef_struct_t",
                        nameHsIdent = HsIdentifier
                          "A_typedef_struct_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (CName "a_typedef_struct"),
                      declAliases = [
                        CName "a_typedef_struct_t"],
                      declHeader =
                      "distilled_lib_1.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "A_typedef_struct_t"),
                        structSizeof = 140,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:37:31",
                            structFieldName = NamePair {
                              nameC = CName "field_0",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_0"},
                            structFieldType = TypePrim
                              PrimBool,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:38:31",
                            structFieldName = NamePair {
                              nameC = CName "field_1",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_1"},
                            structFieldType = TypeExtBinding
                              ResolvedExtBinding {
                                extCName = QualName {
                                  qualNameName = CName "uint8_t",
                                  qualNameKind =
                                  NameKindOrdinary},
                                extHsRef = ExtHsRef {
                                  extHsRefModule = HsModuleName
                                    "HsBindgen.Runtime.Prelude",
                                  extHsRefIdentifier =
                                  HsIdentifier "Word8"},
                                extHsSpec = TypeSpec {
                                  typeSpecModule = Just
                                    (HsModuleName
                                      "HsBindgen.Runtime.Prelude"),
                                  typeSpecIdentifier = Just
                                    (HsIdentifier "Word8"),
                                  typeSpecInstances = Map.fromList
                                    [
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]}},
                            structFieldOffset = 8,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:39:31",
                            structFieldName = NamePair {
                              nameC = CName "field_2",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_2"},
                            structFieldType = TypeExtBinding
                              ResolvedExtBinding {
                                extCName = QualName {
                                  qualNameName = CName "uint16_t",
                                  qualNameKind =
                                  NameKindOrdinary},
                                extHsRef = ExtHsRef {
                                  extHsRefModule = HsModuleName
                                    "HsBindgen.Runtime.Prelude",
                                  extHsRefIdentifier =
                                  HsIdentifier "Word16"},
                                extHsSpec = TypeSpec {
                                  typeSpecModule = Just
                                    (HsModuleName
                                      "HsBindgen.Runtime.Prelude"),
                                  typeSpecIdentifier = Just
                                    (HsIdentifier "Word16"),
                                  typeSpecInstances = Map.fromList
                                    [
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]}},
                            structFieldOffset = 16,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:40:31",
                            structFieldName = NamePair {
                              nameC = CName "field_3",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_3"},
                            structFieldType = TypeExtBinding
                              ResolvedExtBinding {
                                extCName = QualName {
                                  qualNameName = CName "uint32_t",
                                  qualNameKind =
                                  NameKindOrdinary},
                                extHsRef = ExtHsRef {
                                  extHsRefModule = HsModuleName
                                    "HsBindgen.Runtime.Prelude",
                                  extHsRefIdentifier =
                                  HsIdentifier "Word32"},
                                extHsSpec = TypeSpec {
                                  typeSpecModule = Just
                                    (HsModuleName
                                      "HsBindgen.Runtime.Prelude"),
                                  typeSpecIdentifier = Just
                                    (HsIdentifier "Word32"),
                                  typeSpecInstances = Map.fromList
                                    [
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]}},
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:41:31",
                            structFieldName = NamePair {
                              nameC = CName "field_4",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_4"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (CName
                                  "another_typedef_struct_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = CName
                                      "another_typedef_struct_t",
                                    nameHsIdent = HsIdentifier
                                      "Another_typedef_struct_t"}
                                  (NameOriginGenerated
                                    (AnonId
                                      "distilled_lib_1.h:9:9")))),
                            structFieldOffset = 64,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:42:31",
                            structFieldName = NamePair {
                              nameC = CName "field_5",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_5"},
                            structFieldType = TypePointer
                              (TypeTypedef
                                (TypedefSquashed
                                  (CName
                                    "another_typedef_struct_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = CName
                                        "another_typedef_struct_t",
                                      nameHsIdent = HsIdentifier
                                        "Another_typedef_struct_t"}
                                    (NameOriginGenerated
                                      (AnonId
                                        "distilled_lib_1.h:9:9"))))),
                            structFieldOffset = 128,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:43:31",
                            structFieldName = NamePair {
                              nameC = CName "field_6",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_6"},
                            structFieldType = TypePointer
                              TypeVoid,
                            structFieldOffset = 192,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:44:31",
                            structFieldName = NamePair {
                              nameC = CName "field_7",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_7"},
                            structFieldType = TypeConstArray
                              7
                              (TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = CName "uint32_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word32"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word32"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:45:31",
                            structFieldName = NamePair {
                              nameC = CName "field_8",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_8"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (CName "another_typedef_enum_e")
                                (TypeEnum
                                  NamePair {
                                    nameC = CName
                                      "another_typedef_enum_e",
                                    nameHsIdent = HsIdentifier
                                      "Another_typedef_enum_e"}
                                  (NameOriginGenerated
                                    (AnonId
                                      "distilled_lib_1.h:10:9")))),
                            structFieldOffset = 480,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:46:31",
                            structFieldName = NamePair {
                              nameC = CName "field_9",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_9"},
                            structFieldType = TypeConstArray
                              4
                              (TypeTypedef
                                (TypedefSquashed
                                  (CName "another_typedef_enum_e")
                                  (TypeEnum
                                    NamePair {
                                      nameC = CName
                                        "another_typedef_enum_e",
                                      nameHsIdent = HsIdentifier
                                        "Another_typedef_enum_e"}
                                    (NameOriginGenerated
                                      (AnonId
                                        "distilled_lib_1.h:10:9"))))),
                            structFieldOffset = 512,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:47:31",
                            structFieldName = NamePair {
                              nameC = CName "field_10",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_10"},
                            structFieldType = TypeConstArray
                              5
                              (TypeConstArray
                                3
                                (TypeTypedef
                                  (TypedefSquashed
                                    (CName "another_typedef_enum_e")
                                    (TypeEnum
                                      NamePair {
                                        nameC = CName
                                          "another_typedef_enum_e",
                                        nameHsIdent = HsIdentifier
                                          "Another_typedef_enum_e"}
                                      (NameOriginGenerated
                                        (AnonId
                                          "distilled_lib_1.h:10:9")))))),
                            structFieldOffset = 640,
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
              PeekByteOff (Idx 0) 8,
              PeekByteOff (Idx 0) 16,
              PeekByteOff (Idx 0) 24,
              PeekByteOff (Idx 0) 32,
              PeekByteOff (Idx 0) 60,
              PeekByteOff (Idx 0) 64,
              PeekByteOff (Idx 0) 80]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "A_typedef_struct_t",
                structConstr = HsName
                  "@NsConstr"
                  "A_typedef_struct_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_0",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:37:31",
                        structFieldName = NamePair {
                          nameC = CName "field_0",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_0"},
                        structFieldType = TypePrim
                          PrimBool,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_1",
                    fieldType = HsExtBinding
                      ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word8"}
                      TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word8"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]},
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:38:31",
                        structFieldName = NamePair {
                          nameC = CName "field_1",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_1"},
                        structFieldType = TypeExtBinding
                          ResolvedExtBinding {
                            extCName = QualName {
                              qualNameName = CName "uint8_t",
                              qualNameKind =
                              NameKindOrdinary},
                            extHsRef = ExtHsRef {
                              extHsRefModule = HsModuleName
                                "HsBindgen.Runtime.Prelude",
                              extHsRefIdentifier =
                              HsIdentifier "Word8"},
                            extHsSpec = TypeSpec {
                              typeSpecModule = Just
                                (HsModuleName
                                  "HsBindgen.Runtime.Prelude"),
                              typeSpecIdentifier = Just
                                (HsIdentifier "Word8"),
                              typeSpecInstances = Map.fromList
                                [
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]}},
                        structFieldOffset = 8,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_2",
                    fieldType = HsExtBinding
                      ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"}
                      TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]},
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:39:31",
                        structFieldName = NamePair {
                          nameC = CName "field_2",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_2"},
                        structFieldType = TypeExtBinding
                          ResolvedExtBinding {
                            extCName = QualName {
                              qualNameName = CName "uint16_t",
                              qualNameKind =
                              NameKindOrdinary},
                            extHsRef = ExtHsRef {
                              extHsRefModule = HsModuleName
                                "HsBindgen.Runtime.Prelude",
                              extHsRefIdentifier =
                              HsIdentifier "Word16"},
                            extHsSpec = TypeSpec {
                              typeSpecModule = Just
                                (HsModuleName
                                  "HsBindgen.Runtime.Prelude"),
                              typeSpecIdentifier = Just
                                (HsIdentifier "Word16"),
                              typeSpecInstances = Map.fromList
                                [
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]}},
                        structFieldOffset = 16,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_3",
                    fieldType = HsExtBinding
                      ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word32"}
                      TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word32"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]},
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:40:31",
                        structFieldName = NamePair {
                          nameC = CName "field_3",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_3"},
                        structFieldType = TypeExtBinding
                          ResolvedExtBinding {
                            extCName = QualName {
                              qualNameName = CName "uint32_t",
                              qualNameKind =
                              NameKindOrdinary},
                            extHsRef = ExtHsRef {
                              extHsRefModule = HsModuleName
                                "HsBindgen.Runtime.Prelude",
                              extHsRefIdentifier =
                              HsIdentifier "Word32"},
                            extHsSpec = TypeSpec {
                              typeSpecModule = Just
                                (HsModuleName
                                  "HsBindgen.Runtime.Prelude"),
                              typeSpecIdentifier = Just
                                (HsIdentifier "Word32"),
                              typeSpecInstances = Map.fromList
                                [
                                  _×_
                                    Eq
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ord
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Enum
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Ix
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bounded
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Read
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Show
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Bits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    FiniteBits
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Integral
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Num
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Real
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    StaticSize
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    ReadRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    WriteRaw
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = []}),
                                  _×_
                                    Storable
                                    (Require
                                      InstanceSpec {
                                        instanceSpecStrategy = Nothing,
                                        instanceSpecConstraints = [
                                          ]})]}},
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_4",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_struct_t"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:41:31",
                        structFieldName = NamePair {
                          nameC = CName "field_4",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_4"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (CName
                              "another_typedef_struct_t")
                            (TypeStruct
                              NamePair {
                                nameC = CName
                                  "another_typedef_struct_t",
                                nameHsIdent = HsIdentifier
                                  "Another_typedef_struct_t"}
                              (NameOriginGenerated
                                (AnonId
                                  "distilled_lib_1.h:9:9")))),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_5",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Another_typedef_struct_t")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:42:31",
                        structFieldName = NamePair {
                          nameC = CName "field_5",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_5"},
                        structFieldType = TypePointer
                          (TypeTypedef
                            (TypedefSquashed
                              (CName
                                "another_typedef_struct_t")
                              (TypeStruct
                                NamePair {
                                  nameC = CName
                                    "another_typedef_struct_t",
                                  nameHsIdent = HsIdentifier
                                    "Another_typedef_struct_t"}
                                (NameOriginGenerated
                                  (AnonId
                                    "distilled_lib_1.h:9:9"))))),
                        structFieldOffset = 128,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_6",
                    fieldType = HsPtr
                      (HsPrimType HsPrimVoid),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:43:31",
                        structFieldName = NamePair {
                          nameC = CName "field_6",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_6"},
                        structFieldType = TypePointer
                          TypeVoid,
                        structFieldOffset = 192,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_7",
                    fieldType = HsConstArray
                      7
                      (HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word32"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word32"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]}),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:44:31",
                        structFieldName = NamePair {
                          nameC = CName "field_7",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_7"},
                        structFieldType = TypeConstArray
                          7
                          (TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = CName "uint32_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word32"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word32"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}}),
                        structFieldOffset = 256,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_8",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Another_typedef_enum_e"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:45:31",
                        structFieldName = NamePair {
                          nameC = CName "field_8",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_8"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (CName "another_typedef_enum_e")
                            (TypeEnum
                              NamePair {
                                nameC = CName
                                  "another_typedef_enum_e",
                                nameHsIdent = HsIdentifier
                                  "Another_typedef_enum_e"}
                              (NameOriginGenerated
                                (AnonId
                                  "distilled_lib_1.h:10:9")))),
                        structFieldOffset = 480,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_9",
                    fieldType = HsConstArray
                      4
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Another_typedef_enum_e")),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:46:31",
                        structFieldName = NamePair {
                          nameC = CName "field_9",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_9"},
                        structFieldType = TypeConstArray
                          4
                          (TypeTypedef
                            (TypedefSquashed
                              (CName "another_typedef_enum_e")
                              (TypeEnum
                                NamePair {
                                  nameC = CName
                                    "another_typedef_enum_e",
                                  nameHsIdent = HsIdentifier
                                    "Another_typedef_enum_e"}
                                (NameOriginGenerated
                                  (AnonId
                                    "distilled_lib_1.h:10:9"))))),
                        structFieldOffset = 512,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "a_typedef_struct_t_field_10",
                    fieldType = HsConstArray
                      5
                      (HsConstArray
                        3
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "Another_typedef_enum_e"))),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "distilled_lib_1.h:47:31",
                        structFieldName = NamePair {
                          nameC = CName "field_10",
                          nameHsIdent = HsIdentifier
                            "a_typedef_struct_t_field_10"},
                        structFieldType = TypeConstArray
                          5
                          (TypeConstArray
                            3
                            (TypeTypedef
                              (TypedefSquashed
                                (CName "another_typedef_enum_e")
                                (TypeEnum
                                  NamePair {
                                    nameC = CName
                                      "another_typedef_enum_e",
                                    nameHsIdent = HsIdentifier
                                      "Another_typedef_enum_e"}
                                  (NameOriginGenerated
                                    (AnonId
                                      "distilled_lib_1.h:10:9")))))),
                        structFieldOffset = 640,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "distilled_lib_1.h:35:16",
                      declId = NamePair {
                        nameC = CName
                          "a_typedef_struct_t",
                        nameHsIdent = HsIdentifier
                          "A_typedef_struct_t"},
                      declOrigin =
                      NameOriginRenamedFrom
                        (CName "a_typedef_struct"),
                      declAliases = [
                        CName "a_typedef_struct_t"],
                      declHeader =
                      "distilled_lib_1.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "A_typedef_struct_t"),
                        structSizeof = 140,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:37:31",
                            structFieldName = NamePair {
                              nameC = CName "field_0",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_0"},
                            structFieldType = TypePrim
                              PrimBool,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:38:31",
                            structFieldName = NamePair {
                              nameC = CName "field_1",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_1"},
                            structFieldType = TypeExtBinding
                              ResolvedExtBinding {
                                extCName = QualName {
                                  qualNameName = CName "uint8_t",
                                  qualNameKind =
                                  NameKindOrdinary},
                                extHsRef = ExtHsRef {
                                  extHsRefModule = HsModuleName
                                    "HsBindgen.Runtime.Prelude",
                                  extHsRefIdentifier =
                                  HsIdentifier "Word8"},
                                extHsSpec = TypeSpec {
                                  typeSpecModule = Just
                                    (HsModuleName
                                      "HsBindgen.Runtime.Prelude"),
                                  typeSpecIdentifier = Just
                                    (HsIdentifier "Word8"),
                                  typeSpecInstances = Map.fromList
                                    [
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]}},
                            structFieldOffset = 8,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:39:31",
                            structFieldName = NamePair {
                              nameC = CName "field_2",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_2"},
                            structFieldType = TypeExtBinding
                              ResolvedExtBinding {
                                extCName = QualName {
                                  qualNameName = CName "uint16_t",
                                  qualNameKind =
                                  NameKindOrdinary},
                                extHsRef = ExtHsRef {
                                  extHsRefModule = HsModuleName
                                    "HsBindgen.Runtime.Prelude",
                                  extHsRefIdentifier =
                                  HsIdentifier "Word16"},
                                extHsSpec = TypeSpec {
                                  typeSpecModule = Just
                                    (HsModuleName
                                      "HsBindgen.Runtime.Prelude"),
                                  typeSpecIdentifier = Just
                                    (HsIdentifier "Word16"),
                                  typeSpecInstances = Map.fromList
                                    [
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]}},
                            structFieldOffset = 16,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:40:31",
                            structFieldName = NamePair {
                              nameC = CName "field_3",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_3"},
                            structFieldType = TypeExtBinding
                              ResolvedExtBinding {
                                extCName = QualName {
                                  qualNameName = CName "uint32_t",
                                  qualNameKind =
                                  NameKindOrdinary},
                                extHsRef = ExtHsRef {
                                  extHsRefModule = HsModuleName
                                    "HsBindgen.Runtime.Prelude",
                                  extHsRefIdentifier =
                                  HsIdentifier "Word32"},
                                extHsSpec = TypeSpec {
                                  typeSpecModule = Just
                                    (HsModuleName
                                      "HsBindgen.Runtime.Prelude"),
                                  typeSpecIdentifier = Just
                                    (HsIdentifier "Word32"),
                                  typeSpecInstances = Map.fromList
                                    [
                                      _×_
                                        Eq
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ord
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Enum
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Ix
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bounded
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Read
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Show
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Bits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        FiniteBits
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Integral
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Num
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Real
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        StaticSize
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        ReadRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        WriteRaw
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = []}),
                                      _×_
                                        Storable
                                        (Require
                                          InstanceSpec {
                                            instanceSpecStrategy = Nothing,
                                            instanceSpecConstraints = [
                                              ]})]}},
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:41:31",
                            structFieldName = NamePair {
                              nameC = CName "field_4",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_4"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (CName
                                  "another_typedef_struct_t")
                                (TypeStruct
                                  NamePair {
                                    nameC = CName
                                      "another_typedef_struct_t",
                                    nameHsIdent = HsIdentifier
                                      "Another_typedef_struct_t"}
                                  (NameOriginGenerated
                                    (AnonId
                                      "distilled_lib_1.h:9:9")))),
                            structFieldOffset = 64,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:42:31",
                            structFieldName = NamePair {
                              nameC = CName "field_5",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_5"},
                            structFieldType = TypePointer
                              (TypeTypedef
                                (TypedefSquashed
                                  (CName
                                    "another_typedef_struct_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = CName
                                        "another_typedef_struct_t",
                                      nameHsIdent = HsIdentifier
                                        "Another_typedef_struct_t"}
                                    (NameOriginGenerated
                                      (AnonId
                                        "distilled_lib_1.h:9:9"))))),
                            structFieldOffset = 128,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:43:31",
                            structFieldName = NamePair {
                              nameC = CName "field_6",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_6"},
                            structFieldType = TypePointer
                              TypeVoid,
                            structFieldOffset = 192,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:44:31",
                            structFieldName = NamePair {
                              nameC = CName "field_7",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_7"},
                            structFieldType = TypeConstArray
                              7
                              (TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = CName "uint32_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word32"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word32"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}}),
                            structFieldOffset = 256,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:45:31",
                            structFieldName = NamePair {
                              nameC = CName "field_8",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_8"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (CName "another_typedef_enum_e")
                                (TypeEnum
                                  NamePair {
                                    nameC = CName
                                      "another_typedef_enum_e",
                                    nameHsIdent = HsIdentifier
                                      "Another_typedef_enum_e"}
                                  (NameOriginGenerated
                                    (AnonId
                                      "distilled_lib_1.h:10:9")))),
                            structFieldOffset = 480,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:46:31",
                            structFieldName = NamePair {
                              nameC = CName "field_9",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_9"},
                            structFieldType = TypeConstArray
                              4
                              (TypeTypedef
                                (TypedefSquashed
                                  (CName "another_typedef_enum_e")
                                  (TypeEnum
                                    NamePair {
                                      nameC = CName
                                        "another_typedef_enum_e",
                                      nameHsIdent = HsIdentifier
                                        "Another_typedef_enum_e"}
                                    (NameOriginGenerated
                                      (AnonId
                                        "distilled_lib_1.h:10:9"))))),
                            structFieldOffset = 512,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "distilled_lib_1.h:47:31",
                            structFieldName = NamePair {
                              nameC = CName "field_10",
                              nameHsIdent = HsIdentifier
                                "a_typedef_struct_t_field_10"},
                            structFieldType = TypeConstArray
                              5
                              (TypeConstArray
                                3
                                (TypeTypedef
                                  (TypedefSquashed
                                    (CName "another_typedef_enum_e")
                                    (TypeEnum
                                      NamePair {
                                        nameC = CName
                                          "another_typedef_enum_e",
                                        nameHsIdent = HsIdentifier
                                          "Another_typedef_enum_e"}
                                      (NameOriginGenerated
                                        (AnonId
                                          "distilled_lib_1.h:10:9")))))),
                            structFieldOffset = 640,
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
              (Add 11)
              (Seq
                [
                  PokeByteOff (Idx 12) 0 (Idx 0),
                  PokeByteOff (Idx 12) 1 (Idx 1),
                  PokeByteOff (Idx 12) 2 (Idx 2),
                  PokeByteOff (Idx 12) 4 (Idx 3),
                  PokeByteOff (Idx 12) 8 (Idx 4),
                  PokeByteOff (Idx 12) 16 (Idx 5),
                  PokeByteOff (Idx 12) 24 (Idx 6),
                  PokeByteOff (Idx 12) 32 (Idx 7),
                  PokeByteOff (Idx 12) 60 (Idx 8),
                  PokeByteOff (Idx 12) 64 (Idx 9),
                  PokeByteOff
                    (Idx 12)
                    80
                    (Idx 10)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "A_typedef_struct_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "A_typedef_struct_t"),
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a_DEFINE_0",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        0
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a_DEFINE_1",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Unsigned)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        20560
        HsPrimCUInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "a_DEFINE_2",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon IntLikeTyCon)))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon
                      (IntLikeTyCon
                        (CIntegralType
                          (IntLike (Int Signed)))))))
                []]}},
      varDeclBody = VarDeclIntegral
        2
        HsPrimCInt},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "tWO_ARGS",
      varDeclType = ForallTy {
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = TyConAppTy
            (ATyCon
              (GenerativeTyCon
                (DataTyCon (TupleTyCon 2))))
            [
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon IntLikeTyCon)))
                [
                  TyConAppTy
                    (ATyCon
                      (GenerativeTyCon
                        (DataTyCon
                          (IntLikeTyCon
                            (CIntegralType
                              (IntLike (Int Signed)))))))
                    []],
              TyConAppTy
                (ATyCon
                  (GenerativeTyCon
                    (DataTyCon IntLikeTyCon)))
                [
                  TyConAppTy
                    (ATyCon
                      (GenerativeTyCon
                        (DataTyCon
                          (IntLikeTyCon
                            (CIntegralType
                              (IntLike (Int Signed)))))))
                    []]]}},
      varDeclBody = VarDeclApp
        (InfixAppHead MTuple)
        [
          VarDeclIntegral
            13398
            HsPrimCInt,
          VarDeclIntegral
            30874
            HsPrimCInt]},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "A_typedef_enum_e",
      newtypeConstr = HsName
        "@NsConstr"
        "A_typedef_enum_e",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_A_typedef_enum_e",
        fieldType = HsPrimType
          HsPrimCUChar,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:61:9",
          declId = NamePair {
            nameC = CName
              "a_typedef_enum_e",
            nameHsIdent = HsIdentifier
              "A_typedef_enum_e"},
          declOrigin = NameOriginGenerated
            (AnonId
              "distilled_lib_1.h:61:9"),
          declAliases = [
            CName "a_typedef_enum_e"],
          declHeader =
          "distilled_lib_1.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "A_typedef_enum_e",
              newtypeField = HsName
                "@NsVar"
                "un_A_typedef_enum_e"},
            enumType = TypePrim
              (PrimChar
                (PrimSignExplicit Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "distilled_lib_1.h:63:3",
                enumConstantName = NamePair {
                  nameC = CName "ENUM_CASE_0",
                  nameHsIdent = HsIdentifier
                    "ENUM_CASE_0"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "distilled_lib_1.h:64:3",
                enumConstantName = NamePair {
                  nameC = CName "ENUM_CASE_1",
                  nameHsIdent = HsIdentifier
                    "ENUM_CASE_1"},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantLoc =
                "distilled_lib_1.h:65:3",
                enumConstantName = NamePair {
                  nameC = CName "ENUM_CASE_2",
                  nameHsIdent = HsIdentifier
                    "ENUM_CASE_2"},
                enumConstantValue = 2},
              EnumConstant {
                enumConstantLoc =
                "distilled_lib_1.h:66:3",
                enumConstantName = NamePair {
                  nameC = CName "ENUM_CASE_3",
                  nameHsIdent = HsIdentifier
                    "ENUM_CASE_3"},
                enumConstantValue = 3}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "A_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "A_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_A_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      StorableInstance {
        storableSizeOf = 1,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "A_typedef_enum_e",
                structConstr = HsName
                  "@NsConstr"
                  "A_typedef_enum_e",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_A_typedef_enum_e",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "A_typedef_enum_e",
                structConstr = HsName
                  "@NsConstr"
                  "A_typedef_enum_e",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_A_typedef_enum_e",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "A_typedef_enum_e"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "A_typedef_enum_e"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "A_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "A_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_A_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUChar)
      (Map.fromList
        [
          _×_
            0
            (NE.fromList ["ENUM_CASE_0"]),
          _×_
            1
            (NE.fromList ["ENUM_CASE_1"]),
          _×_
            2
            (NE.fromList ["ENUM_CASE_2"]),
          _×_
            3
            (NE.fromList ["ENUM_CASE_3"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "A_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "A_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_A_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName
        "@NsConstr"
        "ENUM_CASE_0")
      (HsName
        "@NsConstr"
        "ENUM_CASE_3")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "A_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "A_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_A_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "A_typedef_enum_e",
        structConstr = HsName
          "@NsConstr"
          "A_typedef_enum_e",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_A_typedef_enum_e",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "ENUM_CASE_0",
      patSynType = HsName
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "distilled_lib_1.h:63:3",
          enumConstantName = NamePair {
            nameC = CName "ENUM_CASE_0",
            nameHsIdent = HsIdentifier
              "ENUM_CASE_0"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "ENUM_CASE_1",
      patSynType = HsName
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "distilled_lib_1.h:64:3",
          enumConstantName = NamePair {
            nameC = CName "ENUM_CASE_1",
            nameHsIdent = HsIdentifier
              "ENUM_CASE_1"},
          enumConstantValue = 1}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "ENUM_CASE_2",
      patSynType = HsName
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "distilled_lib_1.h:65:3",
          enumConstantName = NamePair {
            nameC = CName "ENUM_CASE_2",
            nameHsIdent = HsIdentifier
              "ENUM_CASE_2"},
          enumConstantValue = 2}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "ENUM_CASE_3",
      patSynType = HsName
        "@NsTypeConstr"
        "A_typedef_enum_e",
      patSynConstr = HsName
        "@NsConstr"
        "A_typedef_enum_e",
      patSynValue = 3,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "distilled_lib_1.h:66:3",
          enumConstantName = NamePair {
            nameC = CName "ENUM_CASE_3",
            nameHsIdent = HsIdentifier
              "ENUM_CASE_3"},
          enumConstantValue = 3}},
  DeclInlineCInclude
    "distilled_lib_1.h",
  DeclInlineC
    "int32_t testmodule_some_fun (a_type_t *arg1, uint32_t arg2, uint8_t arg3[]) { return some_fun(arg1, arg2, arg3); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "some_fun",
      foreignImportType = HsFun
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "A_type_t")))
        (HsFun
          (HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word32"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word32"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]})
          (HsFun
            (HsPtr
              (HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word8"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word8"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}))
            (HsIO
              (HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Int32"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Int32"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]})))),
      foreignImportOrigName =
      "testmodule_some_fun",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePointer
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = CName "a_type_t",
                    nameHsIdent = HsIdentifier
                      "A_type_t"})),
            TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = CName "uint32_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word32"},
                extHsSpec = TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word32"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]}},
            TypeIncompleteArray
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = CName "uint8_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word8"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word8"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}})],
          functionRes = TypeExtBinding
            ResolvedExtBinding {
              extCName = QualName {
                qualNameName = CName "int32_t",
                qualNameKind =
                NameKindOrdinary},
              extHsRef = ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Int32"},
              extHsSpec = TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Int32"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}}}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Callback_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Callback_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Callback_t",
        fieldType = HsFunPtr
          (HsFun
            (HsPtr (HsPrimType HsPrimVoid))
            (HsFun
              (HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word32"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word32"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]})
              (HsIO
                (HsExtBinding
                  ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word32"}
                  TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word32"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]})))),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "distilled_lib_1.h:77:19",
          declId = NamePair {
            nameC = CName "callback_t",
            nameHsIdent = HsIdentifier
              "Callback_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "distilled_lib_1.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Callback_t",
              newtypeField = HsName
                "@NsVar"
                "un_Callback_t"},
            typedefType = TypePointer
              (TypeFun
                [
                  TypePointer TypeVoid,
                  TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = CName "uint32_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word32"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word32"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}}]
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = CName "uint32_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "Word32"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "Word32"),
                      typeSpecInstances = Map.fromList
                        [
                          _×_
                            Eq
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ord
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Enum
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Ix
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bounded
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Read
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Show
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Bits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            FiniteBits
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Integral
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Num
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Real
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            StaticSize
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            ReadRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            WriteRaw
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = []}),
                          _×_
                            Storable
                            (Require
                              InstanceSpec {
                                instanceSpecStrategy = Nothing,
                                instanceSpecConstraints = [
                                  ]})]}}))},
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
      "Callback_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Callback_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Callback_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Callback_t"),
  DeclInlineCInclude
    "distilled_lib_1.h",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "v",
      foreignImportType = HsPtr
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Var_t")),
      foreignImportOrigName = "v",
      foreignImportCallConv =
      CallConvGhcCCall ImportAsPtr,
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = CName "var_t",
              nameHsIdent = HsIdentifier
                "Var_t"}))}]
