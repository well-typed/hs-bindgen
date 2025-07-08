[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "FILE",
      structConstr = HsName
        "@NsConstr"
        "FILE",
      structFields = [],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "fun_attributes.h:10:9",
            declId = NamePair {
              nameC = CName "FILE",
              nameHsIdent = HsIdentifier
                "FILE"},
            declOrigin = NameOriginGenerated
              (AnonId
                "fun_attributes.h:10:9"),
            declAliases = [CName "FILE"],
            declHeader =
            "fun_attributes.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "FILE"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
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
          "FILE",
        structConstr = HsName
          "@NsConstr"
          "FILE",
        structFields = [],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "fun_attributes.h:10:9",
              declId = NamePair {
                nameC = CName "FILE",
                nameHsIdent = HsIdentifier
                  "FILE"},
              declOrigin = NameOriginGenerated
                (AnonId
                  "fun_attributes.h:10:9"),
              declAliases = [CName "FILE"],
              declHeader =
              "fun_attributes.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "FILE"),
                structSizeof = 0,
                structAlignment = 1,
                structFields = [],
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
        storableSizeOf = 0,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "FILE",
                structConstr = HsName
                  "@NsConstr"
                  "FILE",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "fun_attributes.h:10:9",
                      declId = NamePair {
                        nameC = CName "FILE",
                        nameHsIdent = HsIdentifier
                          "FILE"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "fun_attributes.h:10:9"),
                      declAliases = [CName "FILE"],
                      declHeader =
                      "fun_attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "FILE"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]})
            []),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "FILE",
                structConstr = HsName
                  "@NsConstr"
                  "FILE",
                structFields = [],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "fun_attributes.h:10:9",
                      declId = NamePair {
                        nameC = CName "FILE",
                        nameHsIdent = HsIdentifier
                          "FILE"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "fun_attributes.h:10:9"),
                      declAliases = [CName "FILE"],
                      declHeader =
                      "fun_attributes.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "FILE"),
                        structSizeof = 0,
                        structAlignment = 1,
                        structFields = [],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "FILE"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "FILE"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Size_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Size_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Size_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "fun_attributes.h:11:13",
          declId = NamePair {
            nameC = CName "size_t",
            nameHsIdent = HsIdentifier
              "Size_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "fun_attributes.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Size_t",
              newtypeField = HsName
                "@NsVar"
                "un_Size_t"},
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
      "Size_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Size_t"),
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void testmodule___f1 (void) { __f1(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "__f1",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule___f1",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void testmodule_f1 (void) { f1(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f1",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_f1",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *testmodule_my_memalign (size_t arg1, size_t arg2) { return my_memalign(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_memalign",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Size_t"))
        (HsFun
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"))
          (HsIO
            (HsPtr
              (HsPrimType HsPrimVoid)))),
      foreignImportOrigName =
      "testmodule_my_memalign",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"}),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})],
          functionRes = TypePointer
            TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *testmodule_my_calloc (size_t arg1, size_t arg2) { return my_calloc(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_calloc",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Size_t"))
        (HsFun
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"))
          (HsIO
            (HsPtr
              (HsPrimType HsPrimVoid)))),
      foreignImportOrigName =
      "testmodule_my_calloc",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"}),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})],
          functionRes = TypePointer
            TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *testmodule_my_realloc (void *arg1, size_t arg2) { return my_realloc(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_realloc",
      foreignImportType = HsFun
        (HsPtr (HsPrimType HsPrimVoid))
        (HsFun
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"))
          (HsIO
            (HsPtr
              (HsPrimType HsPrimVoid)))),
      foreignImportOrigName =
      "testmodule_my_realloc",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer TypeVoid,
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})],
          functionRes = TypePointer
            TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *testmodule_my_alloc1 (size_t arg1) { return my_alloc1(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_alloc1",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Size_t"))
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "testmodule_my_alloc1",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})],
          functionRes = TypePointer
            TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *testmodule_my_alloc2 (size_t arg1) { return my_alloc2(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_alloc2",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Size_t"))
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "testmodule_my_alloc2",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})],
          functionRes = TypePointer
            TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int testmodule_square (signed int arg1) { return square(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "testmodule_square",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int testmodule_old_fn (void) { return old_fn(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "old_fn",
      foreignImportType = HsIO
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "testmodule_old_fn",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "char *testmodule_my_dgettext (char *arg1, char *arg2) { return my_dgettext(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_dgettext",
      foreignImportType = HsFun
        (HsPtr (HsPrimType HsPrimCChar))
        (HsFun
          (HsPtr (HsPrimType HsPrimCChar))
          (HsIO
            (HsPtr
              (HsPrimType HsPrimCChar)))),
      foreignImportOrigName =
      "testmodule_my_dgettext",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed)))),
            TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))))],
          functionRes = TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed))))}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "FILE *testmodule_fdopen (signed int arg1, char *arg2) { return fdopen(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fdopen",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPtr (HsPrimType HsPrimCChar))
          (HsIO
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "FILE"))))),
      foreignImportOrigName =
      "testmodule_fdopen",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))))],
          functionRes = TypePointer
            (TypeTypedef
              (TypedefSquashed
                (CName "FILE")
                (TypeStruct
                  NamePair {
                    nameC = CName "FILE",
                    nameHsIdent = HsIdentifier
                      "FILE"}
                  (NameOriginGenerated
                    (AnonId
                      "fun_attributes.h:10:9")))))}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void testmodule_f2 (void) { f2(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f2",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_f2",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *testmodule_my_memcpy (void *arg1, void *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_memcpy",
      foreignImportType = HsFun
        (HsPtr (HsPrimType HsPrimVoid))
        (HsFun
          (HsPtr (HsPrimType HsPrimVoid))
          (HsFun
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Size_t"))
            (HsIO
              (HsPtr
                (HsPrimType HsPrimVoid))))),
      foreignImportOrigName =
      "testmodule_my_memcpy",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer TypeVoid,
            TypePointer TypeVoid,
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})],
          functionRes = TypePointer
            TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void testmodule_fatal (void) { fatal(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fatal",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_fatal",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int testmodule_hash (char *arg1) { return hash(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hash",
      foreignImportType = HsFun
        (HsPtr (HsPrimType HsPrimCChar))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "testmodule_hash",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))))],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *testmodule_mymalloc (size_t arg1) { return mymalloc(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "mymalloc",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Size_t"))
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "testmodule_mymalloc",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = CName "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})],
          functionRes = TypePointer
            TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void testmodule_foobar (void) { foobar(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "foobar",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_foobar",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int testmodule_core2_func (void) { return core2_func(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "core2_func",
      foreignImportType = HsIO
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "testmodule_core2_func",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int testmodule_sse3_func (void) { return sse3_func(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "sse3_func",
      foreignImportType = HsIO
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "testmodule_sse3_func",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void testmodule_f3 (void) { f3(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f3",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_f3",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int testmodule_fn (void) { return fn(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fn",
      foreignImportType = HsIO
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "testmodule_fn",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int testmodule_y (void) { return y(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "y",
      foreignImportType = HsIO
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "testmodule_y",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int testmodule_x1 (void) { return x1(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "x1",
      foreignImportType = HsIO
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "testmodule_x1",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int testmodule_x2 (void) { return x2(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "x2",
      foreignImportType = HsIO
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "testmodule_x2",
      foreignImportHeader =
      "fun_attributes.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}}]
