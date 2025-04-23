[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "PtrInt",
      newtypeConstr = HsName
        "@NsConstr"
        "PtrInt",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_PtrInt",
        fieldType = HsPtr
          (HsPrimType HsPrimCInt),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:2:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "PtrInt",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimIntegral PrimInt Signed)))
              []
              Declarator {
                declaratorPointer = Pointers
                  [_×_ [] []],
                directDeclarator =
                IdentifierDeclarator
                  AbstractName
                  []})},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "PtrInt"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "PtrInt"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "PtrInt"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "PtrInt"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "PtrPtrChar",
      newtypeConstr = HsName
        "@NsConstr"
        "PtrPtrChar",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_PtrPtrChar",
        fieldType = HsPtr
          (HsPtr
            (HsPrimType HsPrimCChar)),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:5:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "PtrPtrChar",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit Nothing))))
              []
              Declarator {
                declaratorPointer = Pointers
                  [_×_ [] [], _×_ [] []],
                directDeclarator =
                IdentifierDeclarator
                  AbstractName
                  []})},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "PtrPtrChar"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "PtrPtrChar"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "PtrPtrChar"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "PtrPtrChar"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr1",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr1",
        fieldType = HsConstArray
          2
          (HsPrimType HsPrimCInt),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:8:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "Arr1",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimIntegral PrimInt Signed)))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                ArrayDirectDeclarator
                  ArrayDeclarator {
                    arrayDirectDeclarator =
                    IdentifierDeclarator
                      AbstractName
                      [],
                    arrayStatic = False,
                    arrayTypeQualifiers = [],
                    arraySize = ArraySize
                      (SizeExpression
                        (MTerm
                          (MInt
                            IntegerLiteral {
                              integerLiteralText = "2",
                              integerLiteralType = Just
                                (_×_ PrimInt Signed),
                              integerLiteralValue = 2}))),
                    arrayAttributes = []}})},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Arr1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Arr1"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Arr1"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr2",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr2",
        fieldType = HsConstArray
          3
          (HsPtr
            (HsPrimType HsPrimCFloat)),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:11:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "Arr2",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimFloating PrimFloat)))
              []
              Declarator {
                declaratorPointer = Pointers
                  [_×_ [] []],
                directDeclarator =
                ArrayDirectDeclarator
                  ArrayDeclarator {
                    arrayDirectDeclarator =
                    IdentifierDeclarator
                      AbstractName
                      [],
                    arrayStatic = False,
                    arrayTypeQualifiers = [],
                    arraySize = ArraySize
                      (SizeExpression
                        (MTerm
                          (MInt
                            IntegerLiteral {
                              integerLiteralText = "3",
                              integerLiteralType = Just
                                (_×_ PrimInt Signed),
                              integerLiteralValue = 3}))),
                    arrayAttributes = []}})},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Arr2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Arr2"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Arr2"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Arr3",
      newtypeConstr = HsName
        "@NsConstr"
        "Arr3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Arr3",
        fieldType = HsConstArray
          4
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsPrimType HsPrimCFloat)))),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:14:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "Arr3",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimFloating PrimFloat)))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                FunctionDirectDeclarator
                  FunctionDeclarator {
                    functionDirectDeclarator =
                    ParenDeclarator
                      Declarator {
                        declaratorPointer = Pointers
                          [_×_ [] []],
                        directDeclarator =
                        ArrayDirectDeclarator
                          ArrayDeclarator {
                            arrayDirectDeclarator =
                            IdentifierDeclarator
                              AbstractName
                              [],
                            arrayStatic = False,
                            arrayTypeQualifiers = [],
                            arraySize = ArraySize
                              (SizeExpression
                                (MTerm
                                  (MInt
                                    IntegerLiteral {
                                      integerLiteralText = "4",
                                      integerLiteralType = Just
                                        (_×_ PrimInt Signed),
                                      integerLiteralValue = 4}))),
                            arrayAttributes = []}},
                    functionParameters = [
                      Parameter {
                        parameterAttributes = [],
                        parameterDeclSpecifiers =
                        NE.fromList
                          [
                            _×_
                              (DeclTypeSpecifierQualifier
                                (TSQ_TypeSpecifier
                                  (TypeSpecifier
                                    (TypePrim
                                      (PrimFloating PrimDouble)))))
                              []],
                        parameterDeclarator =
                        ParameterAbstractDeclarator
                          Declarator {
                            declaratorPointer = Pointers [],
                            directDeclarator =
                            IdentifierDeclarator
                              AbstractName
                              []}}],
                    functionVariadic = False,
                    functionAttributes = []}})},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Arr3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Arr3"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Arr3"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Fun1",
      newtypeConstr = HsName
        "@NsConstr"
        "Fun1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Fun1",
        fieldType = HsFun
          (HsPrimType HsPrimCInt)
          (HsIO
            (HsPtr
              (HsPrimType HsPrimCFloat))),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:17:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "Fun1",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimFloating PrimFloat)))
              []
              Declarator {
                declaratorPointer = Pointers
                  [_×_ [] []],
                directDeclarator =
                FunctionDirectDeclarator
                  FunctionDeclarator {
                    functionDirectDeclarator =
                    IdentifierDeclarator
                      AbstractName
                      [],
                    functionParameters = [
                      Parameter {
                        parameterAttributes = [],
                        parameterDeclSpecifiers =
                        NE.fromList
                          [
                            _×_
                              (DeclTypeSpecifierQualifier
                                (TSQ_TypeSpecifier
                                  (TypeSpecifier
                                    (TypePrim
                                      (PrimIntegral
                                        PrimInt
                                        Signed)))))
                              []],
                        parameterDeclarator =
                        ParameterAbstractDeclarator
                          Declarator {
                            declaratorPointer = Pointers [],
                            directDeclarator =
                            IdentifierDeclarator
                              AbstractName
                              []}}],
                    functionVariadic = False,
                    functionAttributes = []}})},
      newtypeInstances = Set.fromList
        []},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Fun2",
      newtypeConstr = HsName
        "@NsConstr"
        "Fun2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Fun2",
        fieldType = HsFunPtr
          (HsFun
            (HsPrimType HsPrimCFloat)
            (HsFun
              (HsPtr
                (HsPrimType HsPrimCDouble))
              (HsIO
                (HsPrimType HsPrimCInt)))),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:20:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "Fun2",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimIntegral PrimInt Signed)))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                FunctionDirectDeclarator
                  FunctionDeclarator {
                    functionDirectDeclarator =
                    ParenDeclarator
                      Declarator {
                        declaratorPointer = Pointers
                          [_×_ [] []],
                        directDeclarator =
                        IdentifierDeclarator
                          AbstractName
                          []},
                    functionParameters = [
                      Parameter {
                        parameterAttributes = [],
                        parameterDeclSpecifiers =
                        NE.fromList
                          [
                            _×_
                              (DeclTypeSpecifierQualifier
                                (TSQ_TypeSpecifier
                                  (TypeSpecifier
                                    (TypePrim
                                      (PrimFloating PrimFloat)))))
                              []],
                        parameterDeclarator =
                        ParameterAbstractDeclarator
                          Declarator {
                            declaratorPointer = Pointers [],
                            directDeclarator =
                            IdentifierDeclarator
                              AbstractName
                              []}},
                      Parameter {
                        parameterAttributes = [],
                        parameterDeclSpecifiers =
                        NE.fromList
                          [
                            _×_
                              (DeclTypeSpecifierQualifier
                                (TSQ_TypeSpecifier
                                  (TypeSpecifier
                                    (TypePrim
                                      (PrimFloating PrimDouble)))))
                              []],
                        parameterDeclarator =
                        ParameterAbstractDeclarator
                          Declarator {
                            declaratorPointer = Pointers
                              [_×_ [] []],
                            directDeclarator =
                            IdentifierDeclarator
                              AbstractName
                              []}}],
                    functionVariadic = False,
                    functionAttributes = []}})},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Fun2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Fun2"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Fun2"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Fun2"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Fun3",
      newtypeConstr = HsName
        "@NsConstr"
        "Fun3",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Fun3",
        fieldType = HsFunPtr
          (HsFun
            (HsPtr
              (HsPrimType HsPrimCFloat))
            (HsIO
              (HsPtr
                (HsPrimType HsPrimCInt)))),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:23:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "Fun3",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimIntegral PrimInt Signed)))
              []
              Declarator {
                declaratorPointer = Pointers
                  [_×_ [] []],
                directDeclarator =
                FunctionDirectDeclarator
                  FunctionDeclarator {
                    functionDirectDeclarator =
                    ParenDeclarator
                      Declarator {
                        declaratorPointer = Pointers
                          [_×_ [] []],
                        directDeclarator =
                        IdentifierDeclarator
                          AbstractName
                          []},
                    functionParameters = [
                      Parameter {
                        parameterAttributes = [],
                        parameterDeclSpecifiers =
                        NE.fromList
                          [
                            _×_
                              (DeclTypeSpecifierQualifier
                                (TSQ_TypeSpecifier
                                  (TypeSpecifier
                                    (TypePrim
                                      (PrimFloating PrimFloat)))))
                              []],
                        parameterDeclarator =
                        ParameterAbstractDeclarator
                          Declarator {
                            declaratorPointer = Pointers
                              [_×_ [] []],
                            directDeclarator =
                            IdentifierDeclarator
                              AbstractName
                              []}}],
                    functionVariadic = False,
                    functionAttributes = []}})},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Fun3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Fun3"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Fun3"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Fun3"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Fun4",
      newtypeConstr = HsName
        "@NsConstr"
        "Fun4",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Fun4",
        fieldType = HsFun
          (HsPrimType HsPrimCInt)
          (HsFun
            (HsPtr (HsPrimType HsPrimCLong))
            (HsIO
              (HsFunPtr
                (HsFun
                  (HsPrimType HsPrimCFloat)
                  (HsFun
                    (HsPtr
                      (HsPrimType HsPrimCDouble))
                    (HsIO
                      (HsPtr
                        (HsPrimType HsPrimCLong)))))))),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:26:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "Fun4",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimIntegral PrimLong Signed)))
              []
              Declarator {
                declaratorPointer = Pointers
                  [_×_ [] []],
                directDeclarator =
                FunctionDirectDeclarator
                  FunctionDeclarator {
                    functionDirectDeclarator =
                    ParenDeclarator
                      Declarator {
                        declaratorPointer = Pointers
                          [_×_ [] []],
                        directDeclarator =
                        FunctionDirectDeclarator
                          FunctionDeclarator {
                            functionDirectDeclarator =
                            IdentifierDeclarator
                              AbstractName
                              [],
                            functionParameters = [
                              Parameter {
                                parameterAttributes = [],
                                parameterDeclSpecifiers =
                                NE.fromList
                                  [
                                    _×_
                                      (DeclTypeSpecifierQualifier
                                        (TSQ_TypeSpecifier
                                          (TypeSpecifier
                                            (TypePrim
                                              (PrimIntegral
                                                PrimInt
                                                Signed)))))
                                      []],
                                parameterDeclarator =
                                ParameterAbstractDeclarator
                                  Declarator {
                                    declaratorPointer = Pointers [],
                                    directDeclarator =
                                    IdentifierDeclarator
                                      AbstractName
                                      []}},
                              Parameter {
                                parameterAttributes = [],
                                parameterDeclSpecifiers =
                                NE.fromList
                                  [
                                    _×_
                                      (DeclTypeSpecifierQualifier
                                        (TSQ_TypeSpecifier
                                          (TypeSpecifier
                                            (TypePrim
                                              (PrimIntegral
                                                PrimLong
                                                Signed)))))
                                      []],
                                parameterDeclarator =
                                ParameterAbstractDeclarator
                                  Declarator {
                                    declaratorPointer = Pointers
                                      [_×_ [] []],
                                    directDeclarator =
                                    IdentifierDeclarator
                                      AbstractName
                                      []}}],
                            functionVariadic = False,
                            functionAttributes = []}},
                    functionParameters = [
                      Parameter {
                        parameterAttributes = [],
                        parameterDeclSpecifiers =
                        NE.fromList
                          [
                            _×_
                              (DeclTypeSpecifierQualifier
                                (TSQ_TypeSpecifier
                                  (TypeSpecifier
                                    (TypePrim
                                      (PrimFloating PrimFloat)))))
                              []],
                        parameterDeclarator =
                        ParameterAbstractDeclarator
                          Declarator {
                            declaratorPointer = Pointers [],
                            directDeclarator =
                            IdentifierDeclarator
                              AbstractName
                              []}},
                      Parameter {
                        parameterAttributes = [],
                        parameterDeclSpecifiers =
                        NE.fromList
                          [
                            _×_
                              (DeclTypeSpecifierQualifier
                                (TSQ_TypeSpecifier
                                  (TypeSpecifier
                                    (TypePrim
                                      (PrimFloating PrimDouble)))))
                              []],
                        parameterDeclarator =
                        ParameterAbstractDeclarator
                          Declarator {
                            declaratorPointer = Pointers
                              [_×_ [] []],
                            directDeclarator =
                            IdentifierDeclarator
                              AbstractName
                              []}}],
                    functionVariadic = False,
                    functionAttributes = []}})},
      newtypeInstances = Set.fromList
        []},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Fun5",
      newtypeConstr = HsName
        "@NsConstr"
        "Fun5",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Fun5",
        fieldType = HsFun
          (HsConstArray
            8
            (HsPrimType HsPrimCChar))
          (HsIO
            (HsPtr
              (HsConstArray
                2
                (HsPtr
                  (HsPrimType HsPrimCShort))))),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:29:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "Fun5",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimIntegral
                    PrimShort
                    Signed)))
              []
              Declarator {
                declaratorPointer = Pointers
                  [_×_ [] []],
                directDeclarator =
                ArrayDirectDeclarator
                  ArrayDeclarator {
                    arrayDirectDeclarator =
                    ParenDeclarator
                      Declarator {
                        declaratorPointer = Pointers
                          [_×_ [] []],
                        directDeclarator =
                        FunctionDirectDeclarator
                          FunctionDeclarator {
                            functionDirectDeclarator =
                            IdentifierDeclarator
                              AbstractName
                              [],
                            functionParameters = [
                              Parameter {
                                parameterAttributes = [],
                                parameterDeclSpecifiers =
                                NE.fromList
                                  [
                                    _×_
                                      (DeclTypeSpecifierQualifier
                                        (TSQ_TypeSpecifier
                                          (TypeSpecifier
                                            (TypePrim
                                              (PrimChar
                                                (PrimSignImplicit Nothing))))))
                                      []],
                                parameterDeclarator =
                                ParameterAbstractDeclarator
                                  Declarator {
                                    declaratorPointer = Pointers [],
                                    directDeclarator =
                                    ArrayDirectDeclarator
                                      ArrayDeclarator {
                                        arrayDirectDeclarator =
                                        IdentifierDeclarator
                                          AbstractName
                                          [],
                                        arrayStatic = False,
                                        arrayTypeQualifiers = [],
                                        arraySize = ArraySize
                                          (SizeExpression
                                            (MTerm
                                              (MInt
                                                IntegerLiteral {
                                                  integerLiteralText = "8",
                                                  integerLiteralType = Just
                                                    (_×_ PrimInt Signed),
                                                  integerLiteralValue = 8}))),
                                        arrayAttributes = []}}}],
                            functionVariadic = False,
                            functionAttributes = []}},
                    arrayStatic = False,
                    arrayTypeQualifiers = [],
                    arraySize = ArraySize
                      (SizeExpression
                        (MTerm
                          (MInt
                            IntegerLiteral {
                              integerLiteralText = "2",
                              integerLiteralType = Just
                                (_×_ PrimInt Signed),
                              integerLiteralValue = 2}))),
                    arrayAttributes = []}})},
      newtypeInstances = Set.fromList
        []},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "MTy",
      newtypeConstr = HsName
        "@NsConstr"
        "MTy",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_MTy",
        fieldType = HsPrimType
          HsPrimCFloat,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:33:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "MTy",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimFloating PrimFloat)))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                IdentifierDeclarator
                  AbstractName
                  []})},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Read,
          Show,
          Floating,
          Fractional,
          Num,
          Real,
          RealFloat,
          RealFrac,
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    Floating
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    Fractional
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFloat
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFrac
    (HsName "@NsTypeConstr" "MTy"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Tty",
      newtypeConstr = HsName
        "@NsConstr"
        "Tty",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Tty",
        fieldType = HsTypRef
          (HsName "@NsTypeConstr" "MTy"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "tty",
          typedefType = TypeTypedef
            (CName "MTy"),
          typedefSourceLoc =
          "macro_types.h:34:13"},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Read,
          Show,
          Floating,
          Fractional,
          Num,
          Real,
          RealFloat,
          RealFrac,
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    Floating
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    Fractional
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFloat
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFrac
    (HsName "@NsTypeConstr" "Tty"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "UINT8_T",
      newtypeConstr = HsName
        "@NsConstr"
        "UINT8_T",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_UINT8_T",
        fieldType = HsPrimType
          HsPrimCUChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:36:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "UINT8_T",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimChar
                    (PrimSignExplicit Unsigned))))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                IdentifierDeclarator
                  AbstractName
                  []})},
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
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "BOOLEAN_T",
      newtypeConstr = HsName
        "@NsConstr"
        "BOOLEAN_T",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_BOOLEAN_T",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "UINT8_T"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_types.h:37:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "BOOLEAN_T",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeDefTypeSpecifier
                (CName "UINT8_T"))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                IdentifierDeclarator
                  AbstractName
                  []})},
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
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "BOOLEAN_T"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Boolean_T",
      newtypeConstr = HsName
        "@NsConstr"
        "Boolean_T",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Boolean_T",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "BOOLEAN_T"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "boolean_T",
          typedefType = TypeTypedef
            (CName "BOOLEAN_T"),
          typedefSourceLoc =
          "macro_types.h:38:19"},
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
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Boolean_T")]
