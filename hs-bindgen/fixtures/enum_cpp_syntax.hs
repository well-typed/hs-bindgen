[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Foo_enum",
      newtypeConstr = HsName
        "@NsConstr"
        "Foo_enum",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Foo_enum",
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
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "enum_cpp_syntax.h:4:9",
          declId = NamePair {
            nameC = Name "foo_enum",
            nameHsIdent = HsIdentifier
              "Foo_enum"},
          declOrigin = NameOriginGenerated
            (AnonId
              "enum_cpp_syntax.h:4:9"),
          declAliases = [Name "foo_enum"],
          declHeader =
          "enum_cpp_syntax.h",
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Foo_enum",
              newtypeField = HsName
                "@NsVar"
                "un_Foo_enum"},
            enumType = TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "uint32_t",
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
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "enum_cpp_syntax.h:4:27",
                  fieldName = NamePair {
                    nameC = Name "A",
                    nameHsIdent = HsIdentifier "A"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "enum_cpp_syntax.h:4:30",
                  fieldName = NamePair {
                    nameC = Name "B",
                    nameHsIdent = HsIdentifier "B"},
                  fieldComment = Nothing},
                enumConstantValue = 1},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "enum_cpp_syntax.h:4:33",
                  fieldName = NamePair {
                    nameC = Name "C",
                    nameHsIdent = HsIdentifier "C"},
                  fieldComment = Nothing},
                enumConstantValue = 2}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        (Comment
          Nothing
          (Just "enum_cpp_syntax.h:4:9")
          (Just "enum_cpp_syntax.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Foo_enum",
          structConstr = HsName
            "@NsConstr"
            "Foo_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Foo_enum",
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
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
                    "Foo_enum",
                  structConstr = HsName
                    "@NsConstr"
                    "Foo_enum",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_Foo_enum",
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
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
                  structComment = Nothing})
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
                    "Foo_enum",
                  structConstr = HsName
                    "@NsConstr"
                    "Foo_enum",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "un_Foo_enum",
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
                      fieldOrigin = GeneratedField,
                      fieldComment = Nothing}],
                  structOrigin = Nothing,
                  structInstances = Set.fromList
                    [Eq, Ord, Read, Show, Storable],
                  structComment = Nothing}
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Foo_enum",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Foo_enum",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Foo_enum",
          structConstr = HsName
            "@NsConstr"
            "Foo_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Foo_enum",
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
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
        (Map.fromList
          [
            _×_ 0 (NE.fromList ["A"]),
            _×_ 1 (NE.fromList ["B"]),
            _×_ 2 (NE.fromList ["C"])])
        True,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceSequentialCEnum
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Foo_enum",
          structConstr = HsName
            "@NsConstr"
            "Foo_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Foo_enum",
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
        (HsName "@NsConstr" "A")
        (HsName "@NsConstr" "C"),
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Foo_enum",
          structConstr = HsName
            "@NsConstr"
            "Foo_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Foo_enum",
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumRead
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Foo_enum",
          structConstr = HsName
            "@NsConstr"
            "Foo_enum",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "un_Foo_enum",
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
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing},
      defineInstanceComment =
      Nothing},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "A",
      patSynType = HsName
        "@NsTypeConstr"
        "Foo_enum",
      patSynConstr = HsName
        "@NsConstr"
        "Foo_enum",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "enum_cpp_syntax.h:4:27",
            fieldName = NamePair {
              nameC = Name "A",
              nameHsIdent = HsIdentifier "A"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        (Comment
          Nothing
          (Just "enum_cpp_syntax.h:4:27")
          (Just "enum_cpp_syntax.h")
          [])},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "B",
      patSynType = HsName
        "@NsTypeConstr"
        "Foo_enum",
      patSynConstr = HsName
        "@NsConstr"
        "Foo_enum",
      patSynValue = 1,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "enum_cpp_syntax.h:4:30",
            fieldName = NamePair {
              nameC = Name "B",
              nameHsIdent = HsIdentifier "B"},
            fieldComment = Nothing},
          enumConstantValue = 1},
      patSynComment = Just
        (Comment
          Nothing
          (Just "enum_cpp_syntax.h:4:30")
          (Just "enum_cpp_syntax.h")
          [])},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "C",
      patSynType = HsName
        "@NsTypeConstr"
        "Foo_enum",
      patSynConstr = HsName
        "@NsConstr"
        "Foo_enum",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "enum_cpp_syntax.h:4:33",
            fieldName = NamePair {
              nameC = Name "C",
              nameHsIdent = HsIdentifier "C"},
            fieldComment = Nothing},
          enumConstantValue = 2},
      patSynComment = Just
        (Comment
          Nothing
          (Just "enum_cpp_syntax.h:4:33")
          (Just "enum_cpp_syntax.h")
          [])}]
