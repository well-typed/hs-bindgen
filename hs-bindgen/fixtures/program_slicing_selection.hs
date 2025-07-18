[
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "C_IO_FILE",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "alltypes.h:320:16",
          declId = NamePair {
            nameC = Name "_IO_FILE",
            nameHsIdent = HsIdentifier
              "C_IO_FILE"},
          declOrigin = NameOriginInSource,
          declAliases = [Name "FILE"],
          declHeader =
          "program_slicing_selection.h"},
        declKind = OpaqueStruct,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "FileOperationStatus",
      newtypeConstr = HsName
        "@NsConstr"
        "FileOperationStatus",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_FileOperationStatus",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "program_slicing_selection.h:7:6",
          declId = NamePair {
            nameC = Name
              "FileOperationStatus",
            nameHsIdent = HsIdentifier
              "FileOperationStatus"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "program_slicing_selection.h"},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "FileOperationStatus",
              newtypeField = HsName
                "@NsVar"
                "un_FileOperationStatus"},
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "program_slicing_selection.h:8:3",
                enumConstantName = NamePair {
                  nameC = Name "SUCCESS",
                  nameHsIdent = HsIdentifier
                    "SUCCESS"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "program_slicing_selection.h:9:3",
                enumConstantName = NamePair {
                  nameC = Name "NOT_FOUND",
                  nameHsIdent = HsIdentifier
                    "NOT_FOUND"},
                enumConstantValue = 2},
              EnumConstant {
                enumConstantLoc =
                "program_slicing_selection.h:10:3",
                enumConstantName = NamePair {
                  nameC = Name
                    "PERMISSION_DENIED",
                  nameHsIdent = HsIdentifier
                    "PERMISSION_DENIED"},
                enumConstantValue = 13},
              EnumConstant {
                enumConstantLoc =
                "program_slicing_selection.h:11:3",
                enumConstantName = NamePair {
                  nameC = Name "INVALID_ARGUMENT",
                  nameHsIdent = HsIdentifier
                    "INVALID_ARGUMENT"},
                enumConstantValue = 22},
              EnumConstant {
                enumConstantLoc =
                "program_slicing_selection.h:12:3",
                enumConstantName = NamePair {
                  nameC = Name "OUT_OF_MEMORY",
                  nameHsIdent = HsIdentifier
                    "OUT_OF_MEMORY"},
                enumConstantValue = 12},
              EnumConstant {
                enumConstantLoc =
                "program_slicing_selection.h:13:3",
                enumConstantName = NamePair {
                  nameC = Name
                    "CUSTOM_ERROR_OTHER",
                  nameHsIdent = HsIdentifier
                    "CUSTOM_ERROR_OTHER"},
                enumConstantValue = `-1`}]},
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
          "FileOperationStatus",
        structConstr = HsName
          "@NsConstr"
          "FileOperationStatus",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_FileOperationStatus",
            fieldType = HsPrimType
              HsPrimCInt,
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
                  "FileOperationStatus",
                structConstr = HsName
                  "@NsConstr"
                  "FileOperationStatus",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_FileOperationStatus",
                    fieldType = HsPrimType
                      HsPrimCInt,
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
                  "FileOperationStatus",
                structConstr = HsName
                  "@NsConstr"
                  "FileOperationStatus",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_FileOperationStatus",
                    fieldType = HsPrimType
                      HsPrimCInt,
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
      "FileOperationStatus"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "FileOperationStatus"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "FileOperationStatus",
        structConstr = HsName
          "@NsConstr"
          "FileOperationStatus",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_FileOperationStatus",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCInt)
      (Map.fromList
        [
          _×_
            `-1`
            (NE.fromList
              ["CUSTOM_ERROR_OTHER"]),
          _×_ 0 (NE.fromList ["SUCCESS"]),
          _×_
            2
            (NE.fromList ["NOT_FOUND"]),
          _×_
            12
            (NE.fromList ["OUT_OF_MEMORY"]),
          _×_
            13
            (NE.fromList
              ["PERMISSION_DENIED"]),
          _×_
            22
            (NE.fromList
              ["INVALID_ARGUMENT"])])
      False),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "FileOperationStatus",
        structConstr = HsName
          "@NsConstr"
          "FileOperationStatus",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_FileOperationStatus",
            fieldType = HsPrimType
              HsPrimCInt,
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
          "FileOperationStatus",
        structConstr = HsName
          "@NsConstr"
          "FileOperationStatus",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_FileOperationStatus",
            fieldType = HsPrimType
              HsPrimCInt,
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
        "SUCCESS",
      patSynType = HsName
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = HsName
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "program_slicing_selection.h:8:3",
          enumConstantName = NamePair {
            nameC = Name "SUCCESS",
            nameHsIdent = HsIdentifier
              "SUCCESS"},
          enumConstantValue = 0}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "NOT_FOUND",
      patSynType = HsName
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = HsName
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "program_slicing_selection.h:9:3",
          enumConstantName = NamePair {
            nameC = Name "NOT_FOUND",
            nameHsIdent = HsIdentifier
              "NOT_FOUND"},
          enumConstantValue = 2}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "PERMISSION_DENIED",
      patSynType = HsName
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = HsName
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = 13,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "program_slicing_selection.h:10:3",
          enumConstantName = NamePair {
            nameC = Name
              "PERMISSION_DENIED",
            nameHsIdent = HsIdentifier
              "PERMISSION_DENIED"},
          enumConstantValue = 13}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "INVALID_ARGUMENT",
      patSynType = HsName
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = HsName
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = 22,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "program_slicing_selection.h:11:3",
          enumConstantName = NamePair {
            nameC = Name "INVALID_ARGUMENT",
            nameHsIdent = HsIdentifier
              "INVALID_ARGUMENT"},
          enumConstantValue = 22}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "OUT_OF_MEMORY",
      patSynType = HsName
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = HsName
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = 12,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "program_slicing_selection.h:12:3",
          enumConstantName = NamePair {
            nameC = Name "OUT_OF_MEMORY",
            nameHsIdent = HsIdentifier
              "OUT_OF_MEMORY"},
          enumConstantValue = 12}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "CUSTOM_ERROR_OTHER",
      patSynType = HsName
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = HsName
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = `-1`,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "program_slicing_selection.h:13:3",
          enumConstantName = NamePair {
            nameC = Name
              "CUSTOM_ERROR_OTHER",
            nameHsIdent = HsIdentifier
              "CUSTOM_ERROR_OTHER"},
          enumConstantValue = `-1`}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "FileOperationRecord",
      structConstr = HsName
        "@NsConstr"
        "FileOperationRecord",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "fileOperationRecord_status",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "FileOperationStatus"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "program_slicing_selection.h:17:28",
              structFieldName = NamePair {
                nameC = Name "status",
                nameHsIdent = HsIdentifier
                  "fileOperationRecord_status"},
              structFieldType = TypeEnum
                NamePair {
                  nameC = Name
                    "FileOperationStatus",
                  nameHsIdent = HsIdentifier
                    "FileOperationStatus"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "fileOperationRecord_bytes_processed",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "CSize"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "CSize"),
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
              "program_slicing_selection.h:18:10",
              structFieldName = NamePair {
                nameC = Name "bytes_processed",
                nameHsIdent = HsIdentifier
                  "fileOperationRecord_bytes_processed"},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "size_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "CSize"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "CSize"),
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
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "program_slicing_selection.h:16:8",
            declId = NamePair {
              nameC = Name
                "FileOperationRecord",
              nameHsIdent = HsIdentifier
                "FileOperationRecord"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "program_slicing_selection.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "FileOperationRecord"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "program_slicing_selection.h:17:28",
                  structFieldName = NamePair {
                    nameC = Name "status",
                    nameHsIdent = HsIdentifier
                      "fileOperationRecord_status"},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = Name
                        "FileOperationStatus",
                      nameHsIdent = HsIdentifier
                        "FileOperationStatus"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "program_slicing_selection.h:18:10",
                  structFieldName = NamePair {
                    nameC = Name "bytes_processed",
                    nameHsIdent = HsIdentifier
                      "fileOperationRecord_bytes_processed"},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "size_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "CSize"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "CSize"),
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
          "FileOperationRecord",
        structConstr = HsName
          "@NsConstr"
          "FileOperationRecord",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "fileOperationRecord_status",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "FileOperationStatus"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "program_slicing_selection.h:17:28",
                structFieldName = NamePair {
                  nameC = Name "status",
                  nameHsIdent = HsIdentifier
                    "fileOperationRecord_status"},
                structFieldType = TypeEnum
                  NamePair {
                    nameC = Name
                      "FileOperationStatus",
                    nameHsIdent = HsIdentifier
                      "FileOperationStatus"}
                  NameOriginInSource,
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "fileOperationRecord_bytes_processed",
            fieldType = HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "CSize"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "CSize"),
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
                "program_slicing_selection.h:18:10",
                structFieldName = NamePair {
                  nameC = Name "bytes_processed",
                  nameHsIdent = HsIdentifier
                    "fileOperationRecord_bytes_processed"},
                structFieldType = TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "size_t",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtHsRef {
                      extHsRefModule = HsModuleName
                        "HsBindgen.Runtime.Prelude",
                      extHsRefIdentifier =
                      HsIdentifier "CSize"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (HsModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (HsIdentifier "CSize"),
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
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "program_slicing_selection.h:16:8",
              declId = NamePair {
                nameC = Name
                  "FileOperationRecord",
                nameHsIdent = HsIdentifier
                  "FileOperationRecord"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader =
              "program_slicing_selection.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "FileOperationRecord"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "program_slicing_selection.h:17:28",
                    structFieldName = NamePair {
                      nameC = Name "status",
                      nameHsIdent = HsIdentifier
                        "fileOperationRecord_status"},
                    structFieldType = TypeEnum
                      NamePair {
                        nameC = Name
                          "FileOperationStatus",
                        nameHsIdent = HsIdentifier
                          "FileOperationStatus"}
                      NameOriginInSource,
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "program_slicing_selection.h:18:10",
                    structFieldName = NamePair {
                      nameC = Name "bytes_processed",
                      nameHsIdent = HsIdentifier
                        "fileOperationRecord_bytes_processed"},
                    structFieldType = TypeExtBinding
                      ResolvedExtBinding {
                        extCName = QualName {
                          qualNameName = Name "size_t",
                          qualNameKind =
                          NameKindOrdinary},
                        extHsRef = ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "CSize"},
                        extHsSpec = TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "CSize"),
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
                  "FileOperationRecord",
                structConstr = HsName
                  "@NsConstr"
                  "FileOperationRecord",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "fileOperationRecord_status",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "FileOperationStatus"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "program_slicing_selection.h:17:28",
                        structFieldName = NamePair {
                          nameC = Name "status",
                          nameHsIdent = HsIdentifier
                            "fileOperationRecord_status"},
                        structFieldType = TypeEnum
                          NamePair {
                            nameC = Name
                              "FileOperationStatus",
                            nameHsIdent = HsIdentifier
                              "FileOperationStatus"}
                          NameOriginInSource,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "fileOperationRecord_bytes_processed",
                    fieldType = HsExtBinding
                      ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "CSize"}
                      TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "CSize"),
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
                        "program_slicing_selection.h:18:10",
                        structFieldName = NamePair {
                          nameC = Name "bytes_processed",
                          nameHsIdent = HsIdentifier
                            "fileOperationRecord_bytes_processed"},
                        structFieldType = TypeExtBinding
                          ResolvedExtBinding {
                            extCName = QualName {
                              qualNameName = Name "size_t",
                              qualNameKind =
                              NameKindOrdinary},
                            extHsRef = ExtHsRef {
                              extHsRefModule = HsModuleName
                                "HsBindgen.Runtime.Prelude",
                              extHsRefIdentifier =
                              HsIdentifier "CSize"},
                            extHsSpec = TypeSpec {
                              typeSpecModule = Just
                                (HsModuleName
                                  "HsBindgen.Runtime.Prelude"),
                              typeSpecIdentifier = Just
                                (HsIdentifier "CSize"),
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
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "program_slicing_selection.h:16:8",
                      declId = NamePair {
                        nameC = Name
                          "FileOperationRecord",
                        nameHsIdent = HsIdentifier
                          "FileOperationRecord"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "program_slicing_selection.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "FileOperationRecord"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "program_slicing_selection.h:17:28",
                            structFieldName = NamePair {
                              nameC = Name "status",
                              nameHsIdent = HsIdentifier
                                "fileOperationRecord_status"},
                            structFieldType = TypeEnum
                              NamePair {
                                nameC = Name
                                  "FileOperationStatus",
                                nameHsIdent = HsIdentifier
                                  "FileOperationStatus"}
                              NameOriginInSource,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "program_slicing_selection.h:18:10",
                            structFieldName = NamePair {
                              nameC = Name "bytes_processed",
                              nameHsIdent = HsIdentifier
                                "fileOperationRecord_bytes_processed"},
                            structFieldType = TypeExtBinding
                              ResolvedExtBinding {
                                extCName = QualName {
                                  qualNameName = Name "size_t",
                                  qualNameKind =
                                  NameKindOrdinary},
                                extHsRef = ExtHsRef {
                                  extHsRefModule = HsModuleName
                                    "HsBindgen.Runtime.Prelude",
                                  extHsRefIdentifier =
                                  HsIdentifier "CSize"},
                                extHsSpec = TypeSpec {
                                  typeSpecModule = Just
                                    (HsModuleName
                                      "HsBindgen.Runtime.Prelude"),
                                  typeSpecIdentifier = Just
                                    (HsIdentifier "CSize"),
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
                  "FileOperationRecord",
                structConstr = HsName
                  "@NsConstr"
                  "FileOperationRecord",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "fileOperationRecord_status",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "FileOperationStatus"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "program_slicing_selection.h:17:28",
                        structFieldName = NamePair {
                          nameC = Name "status",
                          nameHsIdent = HsIdentifier
                            "fileOperationRecord_status"},
                        structFieldType = TypeEnum
                          NamePair {
                            nameC = Name
                              "FileOperationStatus",
                            nameHsIdent = HsIdentifier
                              "FileOperationStatus"}
                          NameOriginInSource,
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "fileOperationRecord_bytes_processed",
                    fieldType = HsExtBinding
                      ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "CSize"}
                      TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "CSize"),
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
                        "program_slicing_selection.h:18:10",
                        structFieldName = NamePair {
                          nameC = Name "bytes_processed",
                          nameHsIdent = HsIdentifier
                            "fileOperationRecord_bytes_processed"},
                        structFieldType = TypeExtBinding
                          ResolvedExtBinding {
                            extCName = QualName {
                              qualNameName = Name "size_t",
                              qualNameKind =
                              NameKindOrdinary},
                            extHsRef = ExtHsRef {
                              extHsRefModule = HsModuleName
                                "HsBindgen.Runtime.Prelude",
                              extHsRefIdentifier =
                              HsIdentifier "CSize"},
                            extHsSpec = TypeSpec {
                              typeSpecModule = Just
                                (HsModuleName
                                  "HsBindgen.Runtime.Prelude"),
                              typeSpecIdentifier = Just
                                (HsIdentifier "CSize"),
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
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "program_slicing_selection.h:16:8",
                      declId = NamePair {
                        nameC = Name
                          "FileOperationRecord",
                        nameHsIdent = HsIdentifier
                          "FileOperationRecord"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader =
                      "program_slicing_selection.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "FileOperationRecord"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "program_slicing_selection.h:17:28",
                            structFieldName = NamePair {
                              nameC = Name "status",
                              nameHsIdent = HsIdentifier
                                "fileOperationRecord_status"},
                            structFieldType = TypeEnum
                              NamePair {
                                nameC = Name
                                  "FileOperationStatus",
                                nameHsIdent = HsIdentifier
                                  "FileOperationStatus"}
                              NameOriginInSource,
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "program_slicing_selection.h:18:10",
                            structFieldName = NamePair {
                              nameC = Name "bytes_processed",
                              nameHsIdent = HsIdentifier
                                "fileOperationRecord_bytes_processed"},
                            structFieldType = TypeExtBinding
                              ResolvedExtBinding {
                                extCName = QualName {
                                  qualNameName = Name "size_t",
                                  qualNameKind =
                                  NameKindOrdinary},
                                extHsRef = ExtHsRef {
                                  extHsRefModule = HsModuleName
                                    "HsBindgen.Runtime.Prelude",
                                  extHsRefIdentifier =
                                  HsIdentifier "CSize"},
                                extHsSpec = TypeSpec {
                                  typeSpecModule = Just
                                    (HsModuleName
                                      "HsBindgen.Runtime.Prelude"),
                                  typeSpecIdentifier = Just
                                    (HsIdentifier "CSize"),
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
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "FileOperationRecord"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "FileOperationRecord"),
  DeclInlineCInclude
    "program_slicing_selection.h",
  DeclInlineC
    "enum FileOperationStatus testmodule_read_file_chunk (FILE *arg1, void *arg2, size_t arg3) { return read_file_chunk(arg1, arg2, arg3); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "read_file_chunk",
      foreignImportType = HsFun
        (HsPtr
          (HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "CFile"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "CFile"),
              typeSpecInstances = Map.fromList
                []}))
        (HsFun
          (HsPtr (HsPrimType HsPrimVoid))
          (HsFun
            (HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "CSize"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "CSize"),
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
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "FileOperationStatus"))))),
      foreignImportOrigName =
      "testmodule_read_file_chunk",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePointer
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "FILE",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "CFile"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "CFile"),
                    typeSpecInstances = Map.fromList
                      []}}),
            TypePointer TypeVoid,
            TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "size_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "CSize"},
                extHsSpec = TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "CSize"),
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
                              ]})]}}],
          functionRes = TypeEnum
            NamePair {
              nameC = Name
                "FileOperationStatus",
              nameHsIdent = HsIdentifier
                "FileOperationStatus"}
            NameOriginInSource}}]
