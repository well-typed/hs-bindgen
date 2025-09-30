[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "FileOperationStatus",
      newtypeConstr = Name
        "@NsConstr"
        "FileOperationStatus",
      newtypeField = Field {
        fieldName = Name
          "@NsVar"
          "un_FileOperationStatus",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "program_slicing_selection.h:7:6",
          declId = NamePair {
            nameC = Name
              "FileOperationStatus",
            nameHsIdent = Identifier
              "FileOperationStatus"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          declComment = Nothing},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = Name
                "@NsConstr"
                "FileOperationStatus",
              newtypeField = Name
                "@NsVar"
                "un_FileOperationStatus"},
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "program_slicing_selection.h:8:3",
                  fieldName = NamePair {
                    nameC = Name "SUCCESS",
                    nameHsIdent = Identifier
                      "SUCCESS"},
                  fieldComment = Nothing},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "program_slicing_selection.h:9:3",
                  fieldName = NamePair {
                    nameC = Name "NOT_FOUND",
                    nameHsIdent = Identifier
                      "NOT_FOUND"},
                  fieldComment = Nothing},
                enumConstantValue = 2},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "program_slicing_selection.h:10:3",
                  fieldName = NamePair {
                    nameC = Name
                      "PERMISSION_DENIED",
                    nameHsIdent = Identifier
                      "PERMISSION_DENIED"},
                  fieldComment = Nothing},
                enumConstantValue = 13},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "program_slicing_selection.h:11:3",
                  fieldName = NamePair {
                    nameC = Name "INVALID_ARGUMENT",
                    nameHsIdent = Identifier
                      "INVALID_ARGUMENT"},
                  fieldComment = Nothing},
                enumConstantValue = 22},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "program_slicing_selection.h:12:3",
                  fieldName = NamePair {
                    nameC = Name "OUT_OF_MEMORY",
                    nameHsIdent = Identifier
                      "OUT_OF_MEMORY"},
                  fieldComment = Nothing},
                enumConstantValue = 12},
              EnumConstant {
                enumConstantInfo = FieldInfo {
                  fieldLoc =
                  "program_slicing_selection.h:13:3",
                  fieldName = NamePair {
                    nameC = Name
                      "CUSTOM_ERROR_OTHER",
                    nameHsIdent = Identifier
                      "CUSTOM_ERROR_OTHER"},
                  fieldComment = Nothing},
                enumConstantValue = `-1`}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Read, Show, Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "FileOperationStatus",
          commentLocation = Just
            "program_slicing_selection.h:7:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "FileOperationStatus",
          structConstr = Name
            "@NsConstr"
            "FileOperationStatus",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_FileOperationStatus",
              fieldType = HsPrimType
                HsPrimCInt,
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
                  structName = Name
                    "@NsTypeConstr"
                    "FileOperationStatus",
                  structConstr = Name
                    "@NsConstr"
                    "FileOperationStatus",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_FileOperationStatus",
                      fieldType = HsPrimType
                        HsPrimCInt,
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
                  structName = Name
                    "@NsTypeConstr"
                    "FileOperationStatus",
                  structConstr = Name
                    "@NsConstr"
                    "FileOperationStatus",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "un_FileOperationStatus",
                      fieldType = HsPrimType
                        HsPrimCInt,
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
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FileOperationStatus",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FileOperationStatus",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnum
        Struct {
          structName = Name
            "@NsTypeConstr"
            "FileOperationStatus",
          structConstr = Name
            "@NsConstr"
            "FileOperationStatus",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_FileOperationStatus",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = GeneratedField,
              fieldComment = Nothing}],
          structOrigin = Nothing,
          structInstances = Set.fromList
            [Eq, Ord, Read, Show, Storable],
          structComment = Nothing}
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
        False,
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceCEnumShow
        Struct {
          structName = Name
            "@NsTypeConstr"
            "FileOperationStatus",
          structConstr = Name
            "@NsConstr"
            "FileOperationStatus",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_FileOperationStatus",
              fieldType = HsPrimType
                HsPrimCInt,
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
          structName = Name
            "@NsTypeConstr"
            "FileOperationStatus",
          structConstr = Name
            "@NsConstr"
            "FileOperationStatus",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "un_FileOperationStatus",
              fieldType = HsPrimType
                HsPrimCInt,
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
      patSynName = Name
        "@NsConstr"
        "SUCCESS",
      patSynType = Name
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = Name
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = 0,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "program_slicing_selection.h:8:3",
            fieldName = NamePair {
              nameC = Name "SUCCESS",
              nameHsIdent = Identifier
                "SUCCESS"},
            fieldComment = Nothing},
          enumConstantValue = 0},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "SUCCESS",
          commentLocation = Just
            "program_slicing_selection.h:8:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "NOT_FOUND",
      patSynType = Name
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = Name
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = 2,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "program_slicing_selection.h:9:3",
            fieldName = NamePair {
              nameC = Name "NOT_FOUND",
              nameHsIdent = Identifier
                "NOT_FOUND"},
            fieldComment = Nothing},
          enumConstantValue = 2},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "NOT_FOUND",
          commentLocation = Just
            "program_slicing_selection.h:9:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "PERMISSION_DENIED",
      patSynType = Name
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = Name
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = 13,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "program_slicing_selection.h:10:3",
            fieldName = NamePair {
              nameC = Name
                "PERMISSION_DENIED",
              nameHsIdent = Identifier
                "PERMISSION_DENIED"},
            fieldComment = Nothing},
          enumConstantValue = 13},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "PERMISSION_DENIED",
          commentLocation = Just
            "program_slicing_selection.h:10:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "INVALID_ARGUMENT",
      patSynType = Name
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = Name
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = 22,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "program_slicing_selection.h:11:3",
            fieldName = NamePair {
              nameC = Name "INVALID_ARGUMENT",
              nameHsIdent = Identifier
                "INVALID_ARGUMENT"},
            fieldComment = Nothing},
          enumConstantValue = 22},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "INVALID_ARGUMENT",
          commentLocation = Just
            "program_slicing_selection.h:11:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "OUT_OF_MEMORY",
      patSynType = Name
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = Name
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = 12,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "program_slicing_selection.h:12:3",
            fieldName = NamePair {
              nameC = Name "OUT_OF_MEMORY",
              nameHsIdent = Identifier
                "OUT_OF_MEMORY"},
            fieldComment = Nothing},
          enumConstantValue = 12},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "OUT_OF_MEMORY",
          commentLocation = Just
            "program_slicing_selection.h:12:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          commentChildren = []}},
  DeclPatSyn
    PatSyn {
      patSynName = Name
        "@NsConstr"
        "CUSTOM_ERROR_OTHER",
      patSynType = Name
        "@NsTypeConstr"
        "FileOperationStatus",
      patSynConstr = Name
        "@NsConstr"
        "FileOperationStatus",
      patSynValue = `-1`,
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantInfo = FieldInfo {
            fieldLoc =
            "program_slicing_selection.h:13:3",
            fieldName = NamePair {
              nameC = Name
                "CUSTOM_ERROR_OTHER",
              nameHsIdent = Identifier
                "CUSTOM_ERROR_OTHER"},
            fieldComment = Nothing},
          enumConstantValue = `-1`},
      patSynComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "CUSTOM_ERROR_OTHER",
          commentLocation = Just
            "program_slicing_selection.h:13:3",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          commentChildren = []}},
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "FileOperationRecord",
      structConstr = Name
        "@NsConstr"
        "FileOperationRecord",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "fileOperationRecord_status",
          fieldType = HsTypRef
            (Name
              "@NsTypeConstr"
              "FileOperationStatus"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "program_slicing_selection.h:17:28",
                fieldName = NamePair {
                  nameC = Name "status",
                  nameHsIdent = Identifier
                    "fileOperationRecord_status"},
                fieldComment = Nothing},
              structFieldType = TypeEnum
                NamePair {
                  nameC = Name
                    "FileOperationStatus",
                  nameHsIdent = Identifier
                    "FileOperationStatus"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "status",
              commentLocation = Just
                "program_slicing_selection.h:17:28",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["program_slicing_selection.h"],
                  headerInclude =
                  "program_slicing_selection.h"},
              commentChildren = []}},
        Field {
          fieldName = Name
            "@NsVar"
            "fileOperationRecord_bytes_processed",
          fieldType = HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "CSize"}
            TypeSpec {
              typeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (Identifier "CSize"),
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
              structFieldInfo = FieldInfo {
                fieldLoc =
                "program_slicing_selection.h:18:10",
                fieldName = NamePair {
                  nameC = Name "bytes_processed",
                  nameHsIdent = Identifier
                    "fileOperationRecord_bytes_processed"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "size_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "CSize"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (Identifier "CSize"),
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
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "bytes_processed",
              commentLocation = Just
                "program_slicing_selection.h:18:10",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["program_slicing_selection.h"],
                  headerInclude =
                  "program_slicing_selection.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "program_slicing_selection.h:16:8",
            declId = NamePair {
              nameC = Name
                "FileOperationRecord",
              nameHsIdent = Identifier
                "FileOperationRecord"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["program_slicing_selection.h"],
                headerInclude =
                "program_slicing_selection.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name
                  "@NsConstr"
                  "FileOperationRecord"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "program_slicing_selection.h:17:28",
                    fieldName = NamePair {
                      nameC = Name "status",
                      nameHsIdent = Identifier
                        "fileOperationRecord_status"},
                    fieldComment = Nothing},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = Name
                        "FileOperationStatus",
                      nameHsIdent = Identifier
                        "FileOperationStatus"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "program_slicing_selection.h:18:10",
                    fieldName = NamePair {
                      nameC = Name "bytes_processed",
                      nameHsIdent = Identifier
                        "fileOperationRecord_bytes_processed"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "size_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "CSize"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (Identifier "CSize"),
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
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "FileOperationRecord",
          commentLocation = Just
            "program_slicing_selection.h:16:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "FileOperationRecord",
          structConstr = Name
            "@NsConstr"
            "FileOperationRecord",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "fileOperationRecord_status",
              fieldType = HsTypRef
                (Name
                  "@NsTypeConstr"
                  "FileOperationStatus"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "program_slicing_selection.h:17:28",
                    fieldName = NamePair {
                      nameC = Name "status",
                      nameHsIdent = Identifier
                        "fileOperationRecord_status"},
                    fieldComment = Nothing},
                  structFieldType = TypeEnum
                    NamePair {
                      nameC = Name
                        "FileOperationStatus",
                      nameHsIdent = Identifier
                        "FileOperationStatus"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "status",
                  commentLocation = Just
                    "program_slicing_selection.h:17:28",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["program_slicing_selection.h"],
                      headerInclude =
                      "program_slicing_selection.h"},
                  commentChildren = []}},
            Field {
              fieldName = Name
                "@NsVar"
                "fileOperationRecord_bytes_processed",
              fieldType = HsExtBinding
                ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "CSize"}
                TypeSpec {
                  typeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (Identifier "CSize"),
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
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "program_slicing_selection.h:18:10",
                    fieldName = NamePair {
                      nameC = Name "bytes_processed",
                      nameHsIdent = Identifier
                        "fileOperationRecord_bytes_processed"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "size_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtRef {
                        extRefModule = ModuleName
                          "HsBindgen.Runtime.Prelude",
                        extRefIdentifier = Identifier
                          "CSize"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (ModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (Identifier "CSize"),
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
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just
                    "bytes_processed",
                  commentLocation = Just
                    "program_slicing_selection.h:18:10",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["program_slicing_selection.h"],
                      headerInclude =
                      "program_slicing_selection.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "program_slicing_selection.h:16:8",
                declId = NamePair {
                  nameC = Name
                    "FileOperationRecord",
                  nameHsIdent = Identifier
                    "FileOperationRecord"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["program_slicing_selection.h"],
                    headerInclude =
                    "program_slicing_selection.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name
                      "@NsConstr"
                      "FileOperationRecord"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "program_slicing_selection.h:17:28",
                        fieldName = NamePair {
                          nameC = Name "status",
                          nameHsIdent = Identifier
                            "fileOperationRecord_status"},
                        fieldComment = Nothing},
                      structFieldType = TypeEnum
                        NamePair {
                          nameC = Name
                            "FileOperationStatus",
                          nameHsIdent = Identifier
                            "FileOperationStatus"}
                        NameOriginInSource,
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "program_slicing_selection.h:18:10",
                        fieldName = NamePair {
                          nameC = Name "bytes_processed",
                          nameHsIdent = Identifier
                            "fileOperationRecord_bytes_processed"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "size_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtRef {
                            extRefModule = ModuleName
                              "HsBindgen.Runtime.Prelude",
                            extRefIdentifier = Identifier
                              "CSize"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (ModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (Identifier "CSize"),
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
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "FileOperationRecord",
              commentLocation = Just
                "program_slicing_selection.h:16:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["program_slicing_selection.h"],
                  headerInclude =
                  "program_slicing_selection.h"},
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
                    "FileOperationRecord",
                  structConstr = Name
                    "@NsConstr"
                    "FileOperationRecord",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "fileOperationRecord_status",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "FileOperationStatus"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "program_slicing_selection.h:17:28",
                            fieldName = NamePair {
                              nameC = Name "status",
                              nameHsIdent = Identifier
                                "fileOperationRecord_status"},
                            fieldComment = Nothing},
                          structFieldType = TypeEnum
                            NamePair {
                              nameC = Name
                                "FileOperationStatus",
                              nameHsIdent = Identifier
                                "FileOperationStatus"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "status",
                          commentLocation = Just
                            "program_slicing_selection.h:17:28",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["program_slicing_selection.h"],
                              headerInclude =
                              "program_slicing_selection.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "fileOperationRecord_bytes_processed",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "CSize"}
                        TypeSpec {
                          typeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (Identifier "CSize"),
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "program_slicing_selection.h:18:10",
                            fieldName = NamePair {
                              nameC = Name "bytes_processed",
                              nameHsIdent = Identifier
                                "fileOperationRecord_bytes_processed"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "size_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "CSize"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (Identifier "CSize"),
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
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "bytes_processed",
                          commentLocation = Just
                            "program_slicing_selection.h:18:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["program_slicing_selection.h"],
                              headerInclude =
                              "program_slicing_selection.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "program_slicing_selection.h:16:8",
                        declId = NamePair {
                          nameC = Name
                            "FileOperationRecord",
                          nameHsIdent = Identifier
                            "FileOperationRecord"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["program_slicing_selection.h"],
                            headerInclude =
                            "program_slicing_selection.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "FileOperationRecord"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "program_slicing_selection.h:17:28",
                                fieldName = NamePair {
                                  nameC = Name "status",
                                  nameHsIdent = Identifier
                                    "fileOperationRecord_status"},
                                fieldComment = Nothing},
                              structFieldType = TypeEnum
                                NamePair {
                                  nameC = Name
                                    "FileOperationStatus",
                                  nameHsIdent = Identifier
                                    "FileOperationStatus"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "program_slicing_selection.h:18:10",
                                fieldName = NamePair {
                                  nameC = Name "bytes_processed",
                                  nameHsIdent = Identifier
                                    "fileOperationRecord_bytes_processed"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "size_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "CSize"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (Identifier "CSize"),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "FileOperationRecord",
                      commentLocation = Just
                        "program_slicing_selection.h:16:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["program_slicing_selection.h"],
                          headerInclude =
                          "program_slicing_selection.h"},
                      commentChildren = []}})
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
                  structName = Name
                    "@NsTypeConstr"
                    "FileOperationRecord",
                  structConstr = Name
                    "@NsConstr"
                    "FileOperationRecord",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "fileOperationRecord_status",
                      fieldType = HsTypRef
                        (Name
                          "@NsTypeConstr"
                          "FileOperationStatus"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "program_slicing_selection.h:17:28",
                            fieldName = NamePair {
                              nameC = Name "status",
                              nameHsIdent = Identifier
                                "fileOperationRecord_status"},
                            fieldComment = Nothing},
                          structFieldType = TypeEnum
                            NamePair {
                              nameC = Name
                                "FileOperationStatus",
                              nameHsIdent = Identifier
                                "FileOperationStatus"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "status",
                          commentLocation = Just
                            "program_slicing_selection.h:17:28",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["program_slicing_selection.h"],
                              headerInclude =
                              "program_slicing_selection.h"},
                          commentChildren = []}},
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "fileOperationRecord_bytes_processed",
                      fieldType = HsExtBinding
                        ExtRef {
                          extRefModule = ModuleName
                            "HsBindgen.Runtime.Prelude",
                          extRefIdentifier = Identifier
                            "CSize"}
                        TypeSpec {
                          typeSpecModule = Just
                            (ModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (Identifier "CSize"),
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
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "program_slicing_selection.h:18:10",
                            fieldName = NamePair {
                              nameC = Name "bytes_processed",
                              nameHsIdent = Identifier
                                "fileOperationRecord_bytes_processed"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "size_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtRef {
                                extRefModule = ModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extRefIdentifier = Identifier
                                  "CSize"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (ModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (Identifier "CSize"),
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
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just
                            "bytes_processed",
                          commentLocation = Just
                            "program_slicing_selection.h:18:10",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["program_slicing_selection.h"],
                              headerInclude =
                              "program_slicing_selection.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "program_slicing_selection.h:16:8",
                        declId = NamePair {
                          nameC = Name
                            "FileOperationRecord",
                          nameHsIdent = Identifier
                            "FileOperationRecord"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["program_slicing_selection.h"],
                            headerInclude =
                            "program_slicing_selection.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name
                              "@NsConstr"
                              "FileOperationRecord"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "program_slicing_selection.h:17:28",
                                fieldName = NamePair {
                                  nameC = Name "status",
                                  nameHsIdent = Identifier
                                    "fileOperationRecord_status"},
                                fieldComment = Nothing},
                              structFieldType = TypeEnum
                                NamePair {
                                  nameC = Name
                                    "FileOperationStatus",
                                  nameHsIdent = Identifier
                                    "FileOperationStatus"}
                                NameOriginInSource,
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "program_slicing_selection.h:18:10",
                                fieldName = NamePair {
                                  nameC = Name "bytes_processed",
                                  nameHsIdent = Identifier
                                    "fileOperationRecord_bytes_processed"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "size_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtRef {
                                    extRefModule = ModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extRefIdentifier = Identifier
                                      "CSize"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (ModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (Identifier "CSize"),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "FileOperationRecord",
                      commentLocation = Just
                        "program_slicing_selection.h:16:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["program_slicing_selection.h"],
                          headerInclude =
                          "program_slicing_selection.h"},
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      8
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
        "FileOperationRecord",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "FileOperationRecord",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "read_file_chunk",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "file_ptr"),
          functionParameterType = HsPtr
            (HsExtBinding
              ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "CFile"}
              TypeSpec {
                typeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (Identifier "CFile"),
                typeSpecInstances = Map.fromList
                  []}),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "file_ptr",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "buffer"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "buffer",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "bytes_to_read"),
          functionParameterType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "CSize"}
            TypeSpec {
              typeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (Identifier "CSize"),
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
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "bytes_to_read",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "FileOperationStatus"))),
      foreignImportOrigName =
      "hs_bindgen_test_program_slicing_selection_13b0ed81415a625a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "enum FileOperationStatus hs_bindgen_test_program_slicing_selection_13b0ed81415a625a (FILE *arg1, void *arg2, size_t arg3) { return read_file_chunk(arg1, arg2, arg3); }",
          capiWrapperImport =
          "program_slicing_selection.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "file_ptr",
                  nameHsIdent = Identifier
                    "file_ptr"})
              (TypePointer
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "FILE",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtRef {
                      extRefModule = ModuleName
                        "HsBindgen.Runtime.Prelude",
                      extRefIdentifier = Identifier
                        "CFile"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (ModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (Identifier "CFile"),
                      typeSpecInstances = Map.fromList
                        []}})),
            _×_
              (Just
                NamePair {
                  nameC = Name "buffer",
                  nameHsIdent = Identifier
                    "buffer"})
              (TypePointer TypeVoid),
            _×_
              (Just
                NamePair {
                  nameC = Name "bytes_to_read",
                  nameHsIdent = Identifier
                    "bytes_to_read"})
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "size_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "CSize"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (Identifier "CSize"),
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
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeEnum
            NamePair {
              nameC = Name
                "FileOperationStatus",
              nameHsIdent = Identifier
                "FileOperationStatus"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "read_file_chunk",
          commentLocation = Just
            "program_slicing_selection.h:21:26",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "read_file_chunk",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "file_ptr"),
          functionParameterType = HsPtr
            (HsExtBinding
              ExtRef {
                extRefModule = ModuleName
                  "HsBindgen.Runtime.Prelude",
                extRefIdentifier = Identifier
                  "CFile"}
              TypeSpec {
                typeSpecModule = Just
                  (ModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (Identifier "CFile"),
                typeSpecInstances = Map.fromList
                  []}),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "file_ptr",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "buffer"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "buffer",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "bytes_to_read"),
          functionParameterType =
          HsExtBinding
            ExtRef {
              extRefModule = ModuleName
                "HsBindgen.Runtime.Prelude",
              extRefIdentifier = Identifier
                "CSize"}
            TypeSpec {
              typeSpecModule = Just
                (ModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (Identifier "CSize"),
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
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "bytes_to_read",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "FileOperationStatus"))),
      foreignImportOrigName =
      "hs_bindgen_test_program_slicing_selection_3ade0bc94fb1ed45",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "enum FileOperationStatus hs_bindgen_test_program_slicing_selection_3ade0bc94fb1ed45 (FILE *arg1, void *arg2, size_t arg3) { return read_file_chunk(arg1, arg2, arg3); }",
          capiWrapperImport =
          "program_slicing_selection.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "file_ptr",
                  nameHsIdent = Identifier
                    "file_ptr"})
              (TypePointer
                (TypeExtBinding
                  ResolvedExtBinding {
                    extCName = QualName {
                      qualNameName = Name "FILE",
                      qualNameKind =
                      NameKindOrdinary},
                    extHsRef = ExtRef {
                      extRefModule = ModuleName
                        "HsBindgen.Runtime.Prelude",
                      extRefIdentifier = Identifier
                        "CFile"},
                    extHsSpec = TypeSpec {
                      typeSpecModule = Just
                        (ModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (Identifier "CFile"),
                      typeSpecInstances = Map.fromList
                        []}})),
            _×_
              (Just
                NamePair {
                  nameC = Name "buffer",
                  nameHsIdent = Identifier
                    "buffer"})
              (TypePointer TypeVoid),
            _×_
              (Just
                NamePair {
                  nameC = Name "bytes_to_read",
                  nameHsIdent = Identifier
                    "bytes_to_read"})
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "size_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "CSize"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (Identifier "CSize"),
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
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeEnum
            NamePair {
              nameC = Name
                "FileOperationStatus",
              nameHsIdent = Identifier
                "FileOperationStatus"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "read_file_chunk",
          commentLocation = Just
            "program_slicing_selection.h:21:26",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["program_slicing_selection.h"],
              headerInclude =
              "program_slicing_selection.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_program_slicing_selection_cc45351e6b02b3b4",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsExtBinding
                  ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "CFile"}
                  TypeSpec {
                    typeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (Identifier "CFile"),
                    typeSpecInstances = Map.fromList
                      []}))
              (HsFun
                (HsPtr (HsPrimType HsPrimVoid))
                (HsFun
                  (HsExtBinding
                    ExtRef {
                      extRefModule = ModuleName
                        "HsBindgen.Runtime.Prelude",
                      extRefIdentifier = Identifier
                        "CSize"}
                    TypeSpec {
                      typeSpecModule = Just
                        (ModuleName
                          "HsBindgen.Runtime.Prelude"),
                      typeSpecIdentifier = Just
                        (Identifier "CSize"),
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
                      (Name
                        "@NsTypeConstr"
                        "FileOperationStatus")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_program_slicing_selection_cc45351e6b02b3b4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_read_file_chunk_ptr */ __attribute__ ((const)) enum FileOperationStatus (*hs_bindgen_test_program_slicing_selection_cc45351e6b02b3b4 (void)) (FILE *arg1, void *arg2, size_t arg3) { return &read_file_chunk; } ",
          capiWrapperImport =
          "program_slicing_selection.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "FILE",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtRef {
                    extRefModule = ModuleName
                      "HsBindgen.Runtime.Prelude",
                    extRefIdentifier = Identifier
                      "CFile"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (ModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (Identifier "CFile"),
                    typeSpecInstances = Map.fromList
                      []}}),
            TypePointer TypeVoid,
            TypeExtBinding
              ResolvedExtBinding {
                extCName = QualName {
                  qualNameName = Name "size_t",
                  qualNameKind =
                  NameKindOrdinary},
                extHsRef = ExtRef {
                  extRefModule = ModuleName
                    "HsBindgen.Runtime.Prelude",
                  extRefIdentifier = Identifier
                    "CSize"},
                extHsSpec = TypeSpec {
                  typeSpecModule = Just
                    (ModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (Identifier "CSize"),
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
          (TypeEnum
            NamePair {
              nameC = Name
                "FileOperationStatus",
              nameHsIdent = Identifier
                "FileOperationStatus"}
            NameOriginInSource)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
