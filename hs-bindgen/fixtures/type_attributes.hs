[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S",
      structConstr = HsName
        "@NsConstr"
        "S",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s_f",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCShort),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "type_attributes.h:8:18",
                fieldName = NamePair {
                  nameC = Name "f",
                  nameHsIdent = HsIdentifier
                    "s_f"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimShort
                    Signed)),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "f",
              commentLocation = Just
                "type_attributes.h:8:18",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "type_attributes.h:8:8",
            declId = NamePair {
              nameC = Name "S",
              nameHsIdent = HsIdentifier "S"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "type_attributes.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:8:18",
                    fieldName = NamePair {
                      nameC = Name "f",
                      nameHsIdent = HsIdentifier
                        "s_f"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimShort
                        Signed)),
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
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "S",
          commentLocation = Just
            "type_attributes.h:8:8",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "S",
          structConstr = HsName
            "@NsConstr"
            "S",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "s_f",
              fieldType = HsConstArray
                3
                (HsPrimType HsPrimCShort),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:8:18",
                    fieldName = NamePair {
                      nameC = Name "f",
                      nameHsIdent = HsIdentifier
                        "s_f"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimShort
                        Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "f",
                  commentLocation = Just
                    "type_attributes.h:8:18",
                  commentHeader = Just
                    "type_attributes.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "type_attributes.h:8:8",
                declId = NamePair {
                  nameC = Name "S",
                  nameHsIdent = HsIdentifier "S"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "type_attributes.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "S"),
                  structSizeof = 8,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "type_attributes.h:8:18",
                        fieldName = NamePair {
                          nameC = Name "f",
                          nameHsIdent = HsIdentifier
                            "s_f"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimShort
                            Signed)),
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
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "S",
              commentLocation = Just
                "type_attributes.h:8:8",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "S",
                  structConstr = HsName
                    "@NsConstr"
                    "S",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "s_f",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCShort),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:8:18",
                            fieldName = NamePair {
                              nameC = Name "f",
                              nameHsIdent = HsIdentifier
                                "s_f"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimShort
                                Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "f",
                          commentLocation = Just
                            "type_attributes.h:8:18",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "type_attributes.h:8:8",
                        declId = NamePair {
                          nameC = Name "S",
                          nameHsIdent = HsIdentifier "S"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "type_attributes.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "S"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:8:18",
                                fieldName = NamePair {
                                  nameC = Name "f",
                                  nameHsIdent = HsIdentifier
                                    "s_f"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral
                                    PrimShort
                                    Signed)),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "S",
                      commentLocation = Just
                        "type_attributes.h:8:8",
                      commentHeader = Just
                        "type_attributes.h",
                      commentChildren = []}})
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
                    "S",
                  structConstr = HsName
                    "@NsConstr"
                    "S",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "s_f",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCShort),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:8:18",
                            fieldName = NamePair {
                              nameC = Name "f",
                              nameHsIdent = HsIdentifier
                                "s_f"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimShort
                                Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "f",
                          commentLocation = Just
                            "type_attributes.h:8:18",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "type_attributes.h:8:8",
                        declId = NamePair {
                          nameC = Name "S",
                          nameHsIdent = HsIdentifier "S"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "type_attributes.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "S"),
                          structSizeof = 8,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:8:18",
                                fieldName = NamePair {
                                  nameC = Name "f",
                                  nameHsIdent = HsIdentifier
                                    "s_f"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral
                                    PrimShort
                                    Signed)),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "S",
                      commentLocation = Just
                        "type_attributes.h:8:8",
                      commentHeader = Just
                        "type_attributes.h",
                      commentChildren = []}}
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
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      newtypeConstr = HsName
        "@NsConstr"
        "More_aligned_int",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_More_aligned_int",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_attributes.h:9:13",
          declId = NamePair {
            nameC = Name "more_aligned_int",
            nameHsIdent = HsIdentifier
              "More_aligned_int"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "type_attributes.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "More_aligned_int",
              newtypeField = HsName
                "@NsVar"
                "un_More_aligned_int"},
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
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "more_aligned_int",
          commentLocation = Just
            "type_attributes.h:9:13",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "More_aligned_int",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S2",
      structConstr = HsName
        "@NsConstr"
        "S2",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_f",
          fieldType = HsConstArray
            3
            (HsPrimType HsPrimCShort),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "type_attributes.h:11:19",
                fieldName = NamePair {
                  nameC = Name "f",
                  nameHsIdent = HsIdentifier
                    "s2_f"},
                fieldComment = Nothing},
              structFieldType = TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimShort
                    Signed)),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "f",
              commentLocation = Just
                "type_attributes.h:11:19",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "type_attributes.h:11:8",
            declId = NamePair {
              nameC = Name "S2",
              nameHsIdent = HsIdentifier
                "S2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "type_attributes.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S2"),
              structSizeof = 16,
              structAlignment = 16,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:11:19",
                    fieldName = NamePair {
                      nameC = Name "f",
                      nameHsIdent = HsIdentifier
                        "s2_f"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimShort
                        Signed)),
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
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "S2",
          commentLocation = Just
            "type_attributes.h:11:8",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "S2",
          structConstr = HsName
            "@NsConstr"
            "S2",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "s2_f",
              fieldType = HsConstArray
                3
                (HsPrimType HsPrimCShort),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:11:19",
                    fieldName = NamePair {
                      nameC = Name "f",
                      nameHsIdent = HsIdentifier
                        "s2_f"},
                    fieldComment = Nothing},
                  structFieldType = TypeConstArray
                    3
                    (TypePrim
                      (PrimIntegral
                        PrimShort
                        Signed)),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "f",
                  commentLocation = Just
                    "type_attributes.h:11:19",
                  commentHeader = Just
                    "type_attributes.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "type_attributes.h:11:8",
                declId = NamePair {
                  nameC = Name "S2",
                  nameHsIdent = HsIdentifier
                    "S2"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "type_attributes.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "S2"),
                  structSizeof = 16,
                  structAlignment = 16,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "type_attributes.h:11:19",
                        fieldName = NamePair {
                          nameC = Name "f",
                          nameHsIdent = HsIdentifier
                            "s2_f"},
                        fieldComment = Nothing},
                      structFieldType = TypeConstArray
                        3
                        (TypePrim
                          (PrimIntegral
                            PrimShort
                            Signed)),
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
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "S2",
              commentLocation = Just
                "type_attributes.h:11:8",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 16,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "S2",
                  structConstr = HsName
                    "@NsConstr"
                    "S2",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "s2_f",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCShort),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:11:19",
                            fieldName = NamePair {
                              nameC = Name "f",
                              nameHsIdent = HsIdentifier
                                "s2_f"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimShort
                                Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "f",
                          commentLocation = Just
                            "type_attributes.h:11:19",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "type_attributes.h:11:8",
                        declId = NamePair {
                          nameC = Name "S2",
                          nameHsIdent = HsIdentifier
                            "S2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "type_attributes.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "S2"),
                          structSizeof = 16,
                          structAlignment = 16,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:11:19",
                                fieldName = NamePair {
                                  nameC = Name "f",
                                  nameHsIdent = HsIdentifier
                                    "s2_f"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral
                                    PrimShort
                                    Signed)),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "S2",
                      commentLocation = Just
                        "type_attributes.h:11:8",
                      commentHeader = Just
                        "type_attributes.h",
                      commentChildren = []}})
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
                    "S2",
                  structConstr = HsName
                    "@NsConstr"
                    "S2",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "s2_f",
                      fieldType = HsConstArray
                        3
                        (HsPrimType HsPrimCShort),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:11:19",
                            fieldName = NamePair {
                              nameC = Name "f",
                              nameHsIdent = HsIdentifier
                                "s2_f"},
                            fieldComment = Nothing},
                          structFieldType = TypeConstArray
                            3
                            (TypePrim
                              (PrimIntegral
                                PrimShort
                                Signed)),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "f",
                          commentLocation = Just
                            "type_attributes.h:11:19",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "type_attributes.h:11:8",
                        declId = NamePair {
                          nameC = Name "S2",
                          nameHsIdent = HsIdentifier
                            "S2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "type_attributes.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "S2"),
                          structSizeof = 16,
                          structAlignment = 16,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:11:19",
                                fieldName = NamePair {
                                  nameC = Name "f",
                                  nameHsIdent = HsIdentifier
                                    "s2_f"},
                                fieldComment = Nothing},
                              structFieldType = TypeConstArray
                                3
                                (TypePrim
                                  (PrimIntegral
                                    PrimShort
                                    Signed)),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "S2",
                      commentLocation = Just
                        "type_attributes.h:11:8",
                      commentHeader = Just
                        "type_attributes.h",
                      commentChildren = []}}
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
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "S2",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "My_unpacked_struct",
      structConstr = HsName
        "@NsConstr"
        "My_unpacked_struct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "my_unpacked_struct_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "type_attributes.h:15:8",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = HsIdentifier
                    "my_unpacked_struct_c"},
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
              commentOrigin = Just "c",
              commentLocation = Just
                "type_attributes.h:15:8",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "my_unpacked_struct_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "type_attributes.h:16:7",
                fieldName = NamePair {
                  nameC = Name "i",
                  nameHsIdent = HsIdentifier
                    "my_unpacked_struct_i"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Just
                "type_attributes.h:16:7",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "type_attributes.h:13:8",
            declId = NamePair {
              nameC = Name
                "my_unpacked_struct",
              nameHsIdent = HsIdentifier
                "My_unpacked_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "type_attributes.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "My_unpacked_struct"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:15:8",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = HsIdentifier
                        "my_unpacked_struct_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:16:7",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = HsIdentifier
                        "my_unpacked_struct_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_unpacked_struct",
          commentLocation = Just
            "type_attributes.h:13:8",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "My_unpacked_struct",
          structConstr = HsName
            "@NsConstr"
            "My_unpacked_struct",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "my_unpacked_struct_c",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:15:8",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = HsIdentifier
                        "my_unpacked_struct_c"},
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
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "type_attributes.h:15:8",
                  commentHeader = Just
                    "type_attributes.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "my_unpacked_struct_i",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:16:7",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = HsIdentifier
                        "my_unpacked_struct_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "i",
                  commentLocation = Just
                    "type_attributes.h:16:7",
                  commentHeader = Just
                    "type_attributes.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "type_attributes.h:13:8",
                declId = NamePair {
                  nameC = Name
                    "my_unpacked_struct",
                  nameHsIdent = HsIdentifier
                    "My_unpacked_struct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "type_attributes.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "My_unpacked_struct"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "type_attributes.h:15:8",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = HsIdentifier
                            "my_unpacked_struct_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "type_attributes.h:16:7",
                        fieldName = NamePair {
                          nameC = Name "i",
                          nameHsIdent = HsIdentifier
                            "my_unpacked_struct_i"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
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
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "my_unpacked_struct",
              commentLocation = Just
                "type_attributes.h:13:8",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}}
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
                    "My_unpacked_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "My_unpacked_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "my_unpacked_struct_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:15:8",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = HsIdentifier
                                "my_unpacked_struct_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "type_attributes.h:15:8",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "my_unpacked_struct_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:16:7",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = HsIdentifier
                                "my_unpacked_struct_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "type_attributes.h:16:7",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "type_attributes.h:13:8",
                        declId = NamePair {
                          nameC = Name
                            "my_unpacked_struct",
                          nameHsIdent = HsIdentifier
                            "My_unpacked_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "type_attributes.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "My_unpacked_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:15:8",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = HsIdentifier
                                    "my_unpacked_struct_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:16:7",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = HsIdentifier
                                    "my_unpacked_struct_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "my_unpacked_struct",
                      commentLocation = Just
                        "type_attributes.h:13:8",
                      commentHeader = Just
                        "type_attributes.h",
                      commentChildren = []}})
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
                    "My_unpacked_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "My_unpacked_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "my_unpacked_struct_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:15:8",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = HsIdentifier
                                "my_unpacked_struct_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "type_attributes.h:15:8",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "my_unpacked_struct_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:16:7",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = HsIdentifier
                                "my_unpacked_struct_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "type_attributes.h:16:7",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "type_attributes.h:13:8",
                        declId = NamePair {
                          nameC = Name
                            "my_unpacked_struct",
                          nameHsIdent = HsIdentifier
                            "My_unpacked_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "type_attributes.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "My_unpacked_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:15:8",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = HsIdentifier
                                    "my_unpacked_struct_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:16:7",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = HsIdentifier
                                    "my_unpacked_struct_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
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
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just
                        "my_unpacked_struct",
                      commentLocation = Just
                        "type_attributes.h:13:8",
                      commentHeader = Just
                        "type_attributes.h",
                      commentChildren = []}}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
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
        "My_unpacked_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "My_unpacked_struct",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "My_packed_struct",
      structConstr = HsName
        "@NsConstr"
        "My_packed_struct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "my_packed_struct_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "type_attributes.h:21:9",
                fieldName = NamePair {
                  nameC = Name "c",
                  nameHsIdent = HsIdentifier
                    "my_packed_struct_c"},
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
              commentOrigin = Just "c",
              commentLocation = Just
                "type_attributes.h:21:9",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "my_packed_struct_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "type_attributes.h:22:9",
                fieldName = NamePair {
                  nameC = Name "i",
                  nameHsIdent = HsIdentifier
                    "my_packed_struct_i"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "i",
              commentLocation = Just
                "type_attributes.h:22:9",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}},
        Field {
          fieldName = HsName
            "@NsVar"
            "my_packed_struct_s",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "My_unpacked_struct"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "type_attributes.h:23:30",
                fieldName = NamePair {
                  nameC = Name "s",
                  nameHsIdent = HsIdentifier
                    "my_packed_struct_s"},
                fieldComment = Nothing},
              structFieldType = TypeStruct
                NamePair {
                  nameC = Name
                    "my_unpacked_struct",
                  nameHsIdent = HsIdentifier
                    "My_unpacked_struct"}
                NameOriginInSource,
              structFieldOffset = 40,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "s",
              commentLocation = Just
                "type_attributes.h:23:30",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "type_attributes.h:19:37",
            declId = NamePair {
              nameC = Name "my_packed_struct",
              nameHsIdent = HsIdentifier
                "My_packed_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader =
            "type_attributes.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "My_packed_struct"),
              structSizeof = 13,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:21:9",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = HsIdentifier
                        "my_packed_struct_c"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:22:9",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = HsIdentifier
                        "my_packed_struct_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:23:30",
                    fieldName = NamePair {
                      nameC = Name "s",
                      nameHsIdent = HsIdentifier
                        "my_packed_struct_s"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name
                        "my_unpacked_struct",
                      nameHsIdent = HsIdentifier
                        "My_unpacked_struct"}
                    NameOriginInSource,
                  structFieldOffset = 40,
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
            "my_packed_struct",
          commentLocation = Just
            "type_attributes.h:19:37",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "My_packed_struct",
          structConstr = HsName
            "@NsConstr"
            "My_packed_struct",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "my_packed_struct_c",
              fieldType = HsPrimType
                HsPrimCChar,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:21:9",
                    fieldName = NamePair {
                      nameC = Name "c",
                      nameHsIdent = HsIdentifier
                        "my_packed_struct_c"},
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
                  commentOrigin = Just "c",
                  commentLocation = Just
                    "type_attributes.h:21:9",
                  commentHeader = Just
                    "type_attributes.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "my_packed_struct_i",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:22:9",
                    fieldName = NamePair {
                      nameC = Name "i",
                      nameHsIdent = HsIdentifier
                        "my_packed_struct_i"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "i",
                  commentLocation = Just
                    "type_attributes.h:22:9",
                  commentHeader = Just
                    "type_attributes.h",
                  commentChildren = []}},
            Field {
              fieldName = HsName
                "@NsVar"
                "my_packed_struct_s",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "My_unpacked_struct"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "type_attributes.h:23:30",
                    fieldName = NamePair {
                      nameC = Name "s",
                      nameHsIdent = HsIdentifier
                        "my_packed_struct_s"},
                    fieldComment = Nothing},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = Name
                        "my_unpacked_struct",
                      nameHsIdent = HsIdentifier
                        "My_unpacked_struct"}
                    NameOriginInSource,
                  structFieldOffset = 40,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "s",
                  commentLocation = Just
                    "type_attributes.h:23:30",
                  commentHeader = Just
                    "type_attributes.h",
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "type_attributes.h:19:37",
                declId = NamePair {
                  nameC = Name "my_packed_struct",
                  nameHsIdent = HsIdentifier
                    "My_packed_struct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader =
                "type_attributes.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "My_packed_struct"),
                  structSizeof = 13,
                  structAlignment = 1,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "type_attributes.h:21:9",
                        fieldName = NamePair {
                          nameC = Name "c",
                          nameHsIdent = HsIdentifier
                            "my_packed_struct_c"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimChar
                          (PrimSignImplicit
                            (Just Signed))),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "type_attributes.h:22:9",
                        fieldName = NamePair {
                          nameC = Name "i",
                          nameHsIdent = HsIdentifier
                            "my_packed_struct_i"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 8,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "type_attributes.h:23:30",
                        fieldName = NamePair {
                          nameC = Name "s",
                          nameHsIdent = HsIdentifier
                            "my_packed_struct_s"},
                        fieldComment = Nothing},
                      structFieldType = TypeStruct
                        NamePair {
                          nameC = Name
                            "my_unpacked_struct",
                          nameHsIdent = HsIdentifier
                            "My_unpacked_struct"}
                        NameOriginInSource,
                      structFieldOffset = 40,
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
                "my_packed_struct",
              commentLocation = Just
                "type_attributes.h:19:37",
              commentHeader = Just
                "type_attributes.h",
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 13,
          storableAlignment = 1,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "My_packed_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "My_packed_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "my_packed_struct_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:21:9",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "type_attributes.h:21:9",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "my_packed_struct_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:22:9",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "type_attributes.h:22:9",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "my_packed_struct_s",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "My_unpacked_struct"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:23:30",
                            fieldName = NamePair {
                              nameC = Name "s",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_s"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name
                                "my_unpacked_struct",
                              nameHsIdent = HsIdentifier
                                "My_unpacked_struct"}
                            NameOriginInSource,
                          structFieldOffset = 40,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "s",
                          commentLocation = Just
                            "type_attributes.h:23:30",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "type_attributes.h:19:37",
                        declId = NamePair {
                          nameC = Name "my_packed_struct",
                          nameHsIdent = HsIdentifier
                            "My_packed_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "type_attributes.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "My_packed_struct"),
                          structSizeof = 13,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:21:9",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = HsIdentifier
                                    "my_packed_struct_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:22:9",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = HsIdentifier
                                    "my_packed_struct_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:23:30",
                                fieldName = NamePair {
                                  nameC = Name "s",
                                  nameHsIdent = HsIdentifier
                                    "my_packed_struct_s"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name
                                    "my_unpacked_struct",
                                  nameHsIdent = HsIdentifier
                                    "My_unpacked_struct"}
                                NameOriginInSource,
                              structFieldOffset = 40,
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
                        "my_packed_struct",
                      commentLocation = Just
                        "type_attributes.h:19:37",
                      commentHeader = Just
                        "type_attributes.h",
                      commentChildren = []}})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 1,
                PeekByteOff (Idx 0) 5]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "My_packed_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "My_packed_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "my_packed_struct_c",
                      fieldType = HsPrimType
                        HsPrimCChar,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:21:9",
                            fieldName = NamePair {
                              nameC = Name "c",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_c"},
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
                          commentOrigin = Just "c",
                          commentLocation = Just
                            "type_attributes.h:21:9",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "my_packed_struct_i",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:22:9",
                            fieldName = NamePair {
                              nameC = Name "i",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_i"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 8,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "i",
                          commentLocation = Just
                            "type_attributes.h:22:9",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "my_packed_struct_s",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "My_unpacked_struct"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "type_attributes.h:23:30",
                            fieldName = NamePair {
                              nameC = Name "s",
                              nameHsIdent = HsIdentifier
                                "my_packed_struct_s"},
                            fieldComment = Nothing},
                          structFieldType = TypeStruct
                            NamePair {
                              nameC = Name
                                "my_unpacked_struct",
                              nameHsIdent = HsIdentifier
                                "My_unpacked_struct"}
                            NameOriginInSource,
                          structFieldOffset = 40,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "s",
                          commentLocation = Just
                            "type_attributes.h:23:30",
                          commentHeader = Just
                            "type_attributes.h",
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "type_attributes.h:19:37",
                        declId = NamePair {
                          nameC = Name "my_packed_struct",
                          nameHsIdent = HsIdentifier
                            "My_packed_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader =
                        "type_attributes.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "My_packed_struct"),
                          structSizeof = 13,
                          structAlignment = 1,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:21:9",
                                fieldName = NamePair {
                                  nameC = Name "c",
                                  nameHsIdent = HsIdentifier
                                    "my_packed_struct_c"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimChar
                                  (PrimSignImplicit
                                    (Just Signed))),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:22:9",
                                fieldName = NamePair {
                                  nameC = Name "i",
                                  nameHsIdent = HsIdentifier
                                    "my_packed_struct_i"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 8,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "type_attributes.h:23:30",
                                fieldName = NamePair {
                                  nameC = Name "s",
                                  nameHsIdent = HsIdentifier
                                    "my_packed_struct_s"},
                                fieldComment = Nothing},
                              structFieldType = TypeStruct
                                NamePair {
                                  nameC = Name
                                    "my_unpacked_struct",
                                  nameHsIdent = HsIdentifier
                                    "My_unpacked_struct"}
                                NameOriginInSource,
                              structFieldOffset = 40,
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
                        "my_packed_struct",
                      commentLocation = Just
                        "type_attributes.h:19:37",
                      commentHeader = Just
                        "type_attributes.h",
                      commentChildren = []}}
                (Add 3)
                (Seq
                  [
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 1 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      5
                      (Idx 2)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "My_packed_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "My_packed_struct",
      deriveInstanceComment =
      Nothing},
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Wait",
      emptyDataOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_attributes.h:29:9",
          declId = NamePair {
            nameC = Name "wait",
            nameHsIdent = HsIdentifier
              "Wait"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "type_attributes.h",
          declComment = Nothing},
        declKind = OpaqueUnion,
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      emptyDataComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "wait",
          commentLocation = Just
            "type_attributes.h:29:9",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = []}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Wait_status_ptr_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Wait_status_ptr_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Wait_status_ptr_t",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_attributes.h:26:9",
          declId = NamePair {
            nameC = Name
              "wait_status_ptr_t",
            nameHsIdent = HsIdentifier
              "Wait_status_ptr_t"},
          declOrigin = NameOriginGenerated
            (AnonId
              "type_attributes.h:26:9"),
          declAliases = [
            Name "wait_status_ptr_t"],
          declHeader =
          "type_attributes.h",
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Wait_status_ptr_t",
              newtypeField = HsName
                "@NsVar"
                "un_Wait_status_ptr_t"},
            unionSizeof = 8,
            unionAlignment = 8,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "type_attributes.h:28:8",
                  fieldName = NamePair {
                    nameC = Name "__ip",
                    nameHsIdent = HsIdentifier
                      "wait_status_ptr_t___ip"},
                  fieldComment = Nothing},
                unionFieldType = TypePointer
                  (TypePrim
                    (PrimIntegral PrimInt Signed))},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "type_attributes.h:29:15",
                  fieldName = NamePair {
                    nameC = Name "__up",
                    nameHsIdent = HsIdentifier
                      "wait_status_ptr_t___up"},
                  fieldComment = Nothing},
                unionFieldType = TypePointer
                  (TypeUnion
                    NamePair {
                      nameC = Name "wait",
                      nameHsIdent = HsIdentifier
                        "Wait"}
                    NameOriginInSource)}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "wait_status_ptr_t",
          commentLocation = Just
            "type_attributes.h:26:9",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 8 8),
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Wait_status_ptr_t",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_wait_status_ptr_t___ip",
      unionGetterType = HsPtr
        (HsPrimType HsPrimCInt),
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Wait_status_ptr_t",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "__ip",
          commentLocation = Just
            "type_attributes.h:28:8",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_wait_status_ptr_t___ip"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_wait_status_ptr_t___ip",
      unionSetterType = HsPtr
        (HsPrimType HsPrimCInt),
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Wait_status_ptr_t",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_wait_status_ptr_t___ip"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_wait_status_ptr_t___up",
      unionGetterType = HsPtr
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Wait")),
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Wait_status_ptr_t",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "__up",
          commentLocation = Just
            "type_attributes.h:29:15",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_wait_status_ptr_t___up"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_wait_status_ptr_t___up",
      unionSetterType = HsPtr
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Wait")),
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Wait_status_ptr_t",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeader = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_wait_status_ptr_t___up"]]}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "T1",
      newtypeConstr = HsName
        "@NsConstr"
        "T1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_T1",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_attributes.h:32:13",
          declId = NamePair {
            nameC = Name "T1",
            nameHsIdent = HsIdentifier
              "T1"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "type_attributes.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "T1",
              newtypeField = HsName
                "@NsVar"
                "un_T1"},
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
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "T1",
          commentLocation = Just
            "type_attributes.h:32:13",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "T1",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Short_a",
      newtypeConstr = HsName
        "@NsConstr"
        "Short_a",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Short_a",
        fieldType = HsPrimType
          HsPrimCShort,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "type_attributes.h:34:46",
          declId = NamePair {
            nameC = Name "short_a",
            nameHsIdent = HsIdentifier
              "Short_a"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "type_attributes.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Short_a",
              newtypeField = HsName
                "@NsVar"
                "un_Short_a"},
            typedefType = TypePrim
              (PrimIntegral
                PrimShort
                Signed)},
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
          Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "short_a",
          commentLocation = Just
            "type_attributes.h:34:46",
          commentHeader = Just
            "type_attributes.h",
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Short_a",
      deriveInstanceComment =
      Nothing}]
