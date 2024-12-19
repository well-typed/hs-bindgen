DeclNewtype
  Newtype {
    newtypeName = HsName "@NsTypeConstr" "Foo",
    newtypeConstr = HsName "@NsConstr" "Foo",
    newtypeField =
    Field {
      fieldName = HsName "@NsVar" "unFoo",
      fieldType = HsPrimType HsPrimCUInt,
      fieldOrigin = FieldOriginNone},
    newtypeOrigin =
    NewtypeOriginEnum
      Enu {
        enumTag = CName "foo",
        enumType = TypePrim (PrimIntegral (PrimInt Unsigned)),
        enumSizeof = 4,
        enumAlignment = 4,
        enumValues =
        [ EnumValue {
            valueName = CName "FOO1",
            valueValue = 0,
            valueSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "typenames.h"],
              singleLocLine = 15,
              singleLocColumn = 2}},
          EnumValue {
            valueName = CName "FOO2",
            valueValue = 1,
            valueSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "typenames.h"],
              singleLocLine = 16,
              singleLocColumn = 2}}],
        enumSourceLoc =
        SingleLoc {
          singleLocPath = ["examples", "typenames.h"],
          singleLocLine = 14,
          singleLocColumn = 6}}}
DeclInstance
  (InstanceStorable
     Struct {
       structName = HsName "@NsTypeConstr" "Foo",
       structConstr = HsName "@NsConstr" "Foo",
       structFields =
       [ Field {
           fieldName = HsName "@NsVar" "unFoo",
           fieldType = HsPrimType HsPrimCUInt,
           fieldOrigin = FieldOriginNone}],
       structOrigin =
       StructOriginEnum
         Enu {
           enumTag = CName "foo",
           enumType = TypePrim (PrimIntegral (PrimInt Unsigned)),
           enumSizeof = 4,
           enumAlignment = 4,
           enumValues =
           [ EnumValue {
               valueName = CName "FOO1",
               valueValue = 0,
               valueSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "typenames.h"],
                 singleLocLine = 15,
                 singleLocColumn = 2}},
             EnumValue {
               valueName = CName "FOO2",
               valueValue = 1,
               valueSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "typenames.h"],
                 singleLocLine = 16,
                 singleLocColumn = 2}}],
           enumSourceLoc =
           SingleLoc {
             singleLocPath = ["examples", "typenames.h"],
             singleLocLine = 14,
             singleLocColumn = 6}}}
     StorableInstance {
       storableSizeOf = 4,
       storableAlignment = 4,
       storablePeek =
       Lambda
         (NameHint "ptr")
         (Ap
            (StructCon
               Struct {
                 structName = HsName "@NsTypeConstr" "Foo",
                 structConstr = HsName "@NsConstr" "Foo",
                 structFields =
                 [ Field {
                     fieldName = HsName "@NsVar" "unFoo",
                     fieldType = HsPrimType HsPrimCUInt,
                     fieldOrigin = FieldOriginNone}],
                 structOrigin =
                 StructOriginEnum
                   Enu {
                     enumTag = CName "foo",
                     enumType = TypePrim (PrimIntegral (PrimInt Unsigned)),
                     enumSizeof = 4,
                     enumAlignment = 4,
                     enumValues =
                     [ EnumValue {
                         valueName = CName "FOO1",
                         valueValue = 0,
                         valueSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "typenames.h"],
                           singleLocLine = 15,
                           singleLocColumn = 2}},
                       EnumValue {
                         valueName = CName "FOO2",
                         valueValue = 1,
                         valueSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "typenames.h"],
                           singleLocLine = 16,
                           singleLocColumn = 2}}],
                     enumSourceLoc =
                     SingleLoc {
                       singleLocPath = ["examples", "typenames.h"],
                       singleLocLine = 14,
                       singleLocColumn = 6}}})
            [PeekByteOff (Idx 0) 0]),
       storablePoke =
       Lambda
         (NameHint "ptr")
         (Lambda
            (NameHint "s")
            (ElimStruct
               (Idx 0)
               Struct {
                 structName = HsName "@NsTypeConstr" "Foo",
                 structConstr = HsName "@NsConstr" "Foo",
                 structFields =
                 [ Field {
                     fieldName = HsName "@NsVar" "unFoo",
                     fieldType = HsPrimType HsPrimCUInt,
                     fieldOrigin = FieldOriginNone}],
                 structOrigin =
                 StructOriginEnum
                   Enu {
                     enumTag = CName "foo",
                     enumType = TypePrim (PrimIntegral (PrimInt Unsigned)),
                     enumSizeof = 4,
                     enumAlignment = 4,
                     enumValues =
                     [ EnumValue {
                         valueName = CName "FOO1",
                         valueValue = 0,
                         valueSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "typenames.h"],
                           singleLocLine = 15,
                           singleLocColumn = 2}},
                       EnumValue {
                         valueName = CName "FOO2",
                         valueValue = 1,
                         valueSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "typenames.h"],
                           singleLocLine = 16,
                           singleLocColumn = 2}}],
                     enumSourceLoc =
                     SingleLoc {
                       singleLocPath = ["examples", "typenames.h"],
                       singleLocLine = 14,
                       singleLocColumn = 6}}}
               (Add 1)
               (Seq [PokeByteOff (Idx 2) 0 (Idx 0)])))})
DeclPatSyn
  PatSyn {
    patSynName = HsName "@NsConstr" "FOO1",
    patSynType = HsName "@NsTypeConstr" "Foo",
    patSynConstr = HsName "@NsConstr" "Foo",
    patSynValue = 0,
    patSynOrigin =
    PatSynOriginEnumValue
      EnumValue {
        valueName = CName "FOO1",
        valueValue = 0,
        valueSourceLoc =
        SingleLoc {
          singleLocPath = ["examples", "typenames.h"],
          singleLocLine = 15,
          singleLocColumn = 2}}}
DeclPatSyn
  PatSyn {
    patSynName = HsName "@NsConstr" "FOO2",
    patSynType = HsName "@NsTypeConstr" "Foo",
    patSynConstr = HsName "@NsConstr" "Foo",
    patSynValue = 1,
    patSynOrigin =
    PatSynOriginEnumValue
      EnumValue {
        valueName = CName "FOO2",
        valueValue = 1,
        valueSourceLoc =
        SingleLoc {
          singleLocPath = ["examples", "typenames.h"],
          singleLocLine = 16,
          singleLocColumn = 2}}}
DeclNewtype
  Newtype {
    newtypeName = HsName "@NsTypeConstr" "Foo",
    newtypeConstr = HsName "@NsConstr" "Foo",
    newtypeField =
    Field {
      fieldName = HsName "@NsVar" "unFoo",
      fieldType = HsPrimType HsPrimCDouble,
      fieldOrigin = FieldOriginNone},
    newtypeOrigin =
    NewtypeOriginTypedef
      Typedef {
        typedefName = CName "foo",
        typedefType = TypePrim (PrimFloating PrimDouble),
        typedefSourceLoc =
        SingleLoc {
          singleLocPath = ["examples", "typenames.h"],
          singleLocLine = 19,
          singleLocColumn = 16}}}
DeclNewtypeInstance Storable (HsName "@NsTypeConstr" "Foo")
