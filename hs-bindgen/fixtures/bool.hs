DeclNewtype
  Newtype {
    newtypeName = HsName "@NsTypeConstr" "BOOL",
    newtypeConstr = HsName "@NsConstr" "BOOL",
    newtypeField =
    Field {
      fieldName = HsName "@NsVar" "unBOOL",
      fieldType = HsPrimType HsPrimCBool,
      fieldOrigin = FieldOriginNone},
    newtypeOrigin =
    NewtypeOriginMacro
      Macro {
        macroLoc =
        MultiLoc {
          multiLocExpansion =
          SingleLoc {
            singleLocPath = ["examples", "bool.h"],
            singleLocLine = 13,
            singleLocColumn = 9},
          multiLocPresumed = Nothing,
          multiLocSpelling = Nothing,
          multiLocFile = Nothing},
        macroName = CName "BOOL",
        macroArgs = [],
        macroBody = MTerm (MType PrimBool)}}
DeclData
  Struct {
    structName = HsName "@NsTypeConstr" "Bools1",
    structConstr = HsName "@NsConstr" "Bools1",
    structFields =
    [ Field {
        fieldName = HsName "@NsVar" "bools1_x",
        fieldType = HsPrimType HsPrimCBool,
        fieldOrigin =
        FieldOriginStructField
          StructField {
            fieldName = CName "x",
            fieldOffset = 0,
            fieldType = TypePrim PrimBool,
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 2,
              singleLocColumn = 11}}},
      Field {
        fieldName = HsName "@NsVar" "bools1_y",
        fieldType = HsPrimType HsPrimCBool,
        fieldOrigin =
        FieldOriginStructField
          StructField {
            fieldName = CName "y",
            fieldOffset = 8,
            fieldType = TypePrim PrimBool,
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 3,
              singleLocColumn = 11}}}],
    structOrigin =
    StructOriginStruct
      Struct {
        structDeclPath =
        DeclPathStruct (DeclNameTag (CName "bools1")) DeclPathTop,
        structSizeof = 2,
        structAlignment = 1,
        structFields =
        [ StructField {
            fieldName = CName "x",
            fieldOffset = 0,
            fieldType = TypePrim PrimBool,
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 2,
              singleLocColumn = 11}},
          StructField {
            fieldName = CName "y",
            fieldOffset = 8,
            fieldType = TypePrim PrimBool,
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 3,
              singleLocColumn = 11}}],
        structSourceLoc =
        SingleLoc {
          singleLocPath = ["examples", "bool.h"],
          singleLocLine = 1,
          singleLocColumn = 8}}}
DeclInstance
  (InstanceStorable
     Struct {
       structName = HsName "@NsTypeConstr" "Bools1",
       structConstr = HsName "@NsConstr" "Bools1",
       structFields =
       [ Field {
           fieldName = HsName "@NsVar" "bools1_x",
           fieldType = HsPrimType HsPrimCBool,
           fieldOrigin =
           FieldOriginStructField
             StructField {
               fieldName = CName "x",
               fieldOffset = 0,
               fieldType = TypePrim PrimBool,
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 2,
                 singleLocColumn = 11}}},
         Field {
           fieldName = HsName "@NsVar" "bools1_y",
           fieldType = HsPrimType HsPrimCBool,
           fieldOrigin =
           FieldOriginStructField
             StructField {
               fieldName = CName "y",
               fieldOffset = 8,
               fieldType = TypePrim PrimBool,
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 3,
                 singleLocColumn = 11}}}],
       structOrigin =
       StructOriginStruct
         Struct {
           structDeclPath =
           DeclPathStruct (DeclNameTag (CName "bools1")) DeclPathTop,
           structSizeof = 2,
           structAlignment = 1,
           structFields =
           [ StructField {
               fieldName = CName "x",
               fieldOffset = 0,
               fieldType = TypePrim PrimBool,
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 2,
                 singleLocColumn = 11}},
             StructField {
               fieldName = CName "y",
               fieldOffset = 8,
               fieldType = TypePrim PrimBool,
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 3,
                 singleLocColumn = 11}}],
           structSourceLoc =
           SingleLoc {
             singleLocPath = ["examples", "bool.h"],
             singleLocLine = 1,
             singleLocColumn = 8}}}
     StorableInstance {
       storableSizeOf = 2,
       storableAlignment = 1,
       storablePeek =
       Lambda
         (NameHint "ptr")
         (Ap
            (StructCon
               Struct {
                 structName = HsName "@NsTypeConstr" "Bools1",
                 structConstr = HsName "@NsConstr" "Bools1",
                 structFields =
                 [ Field {
                     fieldName = HsName "@NsVar" "bools1_x",
                     fieldType = HsPrimType HsPrimCBool,
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 2,
                           singleLocColumn = 11}}},
                   Field {
                     fieldName = HsName "@NsVar" "bools1_y",
                     fieldType = HsPrimType HsPrimCBool,
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 3,
                           singleLocColumn = 11}}}],
                 structOrigin =
                 StructOriginStruct
                   Struct {
                     structDeclPath =
                     DeclPathStruct (DeclNameTag (CName "bools1")) DeclPathTop,
                     structSizeof = 2,
                     structAlignment = 1,
                     structFields =
                     [ StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 2,
                           singleLocColumn = 11}},
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 3,
                           singleLocColumn = 11}}],
                     structSourceLoc =
                     SingleLoc {
                       singleLocPath = ["examples", "bool.h"],
                       singleLocLine = 1,
                       singleLocColumn = 8}}})
            [PeekByteOff (Idx 0) 0, PeekByteOff (Idx 0) 1]),
       storablePoke =
       Lambda
         (NameHint "ptr")
         (Lambda
            (NameHint "s")
            (ElimStruct
               (Idx 0)
               Struct {
                 structName = HsName "@NsTypeConstr" "Bools1",
                 structConstr = HsName "@NsConstr" "Bools1",
                 structFields =
                 [ Field {
                     fieldName = HsName "@NsVar" "bools1_x",
                     fieldType = HsPrimType HsPrimCBool,
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 2,
                           singleLocColumn = 11}}},
                   Field {
                     fieldName = HsName "@NsVar" "bools1_y",
                     fieldType = HsPrimType HsPrimCBool,
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 3,
                           singleLocColumn = 11}}}],
                 structOrigin =
                 StructOriginStruct
                   Struct {
                     structDeclPath =
                     DeclPathStruct (DeclNameTag (CName "bools1")) DeclPathTop,
                     structSizeof = 2,
                     structAlignment = 1,
                     structFields =
                     [ StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 2,
                           singleLocColumn = 11}},
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 3,
                           singleLocColumn = 11}}],
                     structSourceLoc =
                     SingleLoc {
                       singleLocPath = ["examples", "bool.h"],
                       singleLocLine = 1,
                       singleLocColumn = 8}}}
               (Add 2)
               (Seq
                  [PokeByteOff (Idx 3) 0 (Idx 0), PokeByteOff (Idx 3) 1 (Idx 1)])))})
DeclData
  Struct {
    structName = HsName "@NsTypeConstr" "Bools2",
    structConstr = HsName "@NsConstr" "Bools2",
    structFields =
    [ Field {
        fieldName = HsName "@NsVar" "bools2_x",
        fieldType = HsPrimType HsPrimCBool,
        fieldOrigin =
        FieldOriginStructField
          StructField {
            fieldName = CName "x",
            fieldOffset = 0,
            fieldType = TypePrim PrimBool,
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 9,
              singleLocColumn = 10}}},
      Field {
        fieldName = HsName "@NsVar" "bools2_y",
        fieldType = HsPrimType HsPrimCBool,
        fieldOrigin =
        FieldOriginStructField
          StructField {
            fieldName = CName "y",
            fieldOffset = 8,
            fieldType = TypePrim PrimBool,
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 10,
              singleLocColumn = 10}}}],
    structOrigin =
    StructOriginStruct
      Struct {
        structDeclPath =
        DeclPathStruct (DeclNameTag (CName "bools2")) DeclPathTop,
        structSizeof = 2,
        structAlignment = 1,
        structFields =
        [ StructField {
            fieldName = CName "x",
            fieldOffset = 0,
            fieldType = TypePrim PrimBool,
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 9,
              singleLocColumn = 10}},
          StructField {
            fieldName = CName "y",
            fieldOffset = 8,
            fieldType = TypePrim PrimBool,
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 10,
              singleLocColumn = 10}}],
        structSourceLoc =
        SingleLoc {
          singleLocPath = ["examples", "bool.h"],
          singleLocLine = 8,
          singleLocColumn = 8}}}
DeclInstance
  (InstanceStorable
     Struct {
       structName = HsName "@NsTypeConstr" "Bools2",
       structConstr = HsName "@NsConstr" "Bools2",
       structFields =
       [ Field {
           fieldName = HsName "@NsVar" "bools2_x",
           fieldType = HsPrimType HsPrimCBool,
           fieldOrigin =
           FieldOriginStructField
             StructField {
               fieldName = CName "x",
               fieldOffset = 0,
               fieldType = TypePrim PrimBool,
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 9,
                 singleLocColumn = 10}}},
         Field {
           fieldName = HsName "@NsVar" "bools2_y",
           fieldType = HsPrimType HsPrimCBool,
           fieldOrigin =
           FieldOriginStructField
             StructField {
               fieldName = CName "y",
               fieldOffset = 8,
               fieldType = TypePrim PrimBool,
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 10,
                 singleLocColumn = 10}}}],
       structOrigin =
       StructOriginStruct
         Struct {
           structDeclPath =
           DeclPathStruct (DeclNameTag (CName "bools2")) DeclPathTop,
           structSizeof = 2,
           structAlignment = 1,
           structFields =
           [ StructField {
               fieldName = CName "x",
               fieldOffset = 0,
               fieldType = TypePrim PrimBool,
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 9,
                 singleLocColumn = 10}},
             StructField {
               fieldName = CName "y",
               fieldOffset = 8,
               fieldType = TypePrim PrimBool,
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 10,
                 singleLocColumn = 10}}],
           structSourceLoc =
           SingleLoc {
             singleLocPath = ["examples", "bool.h"],
             singleLocLine = 8,
             singleLocColumn = 8}}}
     StorableInstance {
       storableSizeOf = 2,
       storableAlignment = 1,
       storablePeek =
       Lambda
         (NameHint "ptr")
         (Ap
            (StructCon
               Struct {
                 structName = HsName "@NsTypeConstr" "Bools2",
                 structConstr = HsName "@NsConstr" "Bools2",
                 structFields =
                 [ Field {
                     fieldName = HsName "@NsVar" "bools2_x",
                     fieldType = HsPrimType HsPrimCBool,
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 9,
                           singleLocColumn = 10}}},
                   Field {
                     fieldName = HsName "@NsVar" "bools2_y",
                     fieldType = HsPrimType HsPrimCBool,
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 10,
                           singleLocColumn = 10}}}],
                 structOrigin =
                 StructOriginStruct
                   Struct {
                     structDeclPath =
                     DeclPathStruct (DeclNameTag (CName "bools2")) DeclPathTop,
                     structSizeof = 2,
                     structAlignment = 1,
                     structFields =
                     [ StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 9,
                           singleLocColumn = 10}},
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 10,
                           singleLocColumn = 10}}],
                     structSourceLoc =
                     SingleLoc {
                       singleLocPath = ["examples", "bool.h"],
                       singleLocLine = 8,
                       singleLocColumn = 8}}})
            [PeekByteOff (Idx 0) 0, PeekByteOff (Idx 0) 1]),
       storablePoke =
       Lambda
         (NameHint "ptr")
         (Lambda
            (NameHint "s")
            (ElimStruct
               (Idx 0)
               Struct {
                 structName = HsName "@NsTypeConstr" "Bools2",
                 structConstr = HsName "@NsConstr" "Bools2",
                 structFields =
                 [ Field {
                     fieldName = HsName "@NsVar" "bools2_x",
                     fieldType = HsPrimType HsPrimCBool,
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 9,
                           singleLocColumn = 10}}},
                   Field {
                     fieldName = HsName "@NsVar" "bools2_y",
                     fieldType = HsPrimType HsPrimCBool,
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 10,
                           singleLocColumn = 10}}}],
                 structOrigin =
                 StructOriginStruct
                   Struct {
                     structDeclPath =
                     DeclPathStruct (DeclNameTag (CName "bools2")) DeclPathTop,
                     structSizeof = 2,
                     structAlignment = 1,
                     structFields =
                     [ StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 9,
                           singleLocColumn = 10}},
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypePrim PrimBool,
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 10,
                           singleLocColumn = 10}}],
                     structSourceLoc =
                     SingleLoc {
                       singleLocPath = ["examples", "bool.h"],
                       singleLocLine = 8,
                       singleLocColumn = 8}}}
               (Add 2)
               (Seq
                  [PokeByteOff (Idx 3) 0 (Idx 0), PokeByteOff (Idx 3) 1 (Idx 1)])))})
DeclData
  Struct {
    structName = HsName "@NsTypeConstr" "Bools3",
    structConstr = HsName "@NsConstr" "Bools3",
    structFields =
    [ Field {
        fieldName = HsName "@NsVar" "bools3_x",
        fieldType = HsTypRef (HsName "@NsTypeConstr" "BOOL"),
        fieldOrigin =
        FieldOriginStructField
          StructField {
            fieldName = CName "x",
            fieldOffset = 0,
            fieldType = TypeTypedef (CName "BOOL"),
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 16,
              singleLocColumn = 10}}},
      Field {
        fieldName = HsName "@NsVar" "bools3_y",
        fieldType = HsTypRef (HsName "@NsTypeConstr" "BOOL"),
        fieldOrigin =
        FieldOriginStructField
          StructField {
            fieldName = CName "y",
            fieldOffset = 8,
            fieldType = TypeTypedef (CName "BOOL"),
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 17,
              singleLocColumn = 10}}}],
    structOrigin =
    StructOriginStruct
      Struct {
        structDeclPath =
        DeclPathStruct (DeclNameTag (CName "bools3")) DeclPathTop,
        structSizeof = 2,
        structAlignment = 1,
        structFields =
        [ StructField {
            fieldName = CName "x",
            fieldOffset = 0,
            fieldType = TypeTypedef (CName "BOOL"),
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 16,
              singleLocColumn = 10}},
          StructField {
            fieldName = CName "y",
            fieldOffset = 8,
            fieldType = TypeTypedef (CName "BOOL"),
            fieldSourceLoc =
            SingleLoc {
              singleLocPath = ["examples", "bool.h"],
              singleLocLine = 17,
              singleLocColumn = 10}}],
        structSourceLoc =
        SingleLoc {
          singleLocPath = ["examples", "bool.h"],
          singleLocLine = 15,
          singleLocColumn = 8}}}
DeclInstance
  (InstanceStorable
     Struct {
       structName = HsName "@NsTypeConstr" "Bools3",
       structConstr = HsName "@NsConstr" "Bools3",
       structFields =
       [ Field {
           fieldName = HsName "@NsVar" "bools3_x",
           fieldType = HsTypRef (HsName "@NsTypeConstr" "BOOL"),
           fieldOrigin =
           FieldOriginStructField
             StructField {
               fieldName = CName "x",
               fieldOffset = 0,
               fieldType = TypeTypedef (CName "BOOL"),
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 16,
                 singleLocColumn = 10}}},
         Field {
           fieldName = HsName "@NsVar" "bools3_y",
           fieldType = HsTypRef (HsName "@NsTypeConstr" "BOOL"),
           fieldOrigin =
           FieldOriginStructField
             StructField {
               fieldName = CName "y",
               fieldOffset = 8,
               fieldType = TypeTypedef (CName "BOOL"),
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 17,
                 singleLocColumn = 10}}}],
       structOrigin =
       StructOriginStruct
         Struct {
           structDeclPath =
           DeclPathStruct (DeclNameTag (CName "bools3")) DeclPathTop,
           structSizeof = 2,
           structAlignment = 1,
           structFields =
           [ StructField {
               fieldName = CName "x",
               fieldOffset = 0,
               fieldType = TypeTypedef (CName "BOOL"),
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 16,
                 singleLocColumn = 10}},
             StructField {
               fieldName = CName "y",
               fieldOffset = 8,
               fieldType = TypeTypedef (CName "BOOL"),
               fieldSourceLoc =
               SingleLoc {
                 singleLocPath = ["examples", "bool.h"],
                 singleLocLine = 17,
                 singleLocColumn = 10}}],
           structSourceLoc =
           SingleLoc {
             singleLocPath = ["examples", "bool.h"],
             singleLocLine = 15,
             singleLocColumn = 8}}}
     StorableInstance {
       storableSizeOf = 2,
       storableAlignment = 1,
       storablePeek =
       Lambda
         (NameHint "ptr")
         (Ap
            (StructCon
               Struct {
                 structName = HsName "@NsTypeConstr" "Bools3",
                 structConstr = HsName "@NsConstr" "Bools3",
                 structFields =
                 [ Field {
                     fieldName = HsName "@NsVar" "bools3_x",
                     fieldType = HsTypRef (HsName "@NsTypeConstr" "BOOL"),
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypeTypedef (CName "BOOL"),
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 16,
                           singleLocColumn = 10}}},
                   Field {
                     fieldName = HsName "@NsVar" "bools3_y",
                     fieldType = HsTypRef (HsName "@NsTypeConstr" "BOOL"),
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypeTypedef (CName "BOOL"),
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 17,
                           singleLocColumn = 10}}}],
                 structOrigin =
                 StructOriginStruct
                   Struct {
                     structDeclPath =
                     DeclPathStruct (DeclNameTag (CName "bools3")) DeclPathTop,
                     structSizeof = 2,
                     structAlignment = 1,
                     structFields =
                     [ StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypeTypedef (CName "BOOL"),
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 16,
                           singleLocColumn = 10}},
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypeTypedef (CName "BOOL"),
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 17,
                           singleLocColumn = 10}}],
                     structSourceLoc =
                     SingleLoc {
                       singleLocPath = ["examples", "bool.h"],
                       singleLocLine = 15,
                       singleLocColumn = 8}}})
            [PeekByteOff (Idx 0) 0, PeekByteOff (Idx 0) 1]),
       storablePoke =
       Lambda
         (NameHint "ptr")
         (Lambda
            (NameHint "s")
            (ElimStruct
               (Idx 0)
               Struct {
                 structName = HsName "@NsTypeConstr" "Bools3",
                 structConstr = HsName "@NsConstr" "Bools3",
                 structFields =
                 [ Field {
                     fieldName = HsName "@NsVar" "bools3_x",
                     fieldType = HsTypRef (HsName "@NsTypeConstr" "BOOL"),
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypeTypedef (CName "BOOL"),
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 16,
                           singleLocColumn = 10}}},
                   Field {
                     fieldName = HsName "@NsVar" "bools3_y",
                     fieldType = HsTypRef (HsName "@NsTypeConstr" "BOOL"),
                     fieldOrigin =
                     FieldOriginStructField
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypeTypedef (CName "BOOL"),
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 17,
                           singleLocColumn = 10}}}],
                 structOrigin =
                 StructOriginStruct
                   Struct {
                     structDeclPath =
                     DeclPathStruct (DeclNameTag (CName "bools3")) DeclPathTop,
                     structSizeof = 2,
                     structAlignment = 1,
                     structFields =
                     [ StructField {
                         fieldName = CName "x",
                         fieldOffset = 0,
                         fieldType = TypeTypedef (CName "BOOL"),
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 16,
                           singleLocColumn = 10}},
                       StructField {
                         fieldName = CName "y",
                         fieldOffset = 8,
                         fieldType = TypeTypedef (CName "BOOL"),
                         fieldSourceLoc =
                         SingleLoc {
                           singleLocPath = ["examples", "bool.h"],
                           singleLocLine = 17,
                           singleLocColumn = 10}}],
                     structSourceLoc =
                     SingleLoc {
                       singleLocPath = ["examples", "bool.h"],
                       singleLocLine = 15,
                       singleLocColumn = 8}}}
               (Add 2)
               (Seq
                  [PokeByteOff (Idx 3) 0 (Idx 0), PokeByteOff (Idx 3) 1 (Idx 1)])))})
