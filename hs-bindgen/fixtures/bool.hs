DeclNewtype (Newtype {newtypeName = "BOOL", newtypeConstr = "BOOL", newtypeField = Field {fieldName = "unBOOL", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginNone}, newtypeOrigin = NewtypeOriginMacro (Macro {macroLoc = "examples/bool.h:13:9", macroName = "BOOL", macroArgs = [], macroBody = MTerm (MType PrimBool)})})
DeclData (Struct {structName = "Bools1", structConstr = "Bools1", structFields = Field {fieldName = "bools1_x", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:2:11"})} ::: Field {fieldName = "bools1_y", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:3:11"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools1") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:2:11"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:3:11"}], structSourceLoc = "examples/bool.h:1:8"})})
DeclInstance (InstanceStorable (Struct {structName = "Bools1", structConstr = "Bools1", structFields = Field {fieldName = "bools1_x", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:2:11"})} ::: Field {fieldName = "bools1_y", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:3:11"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools1") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:2:11"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:3:11"}], structSourceLoc = "examples/bool.h:1:8"})}) (StorableInstance {storableSizeOf = 2, storableAlignment = 1, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "Bools1", structConstr = "Bools1", structFields = Field {fieldName = "bools1_x", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:2:11"})} ::: Field {fieldName = "bools1_y", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:3:11"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools1") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:2:11"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:3:11"}], structSourceLoc = "examples/bool.h:1:8"})})) [PeekByteOff 0 0,PeekByteOff 0 1]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "Bools1", structConstr = "Bools1", structFields = Field {fieldName = "bools1_x", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:2:11"})} ::: Field {fieldName = "bools1_y", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:3:11"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools1") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:2:11"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:3:11"}], structSourceLoc = "examples/bool.h:1:8"})}) 2 (Seq [PokeByteOff 3 0 0,PokeByteOff 3 1 1])))}))
DeclData (Struct {structName = "Bools2", structConstr = "Bools2", structFields = Field {fieldName = "bools2_x", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:9:10"})} ::: Field {fieldName = "bools2_y", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:10:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools2") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:9:10"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:10:10"}], structSourceLoc = "examples/bool.h:8:8"})})
DeclInstance (InstanceStorable (Struct {structName = "Bools2", structConstr = "Bools2", structFields = Field {fieldName = "bools2_x", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:9:10"})} ::: Field {fieldName = "bools2_y", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:10:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools2") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:9:10"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:10:10"}], structSourceLoc = "examples/bool.h:8:8"})}) (StorableInstance {storableSizeOf = 2, storableAlignment = 1, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "Bools2", structConstr = "Bools2", structFields = Field {fieldName = "bools2_x", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:9:10"})} ::: Field {fieldName = "bools2_y", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:10:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools2") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:9:10"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:10:10"}], structSourceLoc = "examples/bool.h:8:8"})})) [PeekByteOff 0 0,PeekByteOff 0 1]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "Bools2", structConstr = "Bools2", structFields = Field {fieldName = "bools2_x", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:9:10"})} ::: Field {fieldName = "bools2_y", fieldType = HsPrimType HsPrimCBool, fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:10:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools2") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:9:10"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypePrim PrimBool, fieldSourceLoc = "examples/bool.h:10:10"}], structSourceLoc = "examples/bool.h:8:8"})}) 2 (Seq [PokeByteOff 3 0 0,PokeByteOff 3 1 1])))}))
DeclData (Struct {structName = "Bools3", structConstr = "Bools3", structFields = Field {fieldName = "bools3_x", fieldType = HsTypRef "BOOL", fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:16:10"})} ::: Field {fieldName = "bools3_y", fieldType = HsTypRef "BOOL", fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:17:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools3") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:16:10"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:17:10"}], structSourceLoc = "examples/bool.h:15:8"})})
DeclInstance (InstanceStorable (Struct {structName = "Bools3", structConstr = "Bools3", structFields = Field {fieldName = "bools3_x", fieldType = HsTypRef "BOOL", fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:16:10"})} ::: Field {fieldName = "bools3_y", fieldType = HsTypRef "BOOL", fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:17:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools3") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:16:10"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:17:10"}], structSourceLoc = "examples/bool.h:15:8"})}) (StorableInstance {storableSizeOf = 2, storableAlignment = 1, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "Bools3", structConstr = "Bools3", structFields = Field {fieldName = "bools3_x", fieldType = HsTypRef "BOOL", fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:16:10"})} ::: Field {fieldName = "bools3_y", fieldType = HsTypRef "BOOL", fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:17:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools3") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:16:10"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:17:10"}], structSourceLoc = "examples/bool.h:15:8"})})) [PeekByteOff 0 0,PeekByteOff 0 1]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "Bools3", structConstr = "Bools3", structFields = Field {fieldName = "bools3_x", fieldType = HsTypRef "BOOL", fieldOrigin = FieldOriginStructField (StructField {fieldName = "x", fieldOffset = 0, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:16:10"})} ::: Field {fieldName = "bools3_y", fieldType = HsTypRef "BOOL", fieldOrigin = FieldOriginStructField (StructField {fieldName = "y", fieldOffset = 8, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:17:10"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (Just "bools3") DeclPathTop, structSizeof = 2, structAlignment = 1, structFields = [StructField {fieldName = "x", fieldOffset = 0, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:16:10"},StructField {fieldName = "y", fieldOffset = 8, fieldType = TypeTypedef "BOOL", fieldSourceLoc = "examples/bool.h:17:10"}], structSourceLoc = "examples/bool.h:15:8"})}) 2 (Seq [PokeByteOff 3 0 0,PokeByteOff 3 1 1])))}))
