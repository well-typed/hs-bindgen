DeclEmpty "Foo"
DeclData (Struct {structName = "Bar", structConstr = "Bar", structFields = Field {fieldName = "bar_ptrA", fieldType = HsPtr (HsTypRef "Foo"), fieldOrigin = FieldOriginStructField (StructField {fieldName = "ptrA", fieldOffset = 0, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "foo") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:5:17"})} ::: Field {fieldName = "bar_ptrB", fieldType = HsPtr (HsTypRef "Bar"), fieldOrigin = FieldOriginStructField (StructField {fieldName = "ptrB", fieldOffset = 64, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "bar") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:6:17"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "bar") DeclPathTop, structSizeof = 16, structAlignment = 8, structFields = [StructField {fieldName = "ptrA", fieldOffset = 0, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "foo") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:5:17"},StructField {fieldName = "ptrB", fieldOffset = 64, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "bar") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:6:17"}], structSourceLoc = "examples/opaque_declaration.h:4:8"})})
DeclInstance (InstanceStorable (Struct {structName = "Bar", structConstr = "Bar", structFields = Field {fieldName = "bar_ptrA", fieldType = HsPtr (HsTypRef "Foo"), fieldOrigin = FieldOriginStructField (StructField {fieldName = "ptrA", fieldOffset = 0, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "foo") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:5:17"})} ::: Field {fieldName = "bar_ptrB", fieldType = HsPtr (HsTypRef "Bar"), fieldOrigin = FieldOriginStructField (StructField {fieldName = "ptrB", fieldOffset = 64, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "bar") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:6:17"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "bar") DeclPathTop, structSizeof = 16, structAlignment = 8, structFields = [StructField {fieldName = "ptrA", fieldOffset = 0, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "foo") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:5:17"},StructField {fieldName = "ptrB", fieldOffset = 64, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "bar") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:6:17"}], structSourceLoc = "examples/opaque_declaration.h:4:8"})}) (StorableInstance {storableSizeOf = 16, storableAlignment = 8, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "Bar", structConstr = "Bar", structFields = Field {fieldName = "bar_ptrA", fieldType = HsPtr (HsTypRef "Foo"), fieldOrigin = FieldOriginStructField (StructField {fieldName = "ptrA", fieldOffset = 0, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "foo") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:5:17"})} ::: Field {fieldName = "bar_ptrB", fieldType = HsPtr (HsTypRef "Bar"), fieldOrigin = FieldOriginStructField (StructField {fieldName = "ptrB", fieldOffset = 64, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "bar") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:6:17"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "bar") DeclPathTop, structSizeof = 16, structAlignment = 8, structFields = [StructField {fieldName = "ptrA", fieldOffset = 0, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "foo") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:5:17"},StructField {fieldName = "ptrB", fieldOffset = 64, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "bar") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:6:17"}], structSourceLoc = "examples/opaque_declaration.h:4:8"})})) [PeekByteOff 0 0,PeekByteOff 0 8]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "Bar", structConstr = "Bar", structFields = Field {fieldName = "bar_ptrA", fieldType = HsPtr (HsTypRef "Foo"), fieldOrigin = FieldOriginStructField (StructField {fieldName = "ptrA", fieldOffset = 0, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "foo") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:5:17"})} ::: Field {fieldName = "bar_ptrB", fieldType = HsPtr (HsTypRef "Bar"), fieldOrigin = FieldOriginStructField (StructField {fieldName = "ptrB", fieldOffset = 64, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "bar") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:6:17"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "bar") DeclPathTop, structSizeof = 16, structAlignment = 8, structFields = [StructField {fieldName = "ptrA", fieldOffset = 0, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "foo") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:5:17"},StructField {fieldName = "ptrB", fieldOffset = 64, fieldType = TypePointer (TypeStruct (DeclPathStruct (DeclNameTag "bar") DeclPathTop)), fieldSourceLoc = "examples/opaque_declaration.h:6:17"}], structSourceLoc = "examples/opaque_declaration.h:4:8"})}) 2 (Seq [PokeByteOff 3 0 0,PokeByteOff 3 8 1])))}))
DeclData (Struct {structName = "Baz", structConstr = "Baz", structFields = VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "baz") DeclPathTop, structSizeof = 0, structAlignment = 1, structFields = [], structSourceLoc = "examples/opaque_declaration.h:9:8"})})
DeclInstance (InstanceStorable (Struct {structName = "Baz", structConstr = "Baz", structFields = VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "baz") DeclPathTop, structSizeof = 0, structAlignment = 1, structFields = [], structSourceLoc = "examples/opaque_declaration.h:9:8"})}) (StorableInstance {storableSizeOf = 0, storableAlignment = 1, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "Baz", structConstr = "Baz", structFields = VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "baz") DeclPathTop, structSizeof = 0, structAlignment = 1, structFields = [], structSourceLoc = "examples/opaque_declaration.h:9:8"})})) []), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "Baz", structConstr = "Baz", structFields = VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "baz") DeclPathTop, structSizeof = 0, structAlignment = 1, structFields = [], structSourceLoc = "examples/opaque_declaration.h:9:8"})}) 0 (Seq [])))}))
DeclEmpty "Quu"
