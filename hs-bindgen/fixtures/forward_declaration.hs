DeclData (Struct {structName = "S1", structConstr = "S1", structFields = Field {fieldName = "s1_a", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:4:7"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "S1") DeclPathTop, structSizeof = 4, structAlignment = 4, structFields = [StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:4:7"}], structSourceLoc = "examples/forward_declaration.h:3:8"})})
DeclInstance (InstanceStorable (Struct {structName = "S1", structConstr = "S1", structFields = Field {fieldName = "s1_a", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:4:7"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "S1") DeclPathTop, structSizeof = 4, structAlignment = 4, structFields = [StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:4:7"}], structSourceLoc = "examples/forward_declaration.h:3:8"})}) (StorableInstance {storableSizeOf = 4, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "S1", structConstr = "S1", structFields = Field {fieldName = "s1_a", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:4:7"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "S1") DeclPathTop, structSizeof = 4, structAlignment = 4, structFields = [StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:4:7"}], structSourceLoc = "examples/forward_declaration.h:3:8"})})) [PeekByteOff 0 0]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "S1", structConstr = "S1", structFields = Field {fieldName = "s1_a", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:4:7"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "S1") DeclPathTop, structSizeof = 4, structAlignment = 4, structFields = [StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:4:7"}], structSourceLoc = "examples/forward_declaration.h:3:8"})}) 1 (Seq [PokeByteOff 2 0 0])))}))
DeclNewtype (Newtype {newtypeName = "S1_t", newtypeConstr = "S1_t", newtypeField = Field {fieldName = "unS1_t", fieldType = HsTypRef "S1", fieldOrigin = FieldOriginNone}, newtypeOrigin = NewtypeOriginTypedef (Typedef {typedefName = "S1_t", typedefType = TypeStruct (DeclPathStruct (DeclNameTag "S1") DeclPathTop), typedefSourceLoc = "examples/forward_declaration.h:1:19"})})
DeclNewtypeInstance Storable "S1_t"
DeclData (Struct {structName = "S2", structConstr = "S2", structFields = Field {fieldName = "s2_a", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:10:7"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "S2") DeclPathTop, structSizeof = 4, structAlignment = 4, structFields = [StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:10:7"}], structSourceLoc = "examples/forward_declaration.h:9:8"})})
DeclInstance (InstanceStorable (Struct {structName = "S2", structConstr = "S2", structFields = Field {fieldName = "s2_a", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:10:7"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "S2") DeclPathTop, structSizeof = 4, structAlignment = 4, structFields = [StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:10:7"}], structSourceLoc = "examples/forward_declaration.h:9:8"})}) (StorableInstance {storableSizeOf = 4, storableAlignment = 4, storablePeek = Lambda "ptr" (Ap (StructCon (Struct {structName = "S2", structConstr = "S2", structFields = Field {fieldName = "s2_a", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:10:7"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "S2") DeclPathTop, structSizeof = 4, structAlignment = 4, structFields = [StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:10:7"}], structSourceLoc = "examples/forward_declaration.h:9:8"})})) [PeekByteOff 0 0]), storablePoke = Lambda "ptr" (Lambda "s" (ElimStruct 0 (Struct {structName = "S2", structConstr = "S2", structFields = Field {fieldName = "s2_a", fieldType = HsPrimType HsPrimCInt, fieldOrigin = FieldOriginStructField (StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:10:7"})} ::: VNil, structOrigin = StructOriginStruct (Struct {structDeclPath = DeclPathStruct (DeclNameTag "S2") DeclPathTop, structSizeof = 4, structAlignment = 4, structFields = [StructField {fieldName = "a", fieldOffset = 0, fieldType = TypePrim (PrimIntegral (PrimInt Signed)), fieldSourceLoc = "examples/forward_declaration.h:10:7"}], structSourceLoc = "examples/forward_declaration.h:9:8"})}) 1 (Seq [PokeByteOff 2 0 0])))}))
