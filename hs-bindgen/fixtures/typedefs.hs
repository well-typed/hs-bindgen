DeclNewtype (Newtype {newtypeName = "CMyint", newtypeConstr = "MkCMyint", newtypeField = Field {fieldName = "unCMyint", fieldType = HsPrimType HsPrimCInt}, newtypeTypeSpelling = Just "myint"})
DeclNewtypeInstance Storable "CMyint"
DeclNewtype (Newtype {newtypeName = "CIntptr", newtypeConstr = "MkCIntptr", newtypeField = Field {fieldName = "unCIntptr", fieldType = HsPtr (HsPrimType HsPrimCInt)}, newtypeTypeSpelling = Just "intptr"})
DeclNewtypeInstance Storable "CIntptr"
