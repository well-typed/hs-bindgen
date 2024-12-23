[
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "iNCR",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              IntegralTyCon
              [TyVarTy (Idx 0)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (TyVarTy (Idx 0))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclApp
            (InfixAppHead MAdd)
            [
              VarDeclVar (Idx 0),
              VarDeclIntegral
                1
                HsPrimCInt]))},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "aDD",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              NumTyCon
              [TyVarTy (Idx 0)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 0))
              (TyVarTy (Idx 0)))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MAdd)
                [
                  VarDeclVar (Idx 1),
                  VarDeclVar (Idx 0)]))))},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "iD",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (TyVarTy (Idx 0))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclVar (Idx 0)))},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "cONST",
      varDeclType = ForallTy {
        forallTySize = Size 2,
        forallTyBinders = [
          NameHint "a",
          NameHint "b"],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (TyVarTy (Idx 0)))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclVar (Idx 1)))))},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "cMP",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              OrdTyCon
              [TyVarTy (Idx 0)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 0))
              (TyConAppTy
                (TyConApp BoolTyCon [])))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MRelLT)
                [
                  VarDeclVar (Idx 1),
                  VarDeclVar (Idx 0)]))))},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "fUN1",
      varDeclType = ForallTy {
        forallTySize = Size 0,
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = FunTy
            (TyConAppTy
              (TyConApp
                (IntLikeTyCon
                  (PrimLongLong Unsigned))
                []))
            (FunTy
              (TyConAppTy
                (TyConApp
                  (IntLikeTyCon
                    (PrimLongLong Unsigned))
                  []))
              (TyConAppTy
                (TyConApp
                  (IntLikeTyCon
                    (PrimLongLong Unsigned))
                  [])))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MAdd)
                [
                  VarDeclVar (Idx 1),
                  VarDeclApp
                    (InfixAppHead MMult)
                    [
                      VarDeclIntegral
                        12
                        HsPrimCULLong,
                      VarDeclVar (Idx 0)]]))))},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "fUN2",
      varDeclType = ForallTy {
        forallTySize = Size 1,
        forallTyBinders = [
          NameHint "a"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              BitsTyCon
              [TyVarTy (Idx 0)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyConAppTy
                (TyConApp
                  (IntLikeTyCon
                    (PrimLongLong Unsigned))
                  []))
              (TyVarTy (Idx 0)))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MShiftLeft)
                [
                  VarDeclVar (Idx 1),
                  VarDeclApp
                    (InfixAppHead MMult)
                    [
                      VarDeclIntegral 3 HsPrimCULLong,
                      VarDeclVar (Idx 0)]]))))},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "g",
      varDeclType = ForallTy {
        forallTySize = Size 2,
        forallTyBinders = [
          NameHint "a",
          NameHint "b"],
        forallTy = QuantTy {
          quantTyCts = [
            ClassTy
              IntegralTyCon
              [TyVarTy (Idx 1)]],
          quantTyBody = FunTy
            (TyVarTy (Idx 0))
            (FunTy
              (TyVarTy (Idx 1))
              (TyVarTy (Idx 1)))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (VarAppHead
                  (HsName "@NsVar" "cONST"))
                [
                  VarDeclApp
                    (VarAppHead
                      (HsName "@NsVar" "iNCR"))
                    [VarDeclVar (Idx 0)],
                  VarDeclApp
                    (VarAppHead
                      (HsName "@NsVar" "iD"))
                    [VarDeclVar (Idx 1)]]))))},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "dIV1",
      varDeclType = ForallTy {
        forallTySize = Size 0,
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = FunTy
            (TyConAppTy
              (TyConApp
                (IntLikeTyCon
                  (PrimInt Unsigned))
                []))
            (FunTy
              (TyConAppTy
                (TyConApp
                  (IntLikeTyCon
                    (PrimInt Unsigned))
                  []))
              (TyConAppTy
                (TyConApp
                  (IntLikeTyCon
                    (PrimInt Unsigned))
                  [])))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MDiv)
                [
                  VarDeclVar (Idx 1),
                  VarDeclApp
                    (InfixAppHead MAdd)
                    [
                      VarDeclVar (Idx 0),
                      VarDeclIntegral
                        12
                        HsPrimCUInt]]))))},
  DeclVar
    VarDecl {
      varDeclName = HsName
        "@NsVar"
        "dIV2",
      varDeclType = ForallTy {
        forallTySize = Size 0,
        forallTyBinders = [],
        forallTy = QuantTy {
          quantTyCts = [],
          quantTyBody = FunTy
            (TyConAppTy
              (TyConApp
                (FloatLikeTyCon PrimFloat)
                []))
            (FunTy
              (TyConAppTy
                (TyConApp
                  (FloatLikeTyCon PrimFloat)
                  []))
              (TyConAppTy
                (TyConApp
                  (FloatLikeTyCon PrimFloat)
                  [])))}},
      varDeclBody = VarDeclLambda
        (Lambda
          (NameHint "x")
          (VarDeclLambda
            (Lambda
              (NameHint "y")
              (VarDeclApp
                (InfixAppHead MDiv)
                [
                  VarDeclApp
                    (InfixAppHead MMult)
                    [
                      VarDeclFloat 10.0,
                      VarDeclVar (Idx 1)],
                  VarDeclVar (Idx 0)]))))}]
