
--
-- An AST format for generated code in imperative languages
--
module SyntaxImp
-- Uncomment the below line to expose all top level symbols for
-- repl testing
--- {-
  ( IId(..)
  , ITy(..)
  , IAnnId(..)
  , IAnnTy(..)
  , IDecl(..)
  , IStmt(..)
  , IConst(..)
  , IExpr(..)
  , (|->)
  , failureShortCircuit
  , getsStmt
  , mkMut
  , mkConst
  , intE
  ) 
-- -}
where

data IId = IId [String] (Maybe Int)
         | IPtrField IId IId
    deriving( Show )

(|->) :: IId -> IId -> IId
(|->) = IPtrField

data IAnnTy = INoAnn ITy
            | IMut ITy -- things are immutable by default
    deriving( Show )

data ITy = IBool 
         | IArr IAnnTy (Maybe Int)
         | IByte
         | IPtr IAnnTy
         | IStruct IId
         | IFile
         | ISize
    deriving( Show )

-- Id, type pair
data IAnnId = IAnnId IId IAnnTy
    deriving( Show )

data IDecl = IFun IId [IAnnId] IAnnTy [IStmt]
           | IStructure IId [IAnnId]
    deriving( Show )

data IStmt = IReturn IExpr
           | IExpload String
           | IExprStmt IExpr
           | IDeclStmt IAnnId
           | IDeclStmtInit IAnnId IExpr
           | IIfThen IExpr [IStmt]
    deriving( Show )


data IExpr = IEId IId
           | IEConst IConst
           | IEFunCall
                IId -- function name
                [IExpr] -- args
           | IEGets IId IExpr
           | IEZero ITy -- a "zero" value for the given type
           | IETrue
           | IEFalse
           | IENot IExpr
    deriving( Show )


data IConst = IConstInt Int
    deriving( Show )

--
-- Macros and helper functions
--

failureShortCircuit :: IExpr -> IStmt
failureShortCircuit expr =
  IIfThen (IENot expr) [IReturn IEFalse]

getsStmt :: IId -> IExpr -> IStmt
getsStmt id expr = IExprStmt $ IEGets id expr

mkMut :: IAnnTy -> IAnnTy
mkMut (INoAnn ty) = IMut ty
mkMut (IMut ty) = IMut ty

mkConst :: IAnnTy -> IAnnTy
mkConst (INoAnn ty) = INoAnn ty
mkConst (IMut ty) = INoAnn ty

intE :: Int -> IExpr
intE i = IEConst (IConstInt i)
