
--
-- An AST format for generated code in imperative languages
--
module SyntaxImp
-- Uncomment the below line to expose all top level symbols for
-- repl testing
{-
() 
-- -}
where

data IId = IId [String] (Maybe Int)
    deriving( Show )

data IAnnTy = INoAnn ITy
            | IMut ITy -- things are immutable by default
    deriving( Show )

data ITy = IBool 
         | IArr IAnnTy (Maybe Int)
         | IByte
         | IPtr IAnnTy
         | IStruct IId
    deriving( Show )

-- Id, type pair
data IAnnId = IAnnId IId IAnnTy
    deriving( Show )

data IDecl = IFun IId [IAnnId] IAnnTy [IStmt]
           | IStructure IId [IAnnId]
    deriving( Show )

data IStmt = IReturn IId
    deriving( Show )
