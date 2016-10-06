{-# LANGUAGE OverloadedStrings #-}


module CPretty
{-
() 
-- -}
where

import SyntaxImp
import Text.PrettyPrint hiding ((<>))
import Data.Monoid

-- import Debug.Trace (trace)

class CPretty a where
    cpretty :: a -> Doc


--
-- Identifiers
--

instance CPretty IId where
    cpretty (IId name Nothing) = join "_" (map text name)
    cpretty (IId name (Just number)) = 
        cpretty (IId name Nothing) <> "_" <> int number
    cpretty (IPtrField ptr field) = cpretty ptr <> "->" <> cpretty field

instance CPretty IAnnId where
    cpretty = prettyAnnId


prettyAnnId :: IAnnId -> Doc
prettyAnnId (IAnnId id (INoAnn (IArr ty (Just n)))) =
    "const " <> cpretty ty <> space <> cpretty id <> brackets (int n)
prettyAnnId (IAnnId id (INoAnn (IArr ty Nothing))) =
    cpretty ty <> space <> cpretty id <> brackets ""
prettyAnnId (IAnnId id ty) = cpretty ty <+> cpretty id

--
-- Types
--

instance CPretty IAnnTy where
    cpretty = prettyAnnTy

prettyAnnTy :: IAnnTy -> Doc
-- prettyAnnTy (IMut (IPtr (INoAnn ty))) = 
-- prettyAnnTy (IMut (IPtr (IMut ty))) = cpretty ty <> " * " <> 
prettyAnnTy (INoAnn ty) =
  case ty of
    (IPtr (INoAnn inner)) -> "const " <> cpretty inner <> " const ";
     _                 -> "const " <> cpretty ty
prettyAnnTy (IMut ty) =
  case ty of
    (IPtr (INoAnn inner)) -> cpretty inner <> " const";
    _ -> cpretty ty

instance CPretty ITy where
    cpretty = prettyTy

prettyTy :: ITy -> Doc
prettyTy IBool = "bool"
prettyTy IByte = "char"
prettyTy ISize = "size_t"
prettyTy (IStruct id) = "struct " <> cpretty id
prettyTy (IArr ty (Just n)) = cpretty ty <> space <> brackets (int n)
prettyTy (IArr ty Nothing) = cpretty ty <> space <> brackets ""
prettyTy (IPtr ty) = cpretty ty <> " *"
prettyTy IFile = "FILE *"



--
-- Decls
--

instance CPretty IDecl where
    cpretty = prettyDecl

prettyDecl :: IDecl -> Doc
prettyDecl (IFun name args rety stmts) =
            cpretty rety 
         $$ cpretty name <> parenList args 
         $+$ block stmts

prettyDecl (IStructure name members) =
    "typedef struct " <> n <> " {" $$
        vcat (map (ind . (<>semi) . cpretty) members) $$
        "} *" <> n <> "_h" <> semi
    where n = cpretty name

    
--
-- Statements
--

instance CPretty IStmt where
    cpretty s = prettyStmt s <> semi

-- a C statement, minus the semi colin
prettyStmt :: IStmt -> Doc
prettyStmt (IReturn expr) = "return " <> cpretty expr 
prettyStmt (IExpload reason) = ">>>>> UNIMPLIMENTED CODE GEN ::" <> text reason
prettyStmt (IExprStmt expr) = cpretty expr
prettyStmt (IDeclStmt (IAnnId id ty)) = cpretty ty <> " " <> cpretty id
prettyStmt (IIfThen expr stmts) =
  "if " <> parens (cpretty expr) $$ block stmts
prettyStmt (IDeclStmtInit (IAnnId id ty) expr) =
  cpretty ty <> " " <> cpretty id <> " = " <> cpretty expr

--
-- Expressions
-- 

instance CPretty IExpr where
    cpretty = prettyExpr

prettyExpr :: IExpr -> Doc
prettyExpr (IEId id) = cpretty id
prettyExpr (IEConst c) = cpretty c
prettyExpr (IEFunCall funName args) = cpretty funName <> parenList args
prettyExpr (IEGets id expr) = cpretty id <> " = " <> cpretty expr
prettyExpr IETrue = "true"
prettyExpr IEFalse = "false"
prettyExpr (IENot expr) = "!" <> cpretty expr
prettyExpr (IEZero ty) =
  case ty of
    (IPtr _) -> "NULL"
    IBool -> "false"
    ISize -> "0"
    IByte -> "0"
    _ -> "BOGUS ZERO!!!!"

--
-- Constants
-- 

instance CPretty IConst where
    cpretty = prettyConst

prettyConst :: IConst -> Doc
prettyConst (IConstInt i) = text $ show i

--
-- Utils
--

join :: Doc -> [Doc] -> Doc
join sep ds = hcat $ punctuate sep ds

ind :: Doc -> Doc
ind = nest 4

parenList :: CPretty a => [a] -> Doc
parenList ds = parens $ join ", " (map cpretty ds)

block :: [IStmt] -> Doc
block stmts =
  foldr ($+$) mempty [
     "{"
    , (ind . vcat . map cpretty) stmts
    , "}"
    ]

--
-- REPL testing
-- 

smts :: [IStmt]
smts = [
  IExprStmt
     (IEGets (IPtrField (IId ["test"] Nothing)
              (IId ["data"] Nothing))
      (IEFunCall (IId ["malloc"] Nothing) [IEConst (IConstInt 3)])),

  IExprStmt (IEGets (IPtrField (IId ["test"] Nothing)
                     (IId ["parent"] Nothing))
             (IEZero (IPtr (INoAnn (IPtr (IMut (IStruct (IId ["test"] Nothing)))))))),

  IExprStmt (IEGets (IPtrField (IId ["test"] Nothing) (IId ["is","valid"] Nothing)) IETrue)
  ,
  IIfThen (IENot (IEFunCall (IId ["fread"] Nothing) [IEId (IPtrField (IId ["test"]
 Nothing) (IId ["data"] Nothing))
  ,IEConst (IConstInt 3),IEConst (IConstInt 1),
   IEId (IId ["file"] Nothing)])) [IReturn IEFalse]]
