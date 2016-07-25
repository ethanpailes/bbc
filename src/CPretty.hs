
module CPretty
{-
() 
-- -}
where

import SyntaxImp
import Text.PrettyPrint hiding ((<>))
import Data.Monoid

class CPretty a where
    cpretty :: a -> Doc

join :: Doc -> [Doc] -> Doc
join sep ds = hcat $ punctuate sep ds

ind :: Doc -> Doc
ind = nest 4

--
-- Identifiers
--

instance CPretty IId where
    cpretty (IId name Nothing) = join "_" (map text name)
    cpretty (IId name (Just number)) = 
        cpretty (IId name Nothing) <> "_" <> int number

instance CPretty IAnnId where
    cpretty = prettyAnnId


prettyAnnId :: IAnnId -> Doc
prettyAnnId (IAnnId id (INoAnn (IArr ty (Just n)))) =
    "const " <> cpretty ty <> space <> cpretty id <> brackets (int n)
prettyAnnId (IAnnId id (INoAnn (IArr ty Nothing))) =
    cpretty ty <> space <> cpretty id <> brackets ""
prettyAnnId (IAnnId id ty) = (cpretty ty) <+> (cpretty id)

{-
prettyAnnId (IAnnId id (IArr ty (Just n))) =
        cpretty ty <> space <> cpretty id <> brackets (int n)
prettyAnnId (IAnnId id (IArr ty Nothing)) =
        cpretty ty <> space <> cpretty id <> brackets ""
-}

--
-- Types
--

instance CPretty IAnnTy where
    cpretty = prettyAnnTy

prettyAnnTy :: IAnnTy -> Doc
prettyAnnTy (INoAnn ty) = "const " <> cpretty ty
prettyAnnTy (IMut ty) = cpretty ty

instance CPretty ITy where
    cpretty = prettyTy

prettyTy :: ITy -> Doc
prettyTy IBool = "bool"
prettyTy IByte = "char"
prettyTy (IStruct id) = "struct " <> cpretty id
prettyTy (IArr ty (Just n)) = cpretty ty <> space <> brackets (int n)
prettyTy (IArr ty Nothing) = cpretty ty <> space <> brackets ""
prettyTy (IPtr ty) = cpretty ty <> " *"



--
-- Decls
--

instance CPretty IDecl where
    cpretty = prettyDecl

prettyDecl :: IDecl -> Doc
prettyDecl (IFun name args rety stmts) =
    cpretty rety <> space <> cpretty name <> 
            parens (join ", " (map cpretty args))
         <> (braces . ind) (vcat (map cpretty stmts))
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
prettyStmt (IReturn id) = "return " <> cpretty id



