
module TypeCheck where
import Ast
import qualified Data.Map as M
import Data.List
import Exceptions
import Control.Exception

type Env = M.Map String

gammaInit :: Env Block
gammaInit = M.empty

typeCheck :: [Block] -> Env Block -> Env Block
typeCheck [] gamma = gamma
typeCheck (Block _ [] : _) gamma = gamma -- throw out empty blocks
typeCheck (Block blockName entries : bs) gamma =
  typeCheck bs gamma'
    where
      gamma' = if tc entries
                  then M.insert blockName (Block blockName entries) gamma
                  else throw TypeError -- should never get here
      tc [] = True
      tc (e:es) =
        case e of
          (Blk _) -> tc es
          (Field _ (BField {} )) -> tc es
          (Field _ (Tycon tyName)) -> if tyName `M.member` gamma
                                             then tc es
                                             else throw $ UnknownBlock tyName
          (Field _ tca@(TyConapp ty tys)) ->
            case ty of
              (Tycon "array") ->
                case tys of
                   [TyConapp {}, _] -> throw (MalformedHigherOrderType
                                                "TypeCheck - bad array:" tca)
                   [_, TyConapp {}] -> throw (MalformedHigherOrderType
                                                "TypeCheck - bad array:" tca)
                   _ -> tc es
              (Tycon tcon) -> throw $ UnknownTypeConstructor tcon
              _ -> throw $ MalformedHigherOrderType "TypeCheck - unknown:" tca
          (Field _ sty@(SumTy tag opts)) ->
            if any (\x -> length x /= 1) (group (sort opts))
               then throw $ NonUniqueSumTags sty
               else tc es

