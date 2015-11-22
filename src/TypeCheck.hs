
module TypeCheck where
import Ast
import Data.Map
import Exceptions
import Control.Exception

type Env = Map String

gammaInit = empty

typeCheck :: [Block] -> Env Block -> Env Block
typeCheck [] gamma = gamma
typeCheck (Block _ [] : bs) gamma = gamma -- throw out empty blocks
typeCheck (Block blockName entries : bs) gamma =
  typeCheck bs gamma'
    where
      gamma' = if tc entries
                  then insert blockName (Block blockName entries) gamma
                  else throw TypeError -- should never get here
      tc [] = True
      tc (e:es) =
        case e of
          (Blk _) -> tc es
          (Field _ (BField {} )) -> tc es
          (Field _ (Tycon tyName)) -> if tyName `member` gamma
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

