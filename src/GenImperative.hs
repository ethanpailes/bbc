{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module GenImperative where
import Ast
import Data.Word
import Test.QuickCheck

--------------------------------------------------------------------------------
-- The General Imperative Ast
--------------------------------------------------------------------------------

data WordSize = W8 | W16 | W32 | W64
type Var = (String, WordSize)  -- for now

-- |Either a varaible or n subblock names followed by a field name
data Location = Variable Var | DataField [String]

data Operation = Read Int Location -- amount, target
               | Write Int Location -- amount, target
               | MaskOr Location Word64 -- target, mask
                                        -- (target := target | mask)
               | MaskAnd Location Word64 -- target, mask
                                         -- target := target & mask
               | BitOr Location Location -- target := target | src
               | BitAnd Location Location -- target := target & src
               | Gets Location Location -- target := source
               | RightShift Location Int -- target >>= shiftby
               | LeftShift Location Int -- target <<= shiftby

data Statement = Op Operation
               | DecInit Var String -- variable, initialization value
               | For Int [Statement] -- bound, statement list

data Method = Method Name [Var] [Statement]

data Record = Record [Method] Block

--------------------------------------------------------------------------------
-- Functions to transform Blocks into Records
--------------------------------------------------------------------------------

tmp, carry :: Var
tmp = ("tmp", W64)
carry = ("carry", W8)


genReadEntry :: Entry -> [Statement]
genReadEntry (Blk b) = undefined
-- Assumes the use of two byte sized variables carry and tmp
-- If the previous entry had to chop off any low order bits they will
-- be stored in carry.
genReadEntry (Field n (BField len s e)) =
  [Op (Read byteLen tgt),
   Op (Gets vTmp tgt),
   Op (RightShift tgt leftOver),
   Op (LeftShift vCarry len),
   Op (BitOr tgt vCarry),
   Op (Gets vCarry vTmp)]
    where byteLen = ceiling $ fromIntegral len / 8
          leftOver = (byteLen * 8) - len
          tgt = DataField [n]
          vTmp = Variable tmp
          vCarry = Variable carry
genReadEntry (Field n (Tycon tyname)) = undefined
genReadEntry (Field n (TyConapp ty tys)) = undefined


genReadBlock :: Block -> Method
genReadBlock (Block n es) = Method
                              ("read_" ++ n) []
                              ( DecInit carry "0" :
                                DecInit tmp "0" :
                                concatMap genReadEntry es)


return []
testMod :: IO Bool
testMod = $quickCheckAll
