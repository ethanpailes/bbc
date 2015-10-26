

import Data.Word

type Var = String -- for now

data Location = Var | Record -- TODO figure out how to target place in record

data Operation = Read Int Location Location -- amount, source, target
               | Write Int Location Location -- amount, source, target
               | MaskOr Location Word64 -- target, mask
                                        -- (target := target | mask)
               | MaskAnd Location Word64 -- target, mask
                                         --(target := target & mask)

data Statement = Op Operation
               | DecInit Var String -- variable, initialization value

data Method = Method [Var] [Statement]

data Record = Record [Method] Data
