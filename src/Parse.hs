{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Ast
import Test.QuickCheck

prop_testProperty :: () -> Bool
prop_testProperty _ = True

return []
testMod :: IO Bool
testMod = $quickCheckAll
