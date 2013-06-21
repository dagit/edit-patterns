module WeaveUtils where

import Data.Tree
import Data.Tree.Types
import Data.Tree.Weaver

isStmt :: Label -> Bool
isStmt (LBLString "StmtBlock")    = True
isStmt (LBLString "IfThen")       = True
isStmt (LBLString "IfThenElse")   = True
isStmt (LBLString "While")        = True
isStmt (LBLString "BasicFor")     = True
isStmt (LBLString "EnhancedFor")  = True
isStmt (LBLString "Empty")        = True
isStmt (LBLString "ExpStmt")      = True
isStmt (LBLString "Assert")       = True
isStmt (LBLString "Switch")       = True
isStmt (LBLString "Do")           = True
isStmt (LBLString "Break")        = True
isStmt (LBLString "Continue")     = True
isStmt (LBLString "Return")       = True
isStmt (LBLString "Synchronized") = True
isStmt (LBLString "Throw")        = True
isStmt (LBLString "Try")          = True
isStmt (LBLString "Labeled")      = True
isStmt _                          = False

wl :: WeaveTree a -> Bool -> LabeledTree
wl (WLeaf l)  _ = l
wl (WNode lbl _ kids) dir = Node lbl (concatMap (wpl dir) kids)

wpl :: Bool -> WeavePoint a -> [LabeledTree]
wpl b     (Match t)      = [wl t b]
wpl True  (Mismatch a _) = [wl a True]
wpl False (Mismatch _ b) = [wl b False]
wpl True  (LeftHole _)   = []
wpl False (LeftHole h)   = [wl h False]
wpl False (RightHole _)  = []
wpl True  (RightHole h)  = [wl h True]

labeler :: (Label -> Bool) -> WeaveTree a -> (Bool, WeaveTree Bool)
labeler _       (WLeaf l)          = (True, WLeaf l)
labeler lbltest (WNode lbl _ kids) = 
  let (kstat,kids') = unzip $ map (wplabeler lbltest) kids
      childIsStmt   = or (map (testPoint lbltest) kids)
      stat          = or kstat
  in (stat && not childIsStmt, WNode lbl stat kids')

wplabeler :: (Label -> Bool) -> WeavePoint a -> (Bool, WeavePoint Bool)
wplabeler lbltest (Match a)      = let (stat,a') = labeler lbltest a in (stat, Match a')
wplabeler lbltest (Mismatch a b) = let (_,a') = labeler lbltest a
                                       (_,b') = labeler lbltest b
                                   in  (True, Mismatch a' b')
wplabeler lbltest (LeftHole h)   = let (_,h') = labeler lbltest h
                                   in  (True, LeftHole h')
wplabeler lbltest (RightHole h)  = let (_,h') = labeler lbltest h
                           in  (True, RightHole h')

testPoint :: (Label -> Bool) -> WeavePoint a -> Bool
testPoint lbltest (Match a)      = testNode lbltest a
testPoint lbltest (Mismatch a b) = testNode lbltest a || testNode lbltest b
testPoint lbltest (LeftHole a)   = testNode lbltest a
testPoint lbltest (RightHole a)  = testNode lbltest a

testNode :: (Label -> Bool) -> WeaveTree a -> Bool
testNode _       (WLeaf _)     = False
testNode lbltest (WNode l _ _) = lbltest l

extract :: WeaveTree a -> [LabeledTree]
extract wt = extractor isStmt (snd (labeler isStmt wt))

extractor :: (Label -> Bool) -> WeaveTree Bool -> [LabeledTree]
extractor lbltest w@(WNode l s kids) | lbltest l && s = [wl w True, wl w False]
                                     | otherwise      = concatMap (extractWP lbltest) kids
extractor _       (WLeaf t) = [t]

extractWP :: (Label -> Bool) -> WeavePoint Bool -> [LabeledTree]
extractWP lbltest (Match wt) = extractor lbltest wt
extractWP _       _          = []

