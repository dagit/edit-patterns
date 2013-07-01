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

data ExtractTree
  = ENode Label       [ExtractTree] (Maybe MismatchType)
  | ELeaf LabeledTree               (Maybe MismatchType)
  deriving Eq

data MismatchType
  = MismatchTypeLeft
  | MismatchTypeRight
  | LeftHoleType
  | RightHoleType
  deriving (Read,Show,Eq,Ord)

addInTree :: Tree a -> b -> Tree (a, b)
addInTree (Node lbl kids) b = Node (lbl,b) (map (`addInTree` b) kids)

extractToLabeled :: ExtractTree -> Tree (Label, Maybe MismatchType)
extractToLabeled (ELeaf t mb)        = t `addInTree` mb
extractToLabeled (ENode lbl kids mb) =
  let ks = (map extractToLabeled kids)
  in (Node (lbl,mb) ks)

wl2 :: WeaveTree a -> Bool -> Maybe MismatchType-> ExtractTree
wl2 (WLeaf l)          _   m = ELeaf l m
wl2 (WNode lbl _ kids) dir m = ENode lbl (concatMap (wpl2 dir) kids) m

wpl2 :: Bool -> WeavePoint a -> [ExtractTree]
wpl2 b     (Match t)      = [wl2 t b Nothing]
wpl2 True  (Mismatch a _) = [wl2 a True (Just MismatchTypeLeft)]
wpl2 False (Mismatch _ b) = [wl2 b False (Just MismatchTypeRight)]
wpl2 True  (LeftHole _)   = []
wpl2 False (LeftHole h)   = [wl2 h False (Just LeftHoleType)]
wpl2 False (RightHole _)  = []
wpl2 True  (RightHole h)  = [wl2 h True (Just RightHoleType)]

wl :: WeaveTree a -> Bool -> LabeledTree
wl (WLeaf l)          _   = l
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

extract2 :: WeaveTree a -> [Tree (Label, Maybe MismatchType)]
extract2 wt = map extractToLabeled (extractor2 isStmt (snd (labeler isStmt wt)))

extractor2 :: (Label -> Bool) -> WeaveTree Bool -> [ExtractTree]
extractor2 lbltest w@(WNode l s kids) | lbltest l && s = [wl2 w True  Nothing
                                                         ,wl2 w False Nothing]
                                      | otherwise      = concatMap (extractWP2 lbltest) kids
extractor2 _       (WLeaf t) = [ELeaf t Nothing]


extractor :: (Label -> Bool) -> WeaveTree Bool -> [LabeledTree]
extractor lbltest w@(WNode l s kids) | lbltest l && s = [wl w True, wl w False]
                                     | otherwise      = concatMap (extractWP lbltest) kids
extractor _       (WLeaf t) = [t]

extractWP :: (Label -> Bool) -> WeavePoint Bool -> [LabeledTree]
extractWP lbltest (Match wt) = extractor lbltest wt
extractWP _       _          = []

extractWP2 :: (Label -> Bool) -> WeavePoint Bool -> [ExtractTree]
extractWP2 lbltest (Match wt) = extractor2 lbltest wt
extractWP2 _       _          = []
