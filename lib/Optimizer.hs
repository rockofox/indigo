{-# LANGUAGE LambdaCase #-}

module Optimizer where

import Data.List (isInfixOf, isPrefixOf)
import Data.Set qualified as Set
import VM

splitLast :: (Eq a) => a -> [a] -> Either [a] ([a], [a])
splitLast c' = foldr go (Left [])
  where
    go c (Right (f, b)) = Right (c : f, b)
    go c (Left s)
        | c' == c = Right ([], s)
        | otherwise = Left (c : s)

labelBaseName :: String -> String
labelBaseName x = do
    let x' = takeWhile (/= '#') x
    let x'' = case splitLast ':' x' of
            Right (_, b) -> b
            Left _ -> x'
    x''

groupByLabel :: [Instruction] -> [[Instruction]]
groupByLabel (x : xs) = case x of
    Label _ ->
        let (group, rest) =
                span
                    ( \case
                        Label _ -> False
                        _ -> True
                    )
                    xs
         in (x : group) : groupByLabel rest
    _ ->
        let (group, rest) =
                span
                    ( \case
                        Label _ -> False
                        _ -> True
                    )
                    (x : xs)
         in group : groupByLabel rest
groupByLabel [] = []

treeShake :: [Instruction] -> [Instruction]
treeShake prog = do
    let grouped = groupByLabel prog
    let (beforeSep, afterSep) = break (\case (Label l : _) -> labelBaseName l == "__sep"; _ -> False) grouped
    let mainProg = case afterSep of
            [] -> grouped
            (_ : rest) -> beforeSep ++ rest
    let usedByDeps = shake mainProg grouped
    concat usedByDeps
  where
    shakeOnce :: [[Instruction]] -> [[Instruction]] -> [[Instruction]]
    shakeOnce x =
        filter
            ( \case
                (label@(Label _) : _) -> do
                    hasUsage label (concat (filter (\case (l : _) -> l /= label; _ -> True) x))
                _ -> True
            )
    shake x y = do
        let x' = shakeOnce x y
        if x' == x then x else shake x' y
    hasUsage :: Instruction -> [Instruction] -> Bool
    hasUsage (Label "main") = const True
    hasUsage (Label "__sep") = const True
    hasUsage (Label ln) = do
        any
            ( \case
                Jmp l' -> match l'
                Jt l' -> match l'
                Jf l' -> match l'
                Call l' -> match l'
                CallLocal l' -> match l'
                TailCall l' -> match l'
                TailCallLocal l' -> match l'
                PushPf l' _ -> match l'
                _ -> False
            )
      where
        match :: String -> Bool
        match x = (x == ln) || (labelBaseName x == labelBaseName ln)
    hasUsage _ = const True

eliminateSideStack :: [Instruction] -> [Instruction]
eliminateSideStack prog =
    let groups = groupByLabel prog
        annotated = assignFunc "" groups
        funcsWithLoad =
            Set.fromList
                [ fn
                | (fn, instrs) <- annotated
                , LoadSideStack `elem` instrs
                ]
        removeSideStack = filter (\case StoreSideStack -> False; ClearSideStack -> False; _ -> True)
        transform (fn, instrs)
            | not (null fn) && fn `Set.notMember` funcsWithLoad = removeSideStack instrs
            | otherwise = instrs
     in concatMap transform annotated
  where
    assignFunc _ [] = []
    assignFunc current (group@(Label l : _) : rest)
        | isInternalLabel l = (current, group) : assignFunc current rest
        | otherwise =
            let fn = labelBaseName l
             in (fn, group) : assignFunc fn rest
    assignFunc current (group : rest) = (current, group) : assignFunc current rest

    isInternalLabel l =
        any (`isPrefixOf` l) ["else", "end", "when_end", "when_next", "__tuple_"]
            || "_nested_" `isInfixOf` l

strengthReduction :: [Instruction] -> [Instruction]
strengthReduction = \case
    Push (DFloat 2.0) : Pow : rest -> Dup : Mul : strengthReduction rest
    Push (DDouble 2.0) : Pow : rest -> Dup : Mul : strengthReduction rest
    Push (DInt 2) : Pow : rest -> Dup : Mul : strengthReduction rest
    Push (DInt 1) : Mul : rest -> strengthReduction rest
    Push (DFloat 1.0) : Mul : rest -> strengthReduction rest
    Push (DDouble 1.0) : Mul : rest -> strengthReduction rest
    Push (DInt 0) : Add : rest -> strengthReduction rest
    Push (DFloat 0.0) : Add : rest -> strengthReduction rest
    Push (DDouble 0.0) : Add : rest -> strengthReduction rest
    Push (DInt 0) : Sub : rest -> strengthReduction rest
    Push (DFloat 0.0) : Sub : rest -> strengthReduction rest
    Push (DDouble 0.0) : Sub : rest -> strengthReduction rest
    Push (DInt a) : Push (DInt b) : Add : rest -> strengthReduction (Push (DInt (a + b)) : rest)
    Push (DInt a) : Push (DInt b) : Sub : rest -> strengthReduction (Push (DInt (a - b)) : rest)
    Push (DInt a) : Push (DInt b) : Mul : rest -> strengthReduction (Push (DInt (a * b)) : rest)
    Push (DFloat a) : Push (DFloat b) : Add : rest -> strengthReduction (Push (DFloat (a + b)) : rest)
    Push (DFloat a) : Push (DFloat b) : Sub : rest -> strengthReduction (Push (DFloat (a - b)) : rest)
    Push (DFloat a) : Push (DFloat b) : Mul : rest -> strengthReduction (Push (DFloat (a * b)) : rest)
    x : xs -> x : strengthReduction xs
    [] -> []

optimize :: [Instruction] -> [Instruction]
optimize = strengthReduction . eliminateSideStack . treeShake
