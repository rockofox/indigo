{-# LANGUAGE LambdaCase #-}

module Optimizer where

import VM

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
                Jz l' -> match l'
                Jnz l' -> match l'
                Call l' -> match l'
                CallLocal l' -> match l'
                PushPf l' _ -> match l'
                _ -> False
            )
      where
        match :: String -> Bool
        match x = (x == ln) || (labelBaseName x == labelBaseName ln)
    hasUsage _ = const True

    splitLast :: (Eq a) => a -> [a] -> Either [a] ([a], [a])
    splitLast c' = foldr go (Left [])
      where
        go c (Right (f, b)) = Right (c : f, b)
        go c (Left s)
            | c' == c = Right ([], s)
            | otherwise = Left (c : s)

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

    labelBaseName :: String -> String
    labelBaseName x = do
        let x' = takeWhile (/= '#') x
        let x'' = case splitLast ':' x' of
                Right (_, b) -> b
                Left _ -> x'
        x''

optimize :: [Instruction] -> [Instruction]
optimize = treeShake
