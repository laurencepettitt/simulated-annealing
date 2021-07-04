{- |Module      :  GTSP

    The module acts as an interface for different
    Travelling Salesman Problems
-}
module GTSP where

type Node = Int

type Cost = Double

type CostFunc = (Node, Node) -> Cost

class GTSP a where
    size :: a -> Int
    nodes :: a -> [Node]
    costFunc :: a -> CostFunc
