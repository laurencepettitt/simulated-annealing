module Data.TSP where

type Cost = Double
type Node = Int

type CostFunc = (Node, Node) -> Cost


-- Abstract Travelling Sales Problem
-- class GTSP c where
    -- Dimension of TSP
    -- dim :: c -> Int
    
    -- Cost function (edge -> weight)
    -- cost :: c -> CostFunc


-- data TSP = TSP {
    -- numNodes :: Int,
    -- edgeWeight :: CostFunc
-- }


-- instance GTSP TSP where
    -- dim = numNodes
    -- cost = edgeWeight