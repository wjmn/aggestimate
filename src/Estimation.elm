module Estimation exposing (..)

import Triplet exposing (Triplet(..))
import Allele exposing (Allele, AllelePair, AllelePairId(..))
import Allele exposing (AllelePair)
import Set


type alias EstimatedAggPositions = 
    { alleleA: Int
    , alleleB: Int }

type alias SelectedAggPosition = 
    { allele: AllelePairId
    , position: Int }

calculateEstimatedAggPosition : Allele -> Int -> Int 
calculateEstimatedAggPosition allele peakIndex =
    let
        numTriplets = List.length allele.triplets
    in
    numTriplets - peakIndex - 3 

calculateEstimatedAggPositions : AllelePair -> Int -> EstimatedAggPositions
calculateEstimatedAggPositions allelePair peakIndex =
    let
        estimatedAggPositionA = calculateEstimatedAggPosition allelePair.alleleA peakIndex
        estimatedAggPositionB = calculateEstimatedAggPosition allelePair.alleleB peakIndex
    in
    { alleleA = estimatedAggPositionA, alleleB = estimatedAggPositionB }

calculateAllEstimatedAggPositions : AllelePair -> List Int -> List EstimatedAggPositions
calculateAllEstimatedAggPositions allelePair peakIndices =
    -- Filter out peak indices with value 0 
    peakIndices |>  List.map (calculateEstimatedAggPositions allelePair)

getSelectedAggPositionsFromAlleles : AllelePair -> List SelectedAggPosition
getSelectedAggPositionsFromAlleles allelePair =
    let
        existingAggPositionsA = allelePair.alleleA.triplets |> List.indexedMap (\i t -> if t == Triplet.Agg then Just (i+1) else Nothing) |> List.filterMap identity
        existingAggPositionsB = allelePair.alleleB.triplets |> List.indexedMap (\i t -> if t == Triplet.Agg then Just (i+1) else Nothing) |> List.filterMap identity
    in 
    List.map (\i -> { allele = AlleleA, position = i }) existingAggPositionsA ++
    List.map (\i -> { allele = AlleleB, position = i }) existingAggPositionsB

getPeakIndicesFromAlleles : AllelePair -> List Int
getPeakIndicesFromAlleles allelePair =
    let
        alleleASize = List.length allelePair.alleleA.triplets
        alleleBSize = List.length allelePair.alleleB.triplets
        existingAggPositionsA = allelePair.alleleA.triplets |> List.indexedMap (\i t -> if t == Triplet.Agg then Just (alleleASize - (i+1) - 3) else Nothing) |> List.filterMap identity
        existingAggPositionsB = allelePair.alleleB.triplets |> List.indexedMap (\i t -> if t == Triplet.Agg then Just (alleleBSize - (i+1) - 3) else Nothing) |> List.filterMap identity
    in 
    existingAggPositionsA ++ existingAggPositionsB

getMostLikelyAggPosition : AllelePair -> EstimatedAggPositions -> Maybe SelectedAggPosition
getMostLikelyAggPosition allelePair estimatedAggPositions =
    let
        estimatedAggPositionA = estimatedAggPositions.alleleA
        estimatedAggPositionB = estimatedAggPositions.alleleB
        existingAggPositionsA = allelePair.alleleA.triplets |> List.indexedMap (\i t -> if t == Triplet.Agg then Just (i+1) else Nothing) |> List.filterMap identity |> Set.fromList
        existingAggPositionsB = allelePair.alleleB.triplets |> List.indexedMap (\i t -> if t == Triplet.Agg then Just (i+1) else Nothing) |> List.filterMap identity |> Set.fromList
    in
    -- If one is below zero, then choose the other
    if estimatedAggPositionA < 0 && estimatedAggPositionB < 0 then
        Just {allele = AlleleA, position = estimatedAggPositionA}
    else if estimatedAggPositionA < 0 then
        Just { allele = AlleleB, position = estimatedAggPositionB }
    else if estimatedAggPositionB < 0 then
        Just { allele = AlleleA, position = estimatedAggPositionA}
    else
        -- Otherwise if it already exists in one of the alleles, then choose that one
        if Set.member estimatedAggPositionA existingAggPositionsA then
            Just { allele = AlleleB, position = estimatedAggPositionB }
        else if Set.member estimatedAggPositionB existingAggPositionsB then
            Just { allele = AlleleA, position = estimatedAggPositionA }
        else
            -- Otherwise choose the one with the lowest value
            if estimatedAggPositionA < estimatedAggPositionB then
                Just { allele = AlleleA, position = estimatedAggPositionA }
            else
                Just { allele = AlleleB, position = estimatedAggPositionB }


makeAlleles : AllelePair -> List SelectedAggPosition -> AllelePair 
makeAlleles originalAllelePair selectedAggPositions = 
    let
        alleleASize = List.length originalAllelePair.alleleA.triplets
        alleleBSize = List.length originalAllelePair.alleleB.triplets
        alleleAPositions = List.filter (\p -> p.allele == AlleleA) selectedAggPositions |> List.map (.position) |> List.map (\x -> x - 1)
        alleleBPositions = List.filter (\p -> p.allele == AlleleB) selectedAggPositions |> List.map (.position) |> List.map (\x -> x - 1)
        newAlleleA = Allele.createWithSize alleleASize
        newAlleleAWithAgg = List.foldl (\p acc -> Allele.setTripletAtIndex p Triplet.Agg acc) newAlleleA alleleAPositions
        newAlleleB = Allele.createWithSize alleleBSize
        newAlleleBWithAgg = List.foldl (\p acc -> Allele.setTripletAtIndex p Triplet.Agg acc) newAlleleB alleleBPositions
    in 
    { alleleA = newAlleleAWithAgg, alleleB = newAlleleBWithAgg }