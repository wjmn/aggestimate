module Estimation exposing (..)

import Triplet exposing (Triplet(..))
import Allele exposing (Allele, AllelePair, AllelePairId(..))
import Allele exposing (AllelePair)
import Set
import Html exposing (select)


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
    numTriplets - (peakIndex - 4) - 3 

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
        existingAggPositionsA = allelePair.alleleA.triplets |> List.indexedMap (\i t -> if t == Triplet.Agg then Just (alleleASize - (i+1-4) - 3) else Nothing) |> List.filterMap identity
        existingAggPositionsB = allelePair.alleleB.triplets |> List.indexedMap (\i t -> if t == Triplet.Agg then Just (alleleBSize - (i+1-4) - 3) else Nothing) |> List.filterMap identity
    in 
    existingAggPositionsA ++ existingAggPositionsB

getMostLikelyAggPositions : List EstimatedAggPositions -> List SelectedAggPosition
getMostLikelyAggPositions estimatedAggPositions =
    -- Sequentially pick a position then add to a growing list
    let
        selectedAggPositions = 
            List.foldl (\est acc -> 
                let
                    positionsA = List.filter (\p -> p.allele == AlleleA) acc |> List.map (.position) |> Set.fromList
                    positionsB = List.filter (\p -> p.allele == AlleleB) acc |> List.map (.position) |> Set.fromList 
                    (allele, position) = 
                        if est.alleleA < 0 && est.alleleB < 0 then
                            (AlleleA, 0)
                        else if est.alleleA < 0 then
                            (AlleleB, est.alleleB)
                        else if est.alleleB < 0 then
                            (AlleleA, est.alleleA)
                        else
                            -- If occurs in accumulator positions
                            if List.member { allele = AlleleA, position = est.alleleA } acc then
                                (AlleleB, est.alleleB)
                            else if List.member { allele = AlleleB, position = est.alleleB } acc then
                                (AlleleA, est.alleleA)
                            else
                                -- If alleleA equals 10, 11, 20, 21, or 22, then choose that 
                                if (est.alleleA == 10 || est.alleleA == 11) && not (Set.member 10 positionsA) && not (Set.member 11 positionsA) then 
                                    (AlleleA, est.alleleA)
                                else if (est.alleleA == 20 || est.alleleA == 21 || est.alleleA == 22) && not (Set.member 20 positionsA) && not (Set.member 21 positionsA) && not (Set.member 22 positionsA) then
                                    (AlleleA, est.alleleA)
                                else if (est.alleleB == 10 || est.alleleB == 11) && not (Set.member 10 positionsB) && not (Set.member 11 positionsB) then
                                    (AlleleB, est.alleleB)
                                else if (est.alleleB == 20 || est.alleleB == 21 || est.alleleB == 22) && not (Set.member 20 positionsB) && not (Set.member 21 positionsB) && not (Set.member 22 positionsB) then
                                    (AlleleB, est.alleleB)
                                else
                                    -- If alleleA is less than alleleB, choose that
                                    if est.alleleA < est.alleleB then
                                        (AlleleA, est.alleleA)
                                    else
                                        (AlleleB, est.alleleB)

                in
                { allele = allele, position = position } :: acc) [] estimatedAggPositions
    in 
    selectedAggPositions |> List.reverse


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