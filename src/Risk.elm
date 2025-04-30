module Risk exposing (..)

import Allele exposing (Allele, AllelePair, AllelePairId(..))
import Dict
import RiskData exposing (RiskDataBin, riskDict)
import Triplet exposing (Triplet(..))


type SizeGroup
    = Normal
    | Intermediate
    | Premutation
    | FullMutation


sizeGroupToString : SizeGroup -> String
sizeGroupToString sizeGroup =
    case sizeGroup of
        Normal ->
            "normal"

        Intermediate ->
            "intermediate"

        Premutation ->
            "PREMUTATION"

        FullMutation ->
            "FULL MUTATION"


overallSizeGroup : Allele -> SizeGroup
overallSizeGroup allele =
    let
        size =
            List.length allele.triplets
    in
    if size <= 44 then
        Normal

    else if size <= 54 then
        Intermediate

    else if size <= 200 then
        Premutation

    else
        FullMutation


getRisk : Allele -> Maybe RiskDataBin
getRisk allele =
    let

        -- Round size DOWN to nearest 5
        size =
            (List.length allele.triplets // 5) * 5

        aggCount =
            List.filter (\triplet -> triplet == Agg) allele.triplets |> List.length

        riskData =
            Dict.get ( size, aggCount ) riskDict
    in
    riskData


getRiskString : Allele -> String
getRiskString allele =
    let
        sizeGroup =
            overallSizeGroup allele

        numAgg =
            List.filter (\triplet -> triplet == Agg) allele.triplets |> List.length

        sizeFloored =
            (List.length allele.triplets // 5) * 5

        sizeBinUpper =
            sizeFloored + 4
    in
    case getRisk allele of
        Nothing ->
            case sizeGroup of
                Normal ->
                    "This allele is in the normal range and is not predicted to be at risk of expanding to a full mutation."

                Intermediate ->
                    "This allele is in the intermediate range. Intermediate alleles do not expand to a full mutation but may be at risk of expanding to a premutation in the next generation. No specific AGG interruption risk data is available."

                Premutation ->
                    "This allele is in the PREMUTATION range. Premutation alleles are at risk of expanding to a full mutation in the next generation. No specific AGG interruption risk data is available."

                FullMutation ->
                    "This allele is in the FULL MUTATION range and is diagnostic of Fragile X disease."

        Just riskData ->
            let
                totalTransmissions =
                    round riskData.totalTransmissions

                unstableTransmissions =
                    round riskData.unstableTransmissions

                numFullMutations =
                    round riskData.numFullMutations

                matRptChangeMin =
                    round riskData.matRptChangeMin

                matRptChangeMax =
                    round riskData.matRptChangeMax

                matRptChangeMinString = 
                    if matRptChangeMin < 0 then
                        String.fromInt (matRptChangeMin) 
                    else
                        "+" ++ String.fromInt matRptChangeMin
                matRptChangeMaxString = 
                    if matRptChangeMax < 0 then
                        String.fromInt (matRptChangeMax) 
                    else
                        "+" ++ String.fromInt matRptChangeMax

                medianMatRptChange =
                    riskData.medianMatRptChange

                medianMatRptChangeString = 
                    if medianMatRptChange < 0 then
                        String.fromFloat (medianMatRptChange) 
                    else
                        "+" ++ String.fromFloat medianMatRptChange
            in
            "This allele is in the "
                ++ sizeGroupToString sizeGroup
                ++ " range and has "
                ++ String.fromInt numAgg
                ++ " AGG interruptions. "
                ++ "In Nolin et al 2015 (PMID: 25210937), for alleles in the range "
                ++ String.fromInt sizeFloored
                ++ "-"
                ++ String.fromInt sizeBinUpper
                ++ " with "
                ++ String.fromInt numAgg
                ++ " AGG repeat/s, "
                ++ String.fromInt numFullMutations
                ++ " out of "
                ++ String.fromInt totalTransmissions
                ++ " transmissions ("
                ++ String.fromInt (round (toFloat numFullMutations / toFloat totalTransmissions * 100))
                ++ "%) became full mutations in the next generation. "
                ++ String.fromInt unstableTransmissions
                ++ " out of "
                ++ String.fromInt totalTransmissions
                ++ " transmissions ("
                ++ String.fromInt (round (toFloat unstableTransmissions / toFloat totalTransmissions * 100))
                ++ "%) were unstable, with a median repeat change during maternal transmission of "
                ++ medianMatRptChangeString
                ++ " repeats (range "
                ++ matRptChangeMinString
                ++ " to "
                ++ matRptChangeMaxString
                ++ ")."
