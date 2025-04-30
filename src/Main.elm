module Main exposing (..)

import Allele exposing (Allele, AllelePair, AllelePairId(..))
import Array exposing (Array)
import Array.Extra
import Browser
import Estimation exposing (EstimatedAggPositions, SelectedAggPosition, calculateEstimatedAggPosition)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import SimAgg
import SimAggRev
import SimTriplet
import Svg exposing (svg)
import Svg.Attributes as SA
import Triplet exposing (Triplet)
import List.Extra exposing (last)



---- MODEL ----


type alias Model =
    { allelePair : AllelePair
    , lastAggPositionsA : List Int
    , lastAggPositionsB : List Int
    , aggPeakPositions : List Int
    , aggInterruptionEstimates : List EstimatedAggPositions
    , selectedAggPositions : List SelectedAggPosition
    , simTripletValues : Array Int
    , simAggValues : Array Int
    , simAggRevValues : Array Int
    }


initialModel : Model
initialModel =
    let
        initialPair =
            { alleleA = Allele.createWithSize 20
            , alleleB = Allele.createWithSize 24
            }

        peakIndices =
            Estimation.getPeakIndicesFromAlleles initialPair

        estimatedAggPositions =
            Estimation.calculateAllEstimatedAggPositions initialPair peakIndices
    in
    { allelePair = initialPair
    , lastAggPositionsA = []
    , lastAggPositionsB = []
    , aggPeakPositions = peakIndices
    , aggInterruptionEstimates = estimatedAggPositions
    , selectedAggPositions = Estimation.getSelectedAggPositionsFromAlleles initialPair
    , simTripletValues = SimTriplet.calculateFragmentDistribution initialPair
    , simAggValues = SimAgg.calculateFragmentDistribution initialPair
    , simAggRevValues = SimAggRev.calculateFragmentDistribution initialPair
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = ChangedAlleleSize AllelePairId String
    | ChangedAlleleSizeTable AllelePairId String
    | ClickedAlleleBlock AllelePairId Int
    | ClickedAddPeakIndex
    | ChangedPeakIndex Int Int
    | SelectedEstimatedPeak Int AllelePairId
    | ClickedDeletePeakIndex Int
    | ClickedReset
    | NoOp


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd model =
    ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedAlleleSize allelePairId string ->
            let
                newSize =
                    case String.toInt string of
                        Just size ->
                            if size >= 0 && size <= Allele.maxAlleleSize then
                                size

                            else if size < 0 then
                                0

                            else
                                Allele.maxAlleleSize

                        Nothing ->
                            0

                allelePair =
                    model.allelePair

                newAllelePair =
                    case allelePairId of
                        AlleleA ->
                            { allelePair | alleleA = List.foldl (\i acc -> Allele.setTripletAtIndex i Triplet.Agg acc) (Allele.changeTripletLength newSize model.allelePair.alleleA) model.lastAggPositionsA }

                        AlleleB ->
                            { allelePair | alleleB = List.foldl (\i acc -> Allele.setTripletAtIndex i Triplet.Agg acc) (Allele.changeTripletLength newSize model.allelePair.alleleB) model.lastAggPositionsB }

                newPeakIndices =
                    Estimation.getPeakIndicesFromAlleles newAllelePair

                newEstimatedAggPositions =
                    Estimation.calculateAllEstimatedAggPositions newAllelePair newPeakIndices

                newSelectedAggPositions =
                    Estimation.getSelectedAggPositionsFromAlleles newAllelePair

                newCalculatedSimTripletValues =
                    SimTriplet.calculateFragmentDistribution newAllelePair

                newCalculatedSimAggValues =
                    SimAgg.calculateFragmentDistribution newAllelePair

                newCalculatedSimAggRevValues =
                    SimAggRev.calculateFragmentDistribution newAllelePair
            in
            { model
                | allelePair = newAllelePair
                , simTripletValues = newCalculatedSimTripletValues
                , aggPeakPositions = newPeakIndices
                , aggInterruptionEstimates = newEstimatedAggPositions
                , selectedAggPositions = newSelectedAggPositions
                , simAggValues = newCalculatedSimAggValues
                , simAggRevValues = newCalculatedSimAggRevValues
            }
                |> withCmd Cmd.none

        ChangedAlleleSizeTable allelePairId string ->
            -- Similar to changing allele size, but do not change the peak indices
            let
                newSize =
                    case String.toInt string of
                        Just size ->
                            if size >= 0 && size <= Allele.maxAlleleSize then
                                size

                            else if size < 0 then
                                0

                            else
                                Allele.maxAlleleSize

                        Nothing ->
                            0

                allelePair =
                    model.allelePair

                newAllelePairTemp =
                    case allelePairId of
                        AlleleA ->
                            { allelePair | alleleA = Allele.changeTripletLength newSize model.allelePair.alleleA }

                        AlleleB ->
                            { allelePair | alleleB = Allele.changeTripletLength newSize model.allelePair.alleleB }

                newEstimatedAggPositions =
                    Estimation.calculateAllEstimatedAggPositions newAllelePairTemp model.aggPeakPositions

                newSelectedAggPositions =
                    Estimation.getMostLikelyAggPositions newEstimatedAggPositions

                newAllellePair =
                    Estimation.makeAlleles newAllelePairTemp newSelectedAggPositions

                lastAggPositionsA =
                    -- just get fresh AGG positions from alleleA
                    List.indexedMap
                        (\i t ->
                            if t == Triplet.Agg then
                                Just i

                            else
                                Nothing
                        )
                        newAllellePair.alleleA.triplets
                        |> List.filterMap identity

                lastAggPositionsB =
                    -- just get fresh AGG positions from alleleB
                    List.indexedMap
                        (\i t ->
                            if t == Triplet.Agg then
                                Just i

                            else
                                Nothing
                        )
                        newAllellePair.alleleB.triplets
                        |> List.filterMap identity

                newCalculatedSimTripletValues =
                    SimTriplet.calculateFragmentDistribution newAllellePair

                newCalculatedSimAggValues =
                    SimAgg.calculateFragmentDistribution newAllellePair

                newCalculatedSimAggRevValues =
                    SimAggRev.calculateFragmentDistribution newAllellePair
            in
            { model
                | allelePair = newAllellePair
                , lastAggPositionsA = lastAggPositionsA
                , lastAggPositionsB = lastAggPositionsB
                , simTripletValues = newCalculatedSimTripletValues
                , aggInterruptionEstimates = newEstimatedAggPositions
                , selectedAggPositions = newSelectedAggPositions
                , simAggValues = newCalculatedSimAggValues
                , simAggRevValues = newCalculatedSimAggRevValues
            }
                |> withCmd Cmd.none

        ClickedAlleleBlock allelePairId index ->
            let
                allelePair =
                    model.allelePair

                newAllelePair =
                    case allelePairId of
                        AlleleA ->
                            { allelePair | alleleA = Allele.cycleTripletAtIndex index allelePair.alleleA }

                        AlleleB ->
                            { allelePair | alleleB = Allele.cycleTripletAtIndex index allelePair.alleleB }

                lastAggPositionsA =
                    case allelePairId of
                        AlleleA ->
                            if Allele.getTripletAtIndex index allelePair.alleleA == Just Triplet.Agg then
                                List.filter (\i -> i /= index) model.lastAggPositionsA

                            else
                                index :: model.lastAggPositionsA

                        AlleleB ->
                            model.lastAggPositionsA

                lastAggPositionsB =
                    case allelePairId of
                        AlleleB ->
                            if Allele.getTripletAtIndex index allelePair.alleleB == Just Triplet.Agg then
                                List.filter (\i -> i /= index) model.lastAggPositionsB

                            else
                                index :: model.lastAggPositionsB

                        AlleleA ->
                            model.lastAggPositionsB

                newPeakIndices =
                    Estimation.getPeakIndicesFromAlleles newAllelePair

                newEstimatedAggPositions =
                    Estimation.calculateAllEstimatedAggPositions newAllelePair newPeakIndices

                newSelectedAggPositions =
                    Estimation.getSelectedAggPositionsFromAlleles newAllelePair

                newCalculatedSimTripletValues =
                    SimTriplet.calculateFragmentDistribution newAllelePair

                newCalculatedSimAggValues =
                    SimAgg.calculateFragmentDistribution newAllelePair

                newCalculatedSimAggRevValues =
                    SimAggRev.calculateFragmentDistribution newAllelePair
            in
            { model
                | allelePair = newAllelePair
                , lastAggPositionsA = lastAggPositionsA
                , lastAggPositionsB = lastAggPositionsB
                , simTripletValues = newCalculatedSimTripletValues
                , aggPeakPositions = newPeakIndices
                , aggInterruptionEstimates = newEstimatedAggPositions
                , selectedAggPositions = newSelectedAggPositions
                , simAggValues = newCalculatedSimAggValues
                , simAggRevValues = newCalculatedSimAggRevValues
            }
                |> withCmd Cmd.none

        ClickedAddPeakIndex ->
            let
                -- Add a new peak index with default value 0  to end of aggPeakPositions
                newPeakIndices =
                    model.aggPeakPositions ++ [ 5 ]

                selectedAggPositions =
                    model.selectedAggPositions ++ [ { allele = AlleleA, position = calculateEstimatedAggPosition model.allelePair.alleleA 5 } ]

                newAllelePair =
                    -- Make a new allele pair with the new peak indices
                    Estimation.makeAlleles model.allelePair selectedAggPositions

                newCalculatedSimTripletValues =
                    SimTriplet.calculateFragmentDistribution newAllelePair

                newCalculatedSimAggValues =
                    SimAgg.calculateFragmentDistribution newAllelePair

                newCalculatedSimAggRevValues =
                    SimAggRev.calculateFragmentDistribution newAllelePair
            in
            { model | aggPeakPositions = newPeakIndices, selectedAggPositions = selectedAggPositions, allelePair = newAllelePair, simTripletValues = newCalculatedSimTripletValues, simAggValues = newCalculatedSimAggValues, simAggRevValues = newCalculatedSimAggRevValues }
                |> withCmd Cmd.none

        ChangedPeakIndex index value ->
            let
                -- Update the peak index at the given index with the new value
                newPeakIndices =
                    List.indexedMap
                        (\i v ->
                            if i == index then
                                value

                            else
                                v
                        )
                        model.aggPeakPositions

                newEstimatedAggPositions =
                    Estimation.calculateAllEstimatedAggPositions model.allelePair newPeakIndices

                newSelectedAggPositions =
                    Estimation.getMostLikelyAggPositions newEstimatedAggPositions

                -- Update the alleles
                newAllellePair =
                    -- Make a new allele pair with the new peak indices
                    Estimation.makeAlleles model.allelePair newSelectedAggPositions

                lastAggPositionsA =
                    -- just get fresh AGG positions from alleleA
                    List.indexedMap
                        (\i t ->
                            if t == Triplet.Agg then
                                Just i

                            else
                                Nothing
                        )
                        newAllellePair.alleleA.triplets
                        |> List.filterMap identity

                lastAggPositionsB =
                    -- just get fresh AGG positions from alleleB
                    List.indexedMap
                        (\i t ->
                            if t == Triplet.Agg then
                                Just i

                            else
                                Nothing
                        )
                        newAllellePair.alleleB.triplets
                        |> List.filterMap identity

                newCalculatedSimTripletValues =
                    SimTriplet.calculateFragmentDistribution newAllellePair

                newCalculatedSimAggValues =
                    SimAgg.calculateFragmentDistribution newAllellePair

                newCalculatedSimAggRevValues =
                    SimAggRev.calculateFragmentDistribution newAllellePair
            in
            { model
                | aggPeakPositions = newPeakIndices
                , lastAggPositionsA = lastAggPositionsA
                , lastAggPositionsB = lastAggPositionsB
                , aggInterruptionEstimates = newEstimatedAggPositions
                , selectedAggPositions = newSelectedAggPositions
                , allelePair = newAllellePair
                , simTripletValues = newCalculatedSimTripletValues
                , simAggValues = newCalculatedSimAggValues
                , simAggRevValues = newCalculatedSimAggRevValues
            }
                |> withCmd Cmd.none

        SelectedEstimatedPeak index allelePairId ->
            let
                alleleSelected =
                    case allelePairId of
                        AlleleA ->
                            model.allelePair.alleleA

                        AlleleB ->
                            model.allelePair.alleleB

                -- Update the selected peak index at the given index with the new value
                newSelectedAggPositions =
                    List.indexedMap
                        (\i v ->
                            if i == index then
                                { allele = allelePairId, position = calculateEstimatedAggPosition alleleSelected (List.Extra.getAt index model.aggPeakPositions |> Maybe.withDefault 0) }

                            else
                                v
                        )
                        model.selectedAggPositions

                newAllelePair =
                    -- Make a new allele pair with the new peak indices
                    Estimation.makeAlleles model.allelePair newSelectedAggPositions
                lastAggPositionsA = 
                    -- just get fresh AGG positions from alleleA
                    List.indexedMap
                        (\i t ->
                            if t == Triplet.Agg then
                                Just i

                            else
                                Nothing
                        )
                        newAllelePair.alleleA.triplets
                        |> List.filterMap identity
                lastAggPositionsB = 
                    -- just get fresh AGG positions from alleleB
                    List.indexedMap
                        (\i t ->
                            if t == Triplet.Agg then
                                Just i

                            else
                                Nothing
                        )
                        newAllelePair.alleleB.triplets
                        |> List.filterMap identity


                newCalculatedSimTripletValues =
                    SimTriplet.calculateFragmentDistribution newAllelePair

                newCalculatedSimAggValues =
                    SimAgg.calculateFragmentDistribution newAllelePair

                newCalculatedSimAggRevValues =
                    SimAggRev.calculateFragmentDistribution newAllelePair
            in
            { model
                | selectedAggPositions = newSelectedAggPositions
                , allelePair = newAllelePair
                , lastAggPositionsA = lastAggPositionsA
                , lastAggPositionsB = lastAggPositionsB
                , simTripletValues = newCalculatedSimTripletValues
                , simAggValues = newCalculatedSimAggValues
                , simAggRevValues = newCalculatedSimAggRevValues
            }
                |> withCmd Cmd.none

        ClickedDeletePeakIndex index ->
            let
                -- Remove the peak index at the given index
                newPeakIndices =
                    -- Remove at index
                    List.Extra.removeAt index model.aggPeakPositions

                newEstimatedAggPositions =
                    Estimation.calculateAllEstimatedAggPositions model.allelePair newPeakIndices

                newSelectedAggPositions =
                    Estimation.getMostLikelyAggPositions newEstimatedAggPositions

                -- Update the alleles
                newAllellePair =
                    -- Make a new allele pair with the new peak indices
                    Estimation.makeAlleles model.allelePair newSelectedAggPositions

                lastAggPositionsA =
                    -- just get fresh AGG positions from alleleA
                    List.indexedMap
                        (\i t ->
                            if t == Triplet.Agg then
                                Just i

                            else
                                Nothing
                        )
                        newAllellePair.alleleA.triplets
                        |> List.filterMap identity

                lastAggPositionsB =
                    -- just get fresh AGG positions from alleleB
                    List.indexedMap
                        (\i t ->
                            if t == Triplet.Agg then
                                Just i

                            else
                                Nothing
                        )
                        newAllellePair.alleleB.triplets
                        |> List.filterMap identity

                newCalculatedSimTripletValues =
                    SimTriplet.calculateFragmentDistribution newAllellePair

                newCalculatedSimAggValues =
                    SimAgg.calculateFragmentDistribution newAllellePair

                newCalculatedSimAggRevValues =
                    SimAggRev.calculateFragmentDistribution newAllellePair
            in
            { model
                | aggPeakPositions = newPeakIndices
                , lastAggPositionsA = lastAggPositionsA
                , lastAggPositionsB = lastAggPositionsB
                , aggInterruptionEstimates = newEstimatedAggPositions
                , selectedAggPositions = newSelectedAggPositions
                , allelePair = newAllellePair
                , simTripletValues = newCalculatedSimTripletValues
                , simAggValues = newCalculatedSimAggValues
                , simAggRevValues = newCalculatedSimAggRevValues
            }
                |> withCmd Cmd.none

        ClickedReset ->
            ( initialModel, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "main" ]
        [ div [ id "section-top" ]
            [ div [ id "section-inputs" ]
                [ h1 [ class "title" ] [ text "AGGEstimate" ]
                , h2 [ class "subtitle" ] [ text "Input the allele sizes and the positions at which an AGG dip starts (first peak = position 5) e.g." ]
                , img [ class "example-image", src "example.png", alt "Example" ] []
                , div [] [ viewPeakSizeInputs model ]
                ]
            , div [ id "section-outputs" ]
                [ div [ id "section-top-right-bordered" ]
                    [ div [ class "allele-section-heading" ] [ text "Fragile X Genotype" ]
                    , viewAllele AlleleA model.allelePair.alleleA
                    , viewAllele AlleleB model.allelePair.alleleB
                    ]
                , viewSimTriplet model
                , viewSimAgg model
                , viewSimAggRev model
                ]
            ]
        ]


viewPeakSizeInputs : Model -> Html Msg
viewPeakSizeInputs model =
    div [ id "peak-size-inputs" ]
        -- table with 3 columns
        [ table [ id "peak-size-inputs-table" ]
            ([ tr []
                [ th [] []
                , th [] []
                , th [ colspan 2, class "table-header-alleles" ] [ text "Alleles" ]
                , th [] []
                ]
             , tr []
                [ th [ colspan 2 ] [ button [ class "reset-button", onClick ClickedReset ] [ text "Reset" ] ]
                , th [] [ input [ type_ "number", value (String.fromInt (Allele.getSize model.allelePair.alleleA)), onInput (ChangedAlleleSizeTable AlleleA) ] [] ]
                , th [] [ input [ type_ "number", value (String.fromInt (Allele.getSize model.allelePair.alleleB)), onInput (ChangedAlleleSizeTable AlleleB) ] [] ]
                , th [] []
                ]
             ]
                ++ List.indexedMap
                    (\i ( peakIndex, selectedAtIndex ) ->
                        let
                            ( classA, classB ) =
                                if selectedAtIndex.allele == AlleleA then
                                    ( "selected", "not-selected" )

                                else if selectedAtIndex.allele == AlleleB then
                                    ( "not-selected", "selected" )

                                else
                                    ( "unknown", "unknown" )

                            extraElementAdder =
                                if i == 0 then
                                    \x -> td [ rowspan (List.length model.aggPeakPositions), class "table-header-dips" ] [ text "Dips" ] :: x

                                else
                                    identity
                        in
                        tr []
                            (extraElementAdder
                                [ td [] [ input [ type_ "number", value (String.fromInt peakIndex), onInput (String.toInt >> Maybe.withDefault 0 >> ChangedPeakIndex i) ] [] ]
                                , td [ class "table-value", class classA, onClick (SelectedEstimatedPeak i AlleleA) ] [ text (String.fromInt (Estimation.calculateEstimatedAggPosition model.allelePair.alleleA peakIndex)) ]
                                , td [ class "table-value", class classB, onClick (SelectedEstimatedPeak i AlleleB) ] [ text (String.fromInt (Estimation.calculateEstimatedAggPosition model.allelePair.alleleB peakIndex)) ]
                                , td [ class "delete-peak-index", onClick (ClickedDeletePeakIndex i) ]
                                    [ text "X" ]
                                ]
                            )
                    )
                    (List.map2 Tuple.pair model.aggPeakPositions model.selectedAggPositions)
                ++ [ tr []
                        [ td [ colspan 4 ] [ button [ class "add-peak-index-button", onClick ClickedAddPeakIndex ] [ text "Add AGG Dip" ] ] ]
                   ]
            )
        ]


viewAlleleVisualTriplet : AllelePairId -> Int -> Triplet -> Html Msg
viewAlleleVisualTriplet allelePairId index triplet =
    div [ class "allele-visual-block", class (Triplet.toString triplet), onClick (ClickedAlleleBlock allelePairId index) ]
        [ div [ class "allele-visual-block-text" ]
            [ text (String.fromInt (1 + index)) ]
        ]


inertBlock : String -> Html Msg
inertBlock inner =
    div [ class "allele-visual-block-inert" ] [ text inner ]


viewAllele : AllelePairId -> Allele -> Html Msg
viewAllele allelePairId allele =
    div [ class "allele-container" ]
        [ div [ class "allele-top" ]
            [ div [ class "allele-label-name" ] [ text ("Allele " ++ Allele.allelePairIdToString allelePairId ++ ":") ]
            , div [ class "allele-label-repeats" ]
                [ input [ class "allele-input", type_ "number", value (Allele.getSize allele |> String.fromInt), onInput (ChangedAlleleSize allelePairId) ] []
                , text " repeats ="
                ]
            , div [ class "allele-label-string" ] [ text (Allele.toGroupedString allele) ]
            ]
        , div [ class "allele-bottom" ]
            [ div [ class "allele-visual-container" ]
                (inertBlock "5'" :: List.indexedMap (viewAlleleVisualTriplet allelePairId) allele.triplets ++ [ inertBlock "3'" ])
            ]
        ]


viewSimTriplet : Model -> Html Msg
viewSimTriplet model =
    let
        xmax =
            SimTriplet.primerRDistance + SimTriplet.primerRLength + (Allele.maxAlleleSize * Triplet.size) + SimTriplet.primerFDistance + SimTriplet.primerFLength + SimTriplet.tripletPrimerAddLength + 10

        xmin =
            80

        xminString =
            String.fromInt xmin

        viewBoxString =
            xminString ++ " 1 " ++ String.fromInt xmax ++ " 112"

        -- Get location of two full sized alleles
        fullAlleleAUnsorted =
            SimTriplet.getFullFragmentSize model.allelePair.alleleA

        alleleASizeUnsorted =
            Allele.getSize model.allelePair.alleleA

        fullAlleleBUnsorted =
            SimTriplet.getFullFragmentSize model.allelePair.alleleB

        alleleBSizeUnsorted =
            Allele.getSize model.allelePair.alleleB

        ( alleleASize, alleleBSize ) =
            if alleleASizeUnsorted > alleleBSizeUnsorted then
                ( alleleASizeUnsorted, alleleBSizeUnsorted )

            else
                ( alleleBSizeUnsorted, alleleASizeUnsorted )

        ( fullAlleleA, fullAlleleB ) =
            if fullAlleleAUnsorted > fullAlleleBUnsorted then
                ( fullAlleleAUnsorted, fullAlleleBUnsorted )

            else
                ( fullAlleleBUnsorted, fullAlleleAUnsorted )
    in
    div [ class "sim-container" ]
        [ div [ class "sim-graph" ]
            [ svg [ SA.viewBox viewBoxString ]
                [ -- draw rectangles at the two full alleles
                  Svg.g []
                    [ Svg.rect [ SA.x (String.fromFloat <| toFloat fullAlleleA - 1.5), SA.y "11", SA.width "3", SA.height "89", SA.fill "#ccc", SA.opacity "0.5" ] []

                    -- put text in grey box
                    , Svg.rect [ SA.x (String.fromFloat <| toFloat fullAlleleA - 10), SA.y "101", SA.width "20", SA.height "11", SA.fill "#ccc", SA.opacity "0.5", SA.stroke "green" ] []
                    , Svg.text_ [ SA.class "no-pointer", SA.x (String.fromInt fullAlleleA), SA.y "110", SA.fill "black", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt alleleASize) ]
                    , Svg.rect [ SA.x (String.fromFloat <| toFloat fullAlleleB - 1.5), SA.y "11", SA.width "3", SA.height "89", SA.fill "#ccc", SA.opacity "0.5" ] []
                    , Svg.rect [ SA.x (String.fromFloat <| toFloat fullAlleleB - 10), SA.y "101", SA.width "20", SA.height "11", SA.fill "#ccc", SA.opacity "0.5", SA.stroke "green" ] []
                    , Svg.text_ [ SA.class "no-pointer", SA.x (String.fromInt fullAlleleB), SA.y "110", SA.fill "black", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt alleleBSize) ]
                    ]
                , Svg.line
                    [ SA.x1 xminString, SA.y1 "100", SA.x2 "1000", SA.y2 "100", SA.stroke "blue" ]
                    []
                , Svg.g []
                    [ Svg.polyline
                        [ SA.points
                            (SimTriplet.arrayValuesToPolylinePoints model.simTripletValues
                                |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat (100 - 10 * y))
                                |> String.join " "
                            )
                        , SA.stroke "blue"
                        , SA.fill "none"
                        , SA.strokeLinejoin "round"
                        ]
                        []
                    ]
                , Svg.g []
                    [ Svg.rect [ SA.x (String.fromInt (SimTriplet.primerRDistance + SimTriplet.primerRLength + 15)), SA.y "1", SA.width (String.fromInt (xmax - 10 - 15 - SimTriplet.primerRDistance - SimTriplet.primerRLength)), SA.height "10", SA.fill "#eee", SA.stroke "green" ] []
                    , Svg.rect [ SA.x xminString, SA.y "11", SA.width (String.fromInt xmax), SA.height "89", SA.fill "none", SA.stroke "black" ] []
                    , Svg.text_ [ SA.x "510", SA.y "9", SA.fill "#222", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text "FRAX" ]
                    , Svg.text_ [ SA.x (String.fromInt (xmin + 4)), SA.y "24", SA.fill "#222", SA.fontSize "10px", SA.textAnchor "left" ] [ Svg.text "Triplet-repeat primed PCR" ]
                    ]

                -- create small rectangles in increments of 3 along the entire x axis
                , Svg.g []
                    (List.range 0 (1000 // 3)
                        |> List.map
                            (\i ->
                                Svg.g [ SA.class "graph-vline" ]
                                    [ Svg.rect [ SA.x (String.fromFloat (toFloat i * 3 - 1.5)), SA.y "11", SA.width "3", SA.height "89", SA.fill "#ccc" ] []
                                    , Svg.text_ [ SA.class "no-pointer", SA.x (String.fromFloat (toFloat i * 3 - 1.5)), SA.y "110", SA.fill "black", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt (i * 3) ++ "bp") ]
                                    ]
                            )
                    )
                ]
            ]
        ]


viewSimAgg : Model -> Html Msg
viewSimAgg model =
    let
        -- Get array positions of any value above 0
        aboveZeroIndices =
            model.simAggValues
                |> Array.Extra.indexedMapToList
                    (\i v ->
                        if v > 0 then
                            Just i

                        else
                            Nothing
                    )
                |> List.filterMap identity

        xmin =
            80

        xminString =
            String.fromInt xmin

        xmax =
            SimTriplet.primerRDistance + SimTriplet.primerRLength + (Allele.maxAlleleSize * Triplet.size) + SimTriplet.primerFDistance + SimTriplet.primerFLength + SimTriplet.tripletPrimerAddLength + 10

        xmaxString =
            String.fromInt xmax

        viewBoxString =
            xminString ++ " 1 " ++ xmaxString ++ " 110"
    in
    div [ class "sim-container" ]
        [ div [ class "sim-graph" ]
            [ svg [ SA.viewBox viewBoxString ]
                [ Svg.g []
                    [ Svg.polyline
                        [ SA.points
                            (SimAgg.arrayValuesToPolylinePoints model.simAggValues
                                |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat (100 - 10 * y))
                                |> String.join " "
                            )
                        , SA.stroke "blue"
                        , SA.fill "none"
                        , SA.strokeLinejoin "round"
                        ]
                        []
                    ]

                -- Axis rectangle
                , Svg.g []
                    [ Svg.rect [ SA.x xminString, SA.y "1", SA.width (String.fromInt xmax), SA.height "99", SA.fill "none", SA.stroke "black" ] []
                    , Svg.text_ [ SA.x (String.fromInt (xmin + 4)), SA.y "14", SA.fill "#222", SA.fontSize "10px", SA.textAnchor "left" ] [ Svg.text "A-primed PCR" ]
                    ]

                -- Add text above all aboveZeroIndices
                , Svg.g []
                    (List.map
                        (\i ->
                            Svg.text_ [ SA.x (String.fromFloat (toFloat i)), SA.y "110", SA.fill "#222", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt i ++ "bp") ]
                        )
                        aboveZeroIndices
                    )
                ]
            ]
        ]


viewSimAggRev : Model -> Html Msg
viewSimAggRev model =
    let
        -- Get array positions of any value above 0
        aboveZeroIndices =
            model.simAggRevValues
                |> Array.Extra.indexedMapToList
                    (\i v ->
                        if v > 0 then
                            Just i

                        else
                            Nothing
                    )
                |> List.filterMap identity

        xmin =
            80

        xminString =
            String.fromInt xmin

        xmax =
            SimTriplet.primerRDistance + SimTriplet.primerRLength + (Allele.maxAlleleSize * Triplet.size) + SimTriplet.primerFDistance + SimTriplet.primerFLength + SimTriplet.tripletPrimerAddLength + 10

        xmaxString =
            String.fromInt xmax

        viewBoxString =
            xminString ++ " 1 " ++ xmaxString ++ " 110"
    in
    div [ class "sim-container" ]
        [ div [ class "sim-graph" ]
            [ svg [ SA.viewBox viewBoxString ]
                [ Svg.g []
                    [ Svg.polyline
                        [ SA.points
                            (SimAggRev.arrayValuesToPolylinePoints model.simAggRevValues
                                |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat (100 - 10 * y))
                                |> String.join " "
                            )
                        , SA.stroke "blue"
                        , SA.fill "none"
                        , SA.strokeLinejoin "round"
                        ]
                        []
                    ]

                -- Axis rectangle
                , Svg.g []
                    [ Svg.rect [ SA.x xminString, SA.y "1", SA.width (String.fromInt xmax), SA.height "99", SA.fill "none", SA.stroke "black" ] []
                    , Svg.text_ [ SA.x (String.fromInt (xmin + 4)), SA.y "14", SA.fill "#222", SA.fontSize "10px", SA.textAnchor "left" ] [ Svg.text "T-primed PCR" ]
                    ]

                -- Add text above all aboveZeroIndices
                , Svg.g []
                    (List.map
                        (\i ->
                            Svg.text_ [ SA.x (String.fromFloat (toFloat i)), SA.y "110", SA.fill "#222", SA.fontSize "8px", SA.textAnchor "middle" ] [ Svg.text (String.fromInt i ++ "bp") ]
                        )
                        aboveZeroIndices
                    )
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
