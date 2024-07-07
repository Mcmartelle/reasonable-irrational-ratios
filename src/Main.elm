module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Chart as C
import Chart.Attributes as CA
import Color exposing (hsl, toCssString)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, style, type_, value)
import Html.Events exposing (..)
import Random
import Svg exposing (Svg, circle, line, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { seed : Float
    , goldenRatioValues : List Float
    , goldenRatioHistogram : List Datum
    , silverRatioValues : List Float
    , silverRatioHistogram : List Datum
    , eRatioValues : List Float
    , eRatioHistogram : List Datum
    , piRatioValues : List Float
    , piRatioHistogram : List Datum
    , whiteNoiseValues : List Float
    , whiteNoiseHistogram : List Datum
    , blueNoiseValues : List Float
    , blueNoiseHistogram : List Datum
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = seed
      , goldenRatioValues = [ seed ]
      , goldenRatioHistogram = addToHistogram initialHistogram seed
      , silverRatioValues = [ seed ]
      , silverRatioHistogram = addToHistogram initialHistogram seed
      , eRatioValues = [ seed ]
      , eRatioHistogram = addToHistogram initialHistogram seed
      , piRatioValues = [ seed ]
      , piRatioHistogram = addToHistogram initialHistogram seed
      , whiteNoiseValues = [ seed ]
      , whiteNoiseHistogram = addToHistogram initialHistogram seed
      , blueNoiseValues = [ seed ]
      , blueNoiseHistogram = addToHistogram initialHistogram seed
      }
    , Cmd.none
    )


seed : Float
seed =
    0.01


goldenRatio : Float
goldenRatio =
    (1 + sqrt 5) / 2


silverRatio : Float
silverRatio =
    1 + sqrt 2


type alias Datum =
    { start : Float
    , end : Float
    , y : Float
    }


initialHistogram : List Datum
initialHistogram =
    [ Datum 0.0 0.1 0.0
    , Datum 0.1 0.2 0.0
    , Datum 0.2 0.3 0.0
    , Datum 0.3 0.4 0.0
    , Datum 0.4 0.5 0.0
    , Datum 0.5 0.6 0.0
    , Datum 0.6 0.7 0.0
    , Datum 0.7 0.8 0.0
    , Datum 0.8 0.9 0.0
    , Datum 0.9 1.0 0.0
    ]



-- UPDATE


type Msg
    = IncrementColors
    | NewWhiteNoise Float
    | NewBlueNoise (List Float)
    | DecrementColors
    | ResetColors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementColors ->
            let
                nextGoldenRatioValue =
                    case List.head model.goldenRatioValues of
                        Just value ->
                            calculateNext value goldenRatio

                        Nothing ->
                            model.seed

                nextSilverRatioValue =
                    case List.head model.silverRatioValues of
                        Just value ->
                            calculateNext value silverRatio

                        Nothing ->
                            model.seed

                nextERatioValue =
                    case List.head model.eRatioValues of
                        Just value ->
                            calculateNext value e

                        Nothing ->
                            model.seed

                nextPiRatioValue =
                    case List.head model.piRatioValues of
                        Just value ->
                            calculateNext value pi

                        Nothing ->
                            model.seed
            in
            ( { model | goldenRatioValues = [ nextGoldenRatioValue ] ++ model.goldenRatioValues, goldenRatioHistogram = addToHistogram model.goldenRatioHistogram nextGoldenRatioValue, eRatioValues = [ nextERatioValue ] ++ model.eRatioValues, silverRatioValues = [ nextSilverRatioValue ] ++ model.silverRatioValues, silverRatioHistogram = addToHistogram model.silverRatioHistogram nextSilverRatioValue, eRatioHistogram = addToHistogram model.eRatioHistogram nextERatioValue, piRatioValues = [ nextPiRatioValue ] ++ model.piRatioValues, piRatioHistogram = addToHistogram model.piRatioHistogram nextPiRatioValue }, Random.generate NewWhiteNoise (Random.float 0 1) )

        NewWhiteNoise nextWhiteNoiseValue ->
            ({model | whiteNoiseValues = [ nextWhiteNoiseValue ] ++ model.whiteNoiseValues, whiteNoiseHistogram = addToHistogram model.whiteNoiseHistogram nextWhiteNoiseValue }, Random.generate NewBlueNoise (blueCandidates ((List.length model.blueNoiseValues) + 1 )))

        NewBlueNoise blueNoiseCandidates ->
            let 
                nextBlueNoiseValue = getFarthest model.blueNoiseValues blueNoiseCandidates
            in
            ({model | blueNoiseValues = [ nextBlueNoiseValue ] ++ model.blueNoiseValues, blueNoiseHistogram = addToHistogram model.blueNoiseHistogram nextBlueNoiseValue }, Cmd.none)

        DecrementColors ->
            ( { model
                | goldenRatioValues =
                    case List.tail model.goldenRatioValues of
                        Just tailValues ->
                            tailValues

                        Nothing ->
                            []
                , goldenRatioHistogram =
                    case List.head model.goldenRatioValues of
                        Just headValue ->
                            removeFromHistogram model.goldenRatioHistogram headValue

                        Nothing ->
                            model.goldenRatioHistogram
                , eRatioValues =
                    case List.tail model.eRatioValues of
                        Just tailValues ->
                            tailValues

                        Nothing ->
                            []
                , eRatioHistogram =
                    case List.head model.eRatioValues of
                        Just headValue ->
                            removeFromHistogram model.eRatioHistogram headValue

                        Nothing ->
                            model.eRatioHistogram
                , silverRatioValues =
                    case List.tail model.silverRatioValues of
                        Just tailValues ->
                            tailValues

                        Nothing ->
                            []
                , silverRatioHistogram =
                    case List.head model.silverRatioValues of
                        Just headValue ->
                            removeFromHistogram model.silverRatioHistogram headValue

                        Nothing ->
                            model.silverRatioHistogram
                , piRatioValues =
                    case List.tail model.piRatioValues of
                        Just tailValues ->
                            tailValues

                        Nothing ->
                            []
                , piRatioHistogram =
                    case List.head model.piRatioValues of
                        Just headValue ->
                            removeFromHistogram model.piRatioHistogram headValue

                        Nothing ->
                            model.piRatioHistogram
                , whiteNoiseValues =
                    case List.tail model.whiteNoiseValues of
                        Just tailValues ->
                            tailValues

                        Nothing ->
                            []
                , whiteNoiseHistogram =
                    case List.head model.whiteNoiseValues of
                        Just headValue ->
                            removeFromHistogram model.whiteNoiseHistogram headValue

                        Nothing ->
                            model.whiteNoiseHistogram
                , blueNoiseValues =
                    case List.tail model.blueNoiseValues of
                        Just tailValues ->
                            tailValues

                        Nothing ->
                            []
                , blueNoiseHistogram =
                    case List.head model.blueNoiseValues of
                        Just headValue ->
                            removeFromHistogram model.blueNoiseHistogram headValue

                        Nothing ->
                            model.blueNoiseHistogram
              }
            , Cmd.none
            )

        ResetColors ->
            ( { model
                | goldenRatioValues = [ seed ]
                , goldenRatioHistogram = addToHistogram initialHistogram seed
                , silverRatioValues = [ seed ]
                , silverRatioHistogram = addToHistogram initialHistogram seed
                , eRatioValues = [ seed ]
                , eRatioHistogram = addToHistogram initialHistogram seed
                , piRatioValues = [ seed ]
                , piRatioHistogram = addToHistogram initialHistogram seed
                , whiteNoiseValues = [ seed ]
                , whiteNoiseHistogram = addToHistogram initialHistogram seed
                , blueNoiseValues = [ seed ]
                , blueNoiseHistogram = addToHistogram initialHistogram seed
              }
            , Cmd.none
            )


calculateNext : Float -> Float -> Float
calculateNext prev ratio =
    let
        sum =
            prev + ratio
    in
    sum - (floor sum |> toFloat)


addToHistogram : List Datum -> Float -> List Datum
addToHistogram data value =
    List.map2 addToBucket data (List.repeat (List.length data) value)


addToBucket : Datum -> Float -> Datum
addToBucket datum value =
    case ( value > datum.start, value < datum.end ) of
        ( True, True ) ->
            { datum | y = datum.y + 1 }

        ( _, _ ) ->
            datum


blueCandidates : Int -> Random.Generator (List Float)
blueCandidates n = 
    Random.list (n + 1) (Random.float 0 1)

getFarthest : List Float -> List Float -> Float
getFarthest values candidates =
    Tuple.second <| List.foldl maxuple (0.0, 0.0) <| List.map2 getClosest (List.repeat (List.length candidates) values) candidates



maxuple : (Float, Float) -> (Float, Float) -> (Float, Float)
maxuple currMax tupy =
    if Tuple.first tupy > Tuple.first currMax then tupy else currMax


getClosest : List Float -> Float -> ( Float, Float )
getClosest values candidate = 
    case (List.minimum ( List.map2 absoluteDifference values (List.repeat (List.length values) candidate ))) of
        Just closest -> ( closest, candidate )
        Nothing -> ( seed, seed )

absoluteDifference : Float -> Float -> Float
absoluteDifference a b =
    abs (a - b)

removeFromHistogram : List Datum -> Float -> List Datum
removeFromHistogram data value =
    List.map2 removeFromBucket data (List.repeat (List.length data) value)


removeFromBucket : Datum -> Float -> Datum
removeFromBucket datum value =
    case ( value > datum.start, value < datum.end ) of
        ( True, True ) ->
            { datum | y = datum.y - 1 }

        ( _, _ ) ->
            datum



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "wide", class "tall", class "col", style "justify-content" "start", style "gap" "3em", style "color" white ]
        [ h1 [] [ Html.text "Pseudo-random Distributions Between 0 and 1" ]
        , p [] [ Html.text "Credit to the SEED division at Electronic Arts for ", a [ href "https://www.youtube.com/watch?v=tethAU66xaA" ] [ text "this video" ] ]
        , p [ style "font-family" "monospace", style "color" "#cccccc", style "background-color" "#000000", style "padding" "1em", style "border-radius" "0.5em" ] [ Html.text "next = (previous + ratio) - floor(prev + ratio)" ]
        , div [ class "row" ]
            [ button [ onClick ResetColors ] [ text "Reset" ]
            , button [ onClick DecrementColors ] [ text "Remove Last" ]
            , button [ onClick IncrementColors, class "cta" ] [ text "Add Next" ]
            ]
        , div [ class "row", style "flex-wrap" "wrap" ]
            [ div [ class "col" ]
                [ h3 [] [ Html.text "Golden Ratio" ]
                , span [ style "font-family" "monospace" ] [ Html.text <| String.fromFloat goldenRatio ]
                , colorCircle model.goldenRatioValues
                , histogram model.goldenRatioHistogram
                ]
            , div [ class "col" ]
                [ h3 [] [ Html.text "Silver Ratio" ]
                , span [ style "font-family" "monospace" ] [ Html.text <| String.fromFloat silverRatio ]
                , colorCircle model.silverRatioValues
                , histogram model.silverRatioHistogram
                ]
            , div [ class "col" ]
                [ h3 [] [ Html.text "e" ]
                , span [ style "font-family" "monospace" ] [ Html.text <| String.fromFloat e ]
                , colorCircle model.eRatioValues
                , histogram model.eRatioHistogram
                ]
            , div [ class "col" ]
                [ h3 [] [ Html.text "pi" ]
                , span [ style "font-family" "monospace" ] [ Html.text <| String.fromFloat pi ]
                , colorCircle model.piRatioValues
                , histogram model.piRatioHistogram
                ]
            , div [ class "col" ]
                [ h3 [] [ Html.text "White Noise" ]
                , span [ style "font-family" "monospace" ] [ Html.text "\"true randomness\""]
                , colorCircle model.whiteNoiseValues
                , histogram model.whiteNoiseHistogram
                ]
            , div [ class "col" ]
                [ h3 [] [ Html.text "Blue Noise" ]
                , span [ style "font-family" "monospace" ] [ Html.text "\"smooth randomish\""]
                , colorCircle model.blueNoiseValues
                , histogram model.blueNoiseHistogram
                ]
            ]
        ]


colorCircle : List Float -> Html Msg
colorCircle data =
    svg
        [ width "360"
        , height "360"
        , viewBox "0 0 360 360"
        ]
        [ circle
            [ cx "180"
            , cy "180"
            , r "178"
            , fill "black"
            , stroke white
            , strokeWidth "4"
            ]
            []
        , svg [] <| List.map colorDialIndicator data
        ]


colorDialIndicator : Float -> Svg Msg
colorDialIndicator n =
    line
        [ x1 "180"
        , y1 "180"
        , x2 "180"
        , y2 "14"
        , Svg.Attributes.stroke (hsl (n * 360.0) 0.99 0.5 |> Color.toCssString)
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.transform ("rotate(" ++ (n * 360.0 |> String.fromFloat) ++ " 180 180)")
        ]
        []


histogram : List Datum -> Html Msg
histogram data =
    C.chart
        [ CA.height 300
        , CA.width 300
        ]
        [ C.grid [ CA.color white ]
        , C.xLabels [ CA.withGrid, CA.moveDown 10, CA.color white ]
        , C.yLabels [ CA.withGrid, CA.ints, CA.color white ]
        , C.bars
            [ CA.x1 .start
            , CA.x2 .end
            , CA.spacing 0.1
            , CA.roundTop 0.5
            ]
            [ C.bar .y [] ]
            data
        ]


black : String
black =
    "#011627"


red : String
red =
    "#EF5350"


green : String
green =
    "#22da6e"


yellow : String
yellow =
    "#c5e478"


blue : String
blue =
    "#82AAFF"


magenta : String
magenta =
    "#C792EA"


cyan : String
cyan =
    "#21c7a8"


white : String
white =
    "#d6deeb"
