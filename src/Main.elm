module Main exposing (..)

import Browser
import History as History
import Html exposing (Html, div, h1, li, text, ul)
import Html.Events exposing (onClick)
import Journey as Journey


type alias Model =
    { journeyHistory : History.JourneyHistory
    }


type Msg
    = NoOp
    | UpdateStartStep Journey.Start
    | UpdateBuildingStep Journey.Building
    | ToggleStepStatus Journey.Step


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


updateHitoryCurrentStep : Model -> Journey.Step -> Model
updateHitoryCurrentStep model currentStep =
    { model
        | journeyHistory =
            History.updateCurrentStep currentStep model.journeyHistory
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( initModel, Cmd.none )

        UpdateStartStep opt ->
            ( Journey.startUpdate opt
                |> updateHitoryCurrentStep model
            , Cmd.none
            )

        UpdateBuildingStep opt ->
            ( Journey.buildingUpdate opt
                |> updateHitoryCurrentStep model
            , Cmd.none
            )

        ToggleStepStatus step ->
            ( Journey.toggleStepStatus step
                |> updateHitoryCurrentStep model
            , Cmd.none
            )


initModel : Model
initModel =
    { journeyHistory = History.journeyHistory
    }



-- VIEW ----


doForOpenStep : Model -> Html Msg
doForOpenStep model =
    let
        step =
            History.getCurrentStep model.journeyHistory
    in
    if Journey.isStart step then
        printStepOpen
            UpdateStartStep
            Journey.startGetOptionLabel
            Journey.startGetOptions

    else
        printStepOpen
            UpdateBuildingStep
            Journey.buildingGetOptionLabel
            Journey.buildingGetOptions


printStepOpen : (option -> Msg) -> (option -> String) -> List option -> Html Msg
printStepOpen msg labels options =
    let
        list =
            \option ->
                li
                    []
                    [ Html.button
                        [ onClick (msg option) ]
                        [ text (labels option) ]
                    ]
    in
    ul []
        (List.map
            list
            options
        )


printStepClose : Journey.Step -> Html Msg
printStepClose journeyStep =
    div [ onClick (ToggleStepStatus journeyStep) ]
        [ text (Journey.labelForSelectedOption journeyStep)
        ]


view : Model -> Html Msg
view model =
    let
        step =
            History.getCurrentStep model.journeyHistory

        visitedSteps =
            History.getVisitedStep
                model.journeyHistory

        _ =
            Debug.log "vs " visitedSteps

        foo =
            List.map
                (\s ->
                    if Journey.isOpenStep s then
                        doForOpenStep model

                    else
                        printStepClose s
                )

        --visitedSteps
    in
    div []
        [ h1 []
            [ text "Your Journey" ]
        , div []
            (foo
                visitedSteps
            )

        --, List.map
        --(\s ->
        --if Journey.isOpenStep s then
        --doForOpenStep model
        --else
        --printStepClose s
        --)
        --visitedSteps
        , if Journey.isOpenStep step then
            doForOpenStep model

          else
            printStepClose step
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
