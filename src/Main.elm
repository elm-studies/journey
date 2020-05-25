module Main exposing (..)

import Browser
import History as History
import Html exposing (Html, div, h1, li, text, ul)
import Html.Events exposing (onClick)
import Journey as Journey


type alias Model =
    { journeyStep : Journey.Step
    , jouneryHistory : History.JourneyHistory
    }


type Msg
    = NoOp
    | UpdateStartStep Journey.Start
    | UpdateBuildingStep Journey.Building
    | ToggleStepStatus Journey.Step


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( initModel, Cmd.none )

        UpdateStartStep opt ->
            ( { model
                | journeyStep = Journey.startUpdate opt
              }
            , Cmd.none
            )

        UpdateBuildingStep opt ->
            ( { model
                | journeyStep = Journey.buildingUpdate opt
              }
            , Cmd.none
            )

        ToggleStepStatus step ->
            ( { model
                | journeyStep = Journey.toggleStepStatus step
              }
            , Cmd.none
            )


initModel : Model
initModel =
    { journeyStep = Journey.building
    , jouneryHistory = History.jouneryHistory
    }



-- VIEW ----


doForOpenStep : Model -> Html Msg
doForOpenStep model =
    let
        step =
            model.journeyStep
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
    div []
        [ h1 [] [ text "Your Journey" ]
        , if Journey.isOpenStep model.journeyStep then
            doForOpenStep model

          else
            printStepClose model.journeyStep
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
