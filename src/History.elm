module History exposing
    ( JourneyHistory
    , getCurrentStep
    , getVisitedStep
    , journeyHistory
    , updateCurrentStep
    )

import Cons as Cons
import Dict exposing (Dict)
import Journey as Journey


type JourneyHistory
    = JourneyHistory
        { visitedSteps : List Journey.Step
        , currentStep : Journey.Step
        , remainingSteps : List Journey.Step
        }


type alias StepId =
    String


regularFlow : List StepId
regularFlow =
    [ "start", "building" ]


flowHead : List StepId -> StepId
flowHead list =
    case list of
        [] ->
            ""

        a :: _ ->
            a


flowTail : List StepId -> List StepId
flowTail list =
    Cons.tail
        (Cons.cons
            ""
            list
        )


journeySteps : Dict StepId Journey.Step
journeySteps =
    Dict.fromList
        [ ( "start", Journey.start )
        , ( "building", Journey.building )
        ]


dictstep : StepId -> Journey.Step
dictstep stepId =
    Dict.get stepId journeySteps
        |> Maybe.withDefault Journey.noneStep


remainingStepsFromList : List StepId -> List Journey.Step
remainingStepsFromList list =
    flowTail list
        |> List.map
            dictstep


journeyHistory : JourneyHistory
journeyHistory =
    JourneyHistory
        { visitedSteps = []
        , currentStep = regularFlow |> flowHead |> dictstep
        , remainingSteps = remainingStepsFromList regularFlow
        }



-- =================================== --


getNextStepOrDefault : Journey.Step -> List Journey.Step -> Journey.Step
getNextStepOrDefault defaultSep listOfSteps =
    case listOfSteps of
        [] ->
            defaultSep

        a :: _ ->
            a


getListOfIds : List Journey.Step -> List String
getListOfIds stepsList =
    List.map Journey.stepId stepsList


updateVisitedSteps : Journey.Step -> List Journey.Step -> List Journey.Step
updateVisitedSteps step visitedSteps =
    let
        isMember =
            getListOfIds
                visitedSteps
                |> List.member
                    (Journey.stepId
                        step
                    )
    in
    if isMember then
        let
            visitedStepsWithoutStep =
                List.filter
                    (\s -> Journey.stepId s /= Journey.stepId step)
                    visitedSteps
        in
        step :: visitedStepsWithoutStep

    else
        step :: visitedSteps


updateJourneyHistory : JourneyHistory -> JourneyHistory
updateJourneyHistory history =
    case history of
        JourneyHistory record ->
            JourneyHistory
                { visitedSteps =
                    updateVisitedSteps
                        record.currentStep
                        record.visitedSteps
                , currentStep =
                    getNextStepOrDefault
                        record.currentStep
                        record.remainingSteps
                , remainingSteps = List.drop 1 record.remainingSteps
                }



--- exposed functions


updateCurrentStep : Journey.Step -> JourneyHistory -> JourneyHistory
updateCurrentStep step history =
    case history of
        JourneyHistory record ->
            JourneyHistory
                { record
                    | currentStep = step
                }
                |> updateJourneyHistory


getCurrentStep : JourneyHistory -> Journey.Step
getCurrentStep (JourneyHistory record) =
    record.currentStep


getVisitedStep : JourneyHistory -> List Journey.Step
getVisitedStep (JourneyHistory record) =
    record.visitedSteps
