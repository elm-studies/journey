module History exposing
    ( JourneyHistory
    , getCurrentStep
    , journeyHistory
    , updateCurrentStep
    )

import Journey as Journey


type JourneyHistory
    = JourneyHistory
        { visitedSteps : List Journey.Step
        , currentStep : Journey.Step
        , remainingSteps : List Journey.Step
        }


journeyHistory : JourneyHistory
journeyHistory =
    JourneyHistory
        { visitedSteps = []
        , currentStep = Journey.start
        , remainingSteps = [ Journey.building ]
        }


getNextStepOrDefault : Journey.Step -> List Journey.Step -> Journey.Step
getNextStepOrDefault defaultSep listOfSteps =
    case listOfSteps of
        [] ->
            defaultSep

        a :: _ ->
            a


updateVisitedSteps : Journey.Step -> List Journey.Step -> List Journey.Step
updateVisitedSteps step visitedSteps =
    let
        _ =
            Debug.log "is member" step
    in
    if List.member step visitedSteps then
        visitedSteps

    else
        step :: visitedSteps


updateJourneyHistory : JourneyHistory -> JourneyHistory
updateJourneyHistory history =
    case history of
        JourneyHistory record ->
            JourneyHistory
                { visitedSteps = updateVisitedSteps record.currentStep record.visitedSteps
                , currentStep = getNextStepOrDefault record.currentStep record.remainingSteps
                , remainingSteps = List.drop 1 record.remainingSteps
                }


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
