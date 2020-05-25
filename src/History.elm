module History exposing (..)

import Journey as Journey


type alias JourneyHistory =
    { visitedSteps : List Journey.Step
    , currentStep : Journey.Step
    , remainingSteps : List Journey.Step
    }


jouneryHistory : JourneyHistory
jouneryHistory =
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


updateJourneyHistory : JourneyHistory -> JourneyHistory
updateJourneyHistory history =
    { visitedSteps = history.currentStep :: history.visitedSteps
    , currentStep = getNextStepOrDefault history.currentStep history.remainingSteps
    , remainingSteps = List.drop 1 history.remainingSteps
    }
