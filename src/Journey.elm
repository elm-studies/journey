module Journey exposing
    ( Building
    , Start
    , Step
    , building
    , buildingGetOptionLabel
    , buildingGetOptions
    , buildingUpdate
    , isBuilding
    , isOpenStep
    , isStart
    , labelForSelectedOption
    , start
    , startGetOptionLabel
    , startGetOptions
    , startUpdate
    , stepId
    , toggleStepStatus
    )

--import Html exposing (Html)


type alias StepLabel =
    String


type alias StepId =
    String


type Payload value
    = NoVal
    | Payload value


type Selected id
    = NoSelected
    | Selected id


type Status
    = Close
    | Open


type Step
    = StartStep StepId (Selected Start) (Payload String) Status
    | BuildingStep StepId (Selected Building) (Payload String) Status


isStart : Step -> Bool
isStart step =
    case step of
        StartStep _ _ _ _ ->
            True

        _ ->
            False


isBuilding : Step -> Bool
isBuilding step =
    case step of
        BuildingStep _ _ _ _ ->
            True

        _ ->
            False


isOpenStep : Step -> Bool
isOpenStep step =
    case step of
        StartStep _ _ _ status ->
            status == Open

        BuildingStep _ _ _ status ->
            status == Open


labelForSelectedOption : Step -> StepLabel
labelForSelectedOption step =
    case step of
        StartStep _ selected _ _ ->
            case selected of
                Selected option ->
                    startGetOptionLabel option

                NoSelected ->
                    ""

        BuildingStep _ selected _ _ ->
            case selected of
                Selected option ->
                    buildingGetOptionLabel option

                NoSelected ->
                    ""


stepId : Step -> StepId
stepId step =
    case step of
        StartStep id _ _ _ ->
            id

        BuildingStep id _ _ _ ->
            id


toggleStatus : Status -> Status
toggleStatus status =
    case status of
        Open ->
            Close

        Close ->
            Open


toggleStepStatus : Step -> Step
toggleStepStatus step =
    case step of
        StartStep id selected payload status ->
            StartStep id
                selected
                payload
                (toggleStatus
                    status
                )

        BuildingStep id selected payload status ->
            BuildingStep id
                selected
                payload
                (toggleStatus
                    status
                )



---- START Step STEP ----


startStepLabels :
    { newConnection : StepLabel
    , temporaryConnection : StepLabel
    , feedInEnergy : StepLabel
    , existingConnection : StepLabel
    }
startStepLabels =
    { newConnection = "New Connection"
    , temporaryConnection = "Temporary Connection"
    , feedInEnergy = "Feedin Energy"
    , existingConnection = "Existing Connection"
    }


type Start
    = NewConnection
    | TemporaryConnection
    | FeedInEnergy
    | ExistingConnection


start : Step
start =
    StartStep "start" NoSelected NoVal Open


startUpdate : Start -> Step
startUpdate opt =
    let
        step =
            \payload -> StartStep "start" (Selected opt) (Payload payload) Close
    in
    case opt of
        NewConnection ->
            step "E2E"

        TemporaryConnection ->
            step "E2E"

        FeedInEnergy ->
            step "Feedin"

        ExistingConnection ->
            step "E2E"


startGetOptionLabel : Start -> StepLabel
startGetOptionLabel step =
    case step of
        NewConnection ->
            startStepLabels.newConnection

        TemporaryConnection ->
            startStepLabels.temporaryConnection

        FeedInEnergy ->
            startStepLabels.feedInEnergy

        ExistingConnection ->
            startStepLabels.existingConnection


startGetOptions : List Start
startGetOptions =
    [ NewConnection
    , TemporaryConnection
    , FeedInEnergy
    , ExistingConnection
    ]



---- Building Step STEP ----


buildingStepLabels :
    { detachedHouse : StepLabel
    , semiDetachedHouse : StepLabel
    }
buildingStepLabels =
    { detachedHouse = "Detached House"
    , semiDetachedHouse = "Semi Detached House"
    }


type Building
    = DetachedHouse
    | SemiDetachedHouse


building : Step
building =
    BuildingStep "building" NoSelected NoVal Open


buildingUpdate : Building -> Step
buildingUpdate opt =
    let
        step =
            \payload -> BuildingStep "building" (Selected opt) (Payload payload) Close
    in
    case opt of
        DetachedHouse ->
            step "detachedHouse"

        SemiDetachedHouse ->
            step "semiDetachedHouse"


buildingGetOptionLabel : Building -> StepLabel
buildingGetOptionLabel step =
    case step of
        DetachedHouse ->
            buildingStepLabels.detachedHouse

        SemiDetachedHouse ->
            buildingStepLabels.semiDetachedHouse


buildingGetOptions : List Building
buildingGetOptions =
    [ DetachedHouse
    , SemiDetachedHouse
    ]
