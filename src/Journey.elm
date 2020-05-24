module Journey exposing
    ( Building
    , Start
    , Step
    , building
    , buildingGetOptionLabel
    , buildingGetOptions
    , buildingUpdate
    , isBuilding
    , isOptionSelected
    , isStart
    , labelForSelectedOption
    , start
    , startGetOptionLabel
    , startGetOptions
    , startUpdate
    )

--import Html exposing (Html)


type alias StepLabel =
    String


type Payload value
    = NoVal
    | Payload value


type Selected id
    = NoSelected
    | Selected id


type Step
    = StartStep (Selected Start) (Payload String)
    | BuildingStep (Selected Building) (Payload String)


isStart : Step -> Bool
isStart step =
    case step of
        StartStep _ _ ->
            True

        _ ->
            False


isBuilding : Step -> Bool
isBuilding step =
    case step of
        StartStep _ _ ->
            True

        _ ->
            False


isOptionSelected : Step -> Bool
isOptionSelected step =
    case step of
        StartStep selected _ ->
            selected /= NoSelected

        BuildingStep selected _ ->
            selected /= NoSelected


labelForSelectedOption : Step -> StepLabel
labelForSelectedOption step =
    case step of
        StartStep selected _ ->
            case selected of
                Selected option ->
                    startGetOptionLabel option

                NoSelected ->
                    ""

        BuildingStep selected _ ->
            case selected of
                Selected option ->
                    buildingGetOptionLabel option

                NoSelected ->
                    ""



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
    StartStep NoSelected NoVal


startUpdate : Start -> Step
startUpdate opt =
    let
        step =
            \payload -> StartStep (Selected opt) (Payload payload)
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
    BuildingStep NoSelected NoVal


buildingUpdate : Building -> Step
buildingUpdate opt =
    let
        step =
            \payload -> BuildingStep (Selected opt) (Payload payload)
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
