import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (type_, placeholder, value, hidden)
import List.Extra
import Random.List
import Select

main = Html.beginnerProgram { model = init, view = view, update = update }

-- MODELS
type alias Model =
  { players: List Player
  , currentName: String
  , currentRole: Role
  , phase: Phase
  , story: List String
  , selectedPlayer: Maybe Player
  , killed: Maybe Player
  }
type alias Player =
  { name: String
  , role: Role
  }

type Role
  = Villager
  | Werewolf
  | Doctor
  | Seer

type Phase
  = Initializing
  | Dusk
  | WerewolvesWakeUp
  | WerewolvesSleep
  | DoctorWakesUp
  | DoctorSleeps
  | SeerWakesUp
  | SeerSleeps
  | Dawn
  | BurnedAtTheStake

init : (Model, Cmd Message)
init =
  ( Model
    []
    ""
    Villager
    Initializing
    []
    Nothing
    Nothing
  , Cmd.none
  )

-- UPDATE
type Message
  = NameChanged String
  | RoleChanged Role
  | CreatePlayer
  | RemovePlayer Player
  | StartPhase Phase
  | SelectPlayer (Maybe Player)


update : Message -> (Model, Cmd Message) -> (Model, Cmd Message)
update message (model, command) =
  case message of
    NameChanged name ->
      ( { model | currentName = name }
      , Cmd.none
      )
    RoleChanged role ->
      ( { model | currentRole = role }
      , Cmd.none
      )
    CreatePlayer ->
      if not (model.phase == Initializing) then
        (model, Cmd.none)
      else
        ( { model
            | players = (Player model.currentName model.currentRole) :: model.players
            , currentName = ""
            , currentRole = Villager
          }
        , Cmd.none
        )
    RemovePlayer player ->
      if not (model.phase == Initializing) then
        (model, Cmd.none)
      else
        ( { model | players = List.Extra.remove player model.players }
        , Cmd.none
        )
    SelectPlayer player ->
      ( { model | selectedPlayer = player }
      , Cmd.none
      )
    StartPhase phase ->
      let
        newModel = case phase of
          Initializing -> model
          Dusk ->
            addToStory "The night has come so everyone goes to sleep" model
          WerewolvesWakeUp ->
            addToStory "And the werewolves sneak into the village to seek out their pray" model
          WerewolvesSleep ->
            addToStory
              "The werewolves picked a unlucky soul so they sneak back out of the village"
              { model
                | killed = model.selectedPlayer
                , selectedPlayer = Nothing
              }
          DoctorWakesUp -> addToStory "Then the Doctor awakens to seek out someone to heal" model
          DoctorSleeps ->
            addToStory
              "The lucky soul has been chosen and the Doctor goes back to sleep"
              { model
                | killed = if model.killed == model.selectedPlayer then Nothing else model.killed
                , selectedPlayer = Nothing
              }
          SeerWakesUp ->
            addToStory "The seer awakens to see the true nature of a single person amongst us" model
          SeerSleeps ->
            addToStory "The truth has been revealed and the seer goes back to sleep" model
          Dawn ->
            case model.killed of
              Nothing ->
                addToStory "Dawn has come and the village wakes up peacecully... without a bloody murder" model
              Just player ->
                addToStory
                  ("Dawn has come and the village finds " ++ player.name ++ " murdered in the dark night. Pitchforks and fire! Who will they burn at the stake?")
                  { model
                    | players = (List.Extra.remove player model.players)
                    , selectedPlayer = Nothing
                    , killed = Nothing
                  }
          BurnedAtTheStake ->
            case model.selectedPlayer of
              Nothing -> model
              Just player ->
                addToStory
                  ("The villagers in their wisdomd decided to burn " ++ player.name ++ " at the stake! Their true identity is revealed, a " ++ toString player.role)
                  { model
                    | players = (List.Extra.remove player model.players)
                    , selectedPlayer = Nothing
                  }

      in
        ( { newModel | phase = phase }
        , Cmd.none
        )


addToStory: String -> Model -> Model
addToStory line model =
  { model | story = line :: model.story }


-- VIEW
view : (Model, Cmd Message) -> Html Message
view (model, command)  =
  let
    initializing = not (model.phase == Initializing)
  in
    div
      []
      [ renderPhase model.phase model
      , renderPlayers initializing model.players
      , renderStory model.story
      ]

renderPlayers:  Bool -> List Player -> Html Message
renderPlayers gameStarted players =
  ul [] (List.map (renderPlayer gameStarted) players)

renderPlayer: Bool -> Player -> Html Message
renderPlayer gameStarted player =
  li []
    [ text player.name
    , a [ onClick (RemovePlayer player), hidden gameStarted ] [ text " [remove]" ]
    ]

renderStory: List String -> Html Message
renderStory story =
  ul [] (List.map (\line -> li [] [ text line ]) story)

renderPhase: Phase -> Model -> Html Message
renderPhase phase model =
  case phase of
    Initializing ->
      renderInitialization model.currentName
    Dusk ->
      renderClickThroughPhase "Zzzzzzz" WerewolvesWakeUp
    WerewolvesWakeUp ->
      renderPickPhase (List.filter (\p -> not (p.role == Werewolf)) model.players) model.selectedPlayer WerewolvesSleep
    WerewolvesSleep ->
      renderClickThroughPhase "Zzzzzzz" DoctorWakesUp
    DoctorWakesUp ->
      renderPickPhase model.players model.selectedPlayer DoctorSleeps
    DoctorSleeps ->
      renderClickThroughPhase "Zzzzzzz" SeerWakesUp
    SeerWakesUp ->
      renderClickThroughPhase "Zzzzzzz" SeerSleeps
    SeerSleeps ->
      renderClickThroughPhase "Zzzzzzz" Dawn
    Dawn ->
      renderPickPhase model.players model.selectedPlayer BurnedAtTheStake
    BurnedAtTheStake ->
      renderClickThroughPhase "Back into the night" Dusk


renderInitialization: String -> Html Message
renderInitialization currentName =
  let
    config =
      { items = [Villager, Werewolf, Doctor, Seer]
      , getId = (\r -> toString r)
      , getLabel = (\r -> toString r)
      , onSelect = (\r -> RoleChanged r)
      , selected = Villager
      }
  in
    div
      []
      [ input [ type_ "text", onInput NameChanged, value currentName] []
      , Select.selectList config
      , button [ onClick CreatePlayer ] [ text "Add" ]
      , button [ onClick (StartPhase Dusk) ] [ text "Into the night!" ]
      ]

renderClickThroughPhase: String -> Phase -> Html Message
renderClickThroughPhase buttonText phase =
  div
    []
    [ button [ onClick (StartPhase phase) ] [ text buttonText ] ]


renderPickPhase: List Player -> Maybe Player  -> Phase -> Html Message
renderPickPhase players selectedPlayer nextPhase =
  let
    playerToString =
      (\mp -> case mp of
        Nothing -> "-"
        Just p -> p.name
      )
    config =
      { items = Nothing :: (List.map (\p -> Just p) players)
      , getId = playerToString
      , getLabel = playerToString
      , onSelect = (\mp -> SelectPlayer mp)
      , selected = Nothing
      }
    isBlocked = case selectedPlayer of
      Nothing -> True
      _ -> False
  in
   div
    []
    [ Select.selectList config
    , button [ onClick (StartPhase nextPhase), hidden isBlocked ] [ text "pick" ]
    ]
