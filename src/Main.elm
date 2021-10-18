module Main exposing (main)

import Browser
import Html exposing(..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Task
import Process
import Array

type alias Model =
  { userName : Maybe String
  , userNameInput : Maybe String
  , roomName : Maybe String
  , roomNameInput : Maybe String
  , loggedIn : Bool
  , userSelectedTokenName: Maybe String
  , houseSelectedTokenName: Maybe String
  , animate: Bool
  , randomTokenCount: Int
  , matchFinished: Bool
  , matchResult: Maybe String
  , score: Int
  }

type Msg
    = ChangeUserName String
    | SaveUserName
    | ChangeRoomName String
    | SaveRoomName
    | CreateRoom
    | NewRandomNumber Int
    | GetRandomTokenNumber
    | SetUserTokenName String
    | GetMatchResult()
    | PlayAgain

init : () -> ( Model, Cmd Msg )
init _ =
    ( { userName = Nothing
      , userNameInput = Nothing
      , roomName = Nothing
      , roomNameInput = Nothing
      , loggedIn = False
      , userSelectedTokenName = Nothing
      , animate = False
      , houseSelectedTokenName = Nothing
      , randomTokenCount = 0
      , matchFinished = False
      , matchResult = Nothing
      , score = 0
      }
    , Cmd.none
    )

delay : Float -> msg -> Cmd msg
delay time msg =
  Process.sleep time
  |> Task.andThen (always <| Task.succeed msg)
  |> Task.perform identity

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChangeUserName name ->
            ({ model | userNameInput = Just name }, Cmd.none)
        SaveUserName ->
            ({ model | userName = model.userNameInput }, Cmd.none)
        ChangeRoomName name ->
            ({ model | roomNameInput = Just name }, Cmd.none)
        SaveRoomName ->
            ({ model | roomName = model.roomNameInput }, Cmd.none)
        CreateRoom ->
            ({ model | loggedIn = True }, Cmd.none)
        SetUserTokenName tokenName ->
            ({ model | userSelectedTokenName = Just(tokenName), animate = True },  Random.generate NewRandomNumber (Random.int 1 3))
        GetRandomTokenNumber ->
            ({ model | randomTokenCount = (model.randomTokenCount + 1) }, Random.generate NewRandomNumber (Random.int 1 3))
        NewRandomNumber number ->
          let
            cmd = if model.randomTokenCount > 10 then (Task.perform GetMatchResult (Task.succeed ())) else ( delay (300) <| GetRandomTokenNumber )
            animate = if model.randomTokenCount > 10 then False else True
          in
          ({ model | houseSelectedTokenName = getTokenFromNumber number, animate = animate}, cmd )
        GetMatchResult _ ->
          let 
            matchResult = getMatchResult model.userSelectedTokenName model.houseSelectedTokenName
            score = case matchResult of 
              Just result ->
                if result == "YOU WIN" then model.score + 1 else if result == "YOU LOSE" then model.score - 1 else model.score
              Nothing ->
                model.score
          in
          ({ model | matchFinished = True , matchResult = matchResult, score = score}, Cmd.none )
        PlayAgain ->
          ({ model | userSelectedTokenName = Nothing, houseSelectedTokenName = Nothing, randomTokenCount = 0, matchFinished = False, matchResult = Nothing }, Cmd.none)

getMatchResult: Maybe String -> Maybe String -> Maybe String
getMatchResult userSelectedTokenName houseSelectedTokenName =
  Maybe.map2  calculateMatchResult userSelectedTokenName houseSelectedTokenName

calculateMatchResult:  String ->  String -> String
calculateMatchResult userToken houseToken =
  if userToken == houseToken then
    "DRAW"
  else
    if (userToken == "paper" && houseToken == "rock")
      || (userToken == "rock" && houseToken == "scissors")
      || (userToken == "scissors" && houseToken == "paper") then
      "YOU WIN"
    else 
      "YOU LOSE"

getTokenFromNumber: Int -> Maybe String
getTokenFromNumber num =
  case num of
    1 -> 
      Just("paper")
    2 -> 
      Just("rock")
    3 -> 
      Just("scissors")
    _ ->
      Nothing

view : Model -> Html Msg
view model =
  if model.loggedIn then
      div [ class "absolute bottom-0 flex flex-col items-center left-0 right-0 top-0 p-6"]
      [
        showHeader model
        ,
        case model.userSelectedTokenName of
          Nothing ->
            div [ class "flex flex-col justify-center items-center w-full h-full"]
            [
              div[ class "user-token-list gap-x-9 gap-y-2 grid grid-cols-2 grid-rows-2 w-full"]
              [
                showToken model "rock" False
              ,
                showToken model "paper" False
              ,
                showToken model "scissors" False
              ]
            ]
          Just userSelectedTokenName ->
            div [ class "flex flex-col justify-center items-center w-full h-full"]
            [
              div[ class "user-token-list grid grid-cols-2 gap-x-20" ]
              [
                showToken model userSelectedTokenName False
              ,
                showToken model (Maybe.withDefault "rock" model.houseSelectedTokenName) True
                ,
                p [ class "token-label" ] 
                [ text "YOU PICKED"]
                ,
                p [ class "token-label" ] 
                [ text "HOUSE PICKED"]
              ]
              ,
                showResults model
            ]
      ]
  else
      div [ class "absolute bottom-0 flex flex-col items-center justify-center left-0 right-0 top-0 p-6"]
    [
      h1 [ class "text-white" ] 
      [ 
        case model.userName of
          Just userName ->
            text (String.append "Welcome " userName)
          Nothing ->
            text "Welcome Guest" 
      ]
    ,  
      div  [ class "grid grid-cols-2 w-fit gap-x-7 gap-y-2.5 mt-2" ]
      [
        input [ class "lobby-options-input", id "name", type_ "text", placeholder "Type your username", onInput ChangeUserName ] []
        , button [ class "self-end w-full", onClick SaveUserName ] [ text "Change name" ]
        , button [ class "justify-self-center col-span-2", onClick CreateRoom ] [ text "Start Game" ]
      ]
    ]

showHeader: Model -> Html Msg
showHeader model =
  div [ class "header mb-2"]
  [
    div [ class "header-container"]
    [
      div [ class "header-title"]
      [
        h1 []
        [ text "ROCK PAPER SCISSORS" ]
      ]
      ,
      div [ class "header-score-container"]
      [
        span []
        [ text "SCORE" ]
        ,
        p [ class "score" ]
        [ text (String.fromInt model.score) ]
      ]
    ]
  ]

showToken: Model -> String -> Bool -> Html Msg
showToken model item isHouseToken =
  let
    event = if isHouseToken then [] else
      case model.userSelectedTokenName of
        Just _ -> []
        Nothing -> [ onClick (SetUserTokenName item)]
  in
  div (List.append [ class (String.append "token " item) ] event)
  [
    div [ class ( String.append "token-circle" (if model.animate && isHouseToken then " animate" else ""))]
    [
      div [ class "token-inner-circle"]
      [
        img [src (String.append (String.append "assets/images/icon-" item) ".svg"), class "token-icon" ] []
      ]
    ]
  ]

showResults: Model -> Html Msg
showResults model =
  if model.matchFinished then 
    case model.matchResult of
      Just matchResult ->
        div [ class "result-container"]
        [
          div [ class "result" ]
          [
            span []
            [ text matchResult]
          ]
          ,
          button [ class "play-again", onClick PlayAgain]
          [
            span []
            [ text "PLAY AGAIN"]
          ]
        ]
      Nothing ->
        text ""
  else text ""

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }