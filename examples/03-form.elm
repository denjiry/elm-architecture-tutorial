import Browser
import Html exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Char exposing (isDigit, isUpper, isLower)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , result : String
  , error : String
  }


init : Model
init =
  Model "" "" "" "" ""



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Submit ->
      viewValidation model



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , button [onClick Submit ] [text "submit"]
    , div [ style "color" model.result ] [ text model.error ]
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Model
viewValidation model =
  if model.password == model.passwordAgain then
    if String.length(model.password) >= 8 then
      if String.any isDigit model.password && String.any isUpper model.password && String.any isLower model.password then
        {model | result = "green", error = "OK"}
      else
        {model | result = "red", error = "must contain upper lower digit"}
    else
      {model | result = "red", error = "too short"}
  else
    {model | result = "red", error = "Passwords do not match!"}
