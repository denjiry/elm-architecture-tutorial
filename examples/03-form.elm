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
  , submit : Bool
  }


init : Model
init =
  Model "" "" "" False



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
      { model | name = name, submit = False }

    Password password ->
      { model | password = password, submit = False }

    PasswordAgain password ->
      { model | passwordAgain = password, submit = False }

    Submit ->
      { model | submit = True}



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , button [onClick Submit ] [text "submit"]
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if model.submit then
    if model.password == model.passwordAgain then
      if String.length(model.password) >= 8 then
        if String.any isDigit model.password && String.any isUpper model.password && String.any isLower model.password then
          div [] [ text "" ]
        else
          div [ style "color" "red" ] [ text "must contain upper lower digit" ]
      else
        div [ style "color" "red" ] [ text "too short" ]
    else
      div [ style "color" "red" ] [ text "Passwords do not match!" ]
  else
    div [] [ text ""]
