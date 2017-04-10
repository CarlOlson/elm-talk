module Todo exposing (..)

import Html exposing (Html, div, input, text)
import Html.Events as Events exposing (onInput, onClick)
import Html.Attributes exposing (autofocus, id, type_)
import Json.Decode as Json
import Dom
import Task

main = Html.program
       { init = initModel ! []
       , update = update
       , view = view
       , subscriptions = \x -> Sub.none
       }

-- Model

type alias Todo =
    { completed : Bool
    , message : String
    , id : Int
    }

type alias Model =
    { todos : List Todo
    , input : String
    }

initModel : Model
initModel = Model [] ""

-- Update

type Msg
    = Add String
    | Complete Int
    | Input String
    | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add str ->
            { model
                | todos = (Todo False str <| List.length model.todos) :: model.todos
            } ! [ Dom.focus "focus" |> Task.attempt (\_ -> NoOp) ]

        Complete n ->
            { model
                | todos = List.map (\todo ->
                                        if todo.id == n
                                        then {todo | completed = not todo.completed}
                                        else todo
                                   ) model.todos
            } ! []

        Input message ->
            { model | input = message } ! []

        NoOp ->
            model ! []

-- View

view : Model -> Html Msg
view model =
    div [] (viewTodos (List.reverse model.todos) ++
                [ input [ onInput Input
                        , onEnter (Add model.input)
                        , autofocus True
                        , id "focus" ]
                      [ text model.input ]
                , div [] [ text (completedMessage model) ]
                ])

viewTodo : Todo -> Html Msg
viewTodo todo =
    div []
        [ checkbox (Complete todo.id)
        , text todo.message ]

viewTodos : List Todo -> List (Html Msg)
viewTodos todos =
    case todos of
        [] -> []
        todo :: todos ->
            viewTodo todo :: viewTodos todos

-- Helpers

onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                msg
            else
                NoOp
    in
        Events.on "keydown" (Json.map isEnter Events.keyCode)

checkbox : Msg -> Html Msg
checkbox msg =
    input [ type_ "checkbox"
          , onClick msg
          ] []

completedMessage : Model -> String
completedMessage model =
    let
        completed = List.filter (\todo -> todo.completed) model.todos
        completedCount = List.length completed
        totalCount = List.length model.todos
    in
        if totalCount > 0
        then (toString completedCount) ++ " / " ++ (toString totalCount) ++ " Completed"
        else ""
