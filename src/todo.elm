module Todo exposing (..)

import Html exposing (Html, div)

main = Html.beginnerProgram
       { model = initModel
       , update = update
       , view = view
       }

type alias Todo =
    { completed : Bool
    , message : String
    , id : Int
    }

type alias Model =
    { todos : List Todo
    }

type Msg
    = Add String
    | Complete Int

initModel : Model
initModel = Model []

update : Msg -> Model -> Model
update msg model =
    case msg of
        Add str ->
            { model
                | todos = (Todo False str <| List.length model.todos) :: model.todos
            }
        Complete n ->
            { model
                | todos = List.map (\todo ->
                                        if todo.id == n
                                        then {todo | completed = True}
                                        else todo
                                   ) model.todos
            }

view : Model -> Html msg
view model = div [] []
