module Tests exposing (..)

import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String

import Todo exposing (..)

expectAll : List Expect.Expectation -> Expect.Expectation
expectAll expects =
    case expects of
        [] -> Expect.pass
        expectation :: rest ->
            if expectation == Expect.pass
            then expectAll rest
            else expectation

all : Test
all =
    describe "Todo App"
        [ test "starts with an empty todo list" <| \() ->
              Expect.equal (List.length initModel.todos) 0
        , test "update will add todos" <| \() ->
              let
                  model = justModel <| update (Add "hello") initModel
              in
                  Expect.equal (List.length model.todos) 1
        , test "todos start uncompleted" <| \() ->
              let
                  model = justModel <| update (Add "hello") initModel
              in
                  Expect.true "" <| List.all (\todo -> todo.completed == False) model.todos
        , test "update will complete todos" <| \() ->
              let
                  model = (initModel
                          |> update (Add "hello")
                          |> justModel
                          |> update (Add "hello")
                          |> justModel
                          |> update (Complete 0)
                          |> justModel
                          )
              in
                  case model.todos of
                      t1 :: t2 :: [] -> expectAll
                                        [ Expect.equal t2.completed True
                                        , Expect.equal t2.id 0
                                        , Expect.equal t1.completed False
                                        ]
                      _ -> Expect.fail ""
        , test "gives unique ids to each todo" <| \() ->
            let
                model = (initModel
                        |> update (Add "hello")
                        |> justModel
                        |> update (Add "hello")
                        |> justModel
                        )
            in
                case model.todos of
                    t1 :: t2 :: [] -> Expect.notEqual t1.id t2.id
                    _ -> Expect.fail ""
        , test "view should display todos" <| \() ->
            let
                model = initModel |> update (Add "hello") |> justModel
            in
                (view model
                |> Query.fromHtml
                |> Query.has [ text "hello" ]
                )
        , test "input should default to empty" <| \() ->
            let
                model = initModel
            in
                Expect.equal model.input ""
        , test "update should update the input" <| \() ->
            let
                model = initModel |> update (Input "hello") |> justModel
            in
                Expect.equal model.input "hello"
        , test "input should be rendered" <| \() ->
            let
                model = initModel |> update (Input "hello") |> justModel
            in
                (view model
                |> Query.fromHtml
                |> Query.find [ tag "input" ]
                |> Query.has [ text "hello" ]
                )
        , test "todos should have checkboxes" <| \() ->
            let
                model = initModel |> update (Add "hello") |> justModel
            in
                (view model
                |> Query.fromHtml
                |> Query.has [ attribute "type" "checkbox" ]
                )
        , test "shows how many are completed" <| \() ->
            let
                model = {todos = [Todo True "" 0, Todo True "" 1, Todo False "" 2], input = ""}
            in
                (view model
                |> Query.fromHtml
                |> Query.has [ text "2 / 3 Completed" ]
                )
        , test "only show completed message if todos exist" <| \() ->
            (view initModel
            |> Query.fromHtml
            |> Query.findAll [ text "Completed" ]
            |> Query.count (Expect.equal 0)
            )
        ]

justModel : (Model, Cmd msg) -> Model
justModel mc =
    case mc of
        (model, _) -> model
