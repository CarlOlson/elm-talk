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
                  model = update (Add "hello") initModel
              in
                  Expect.equal (List.length model.todos) 1
        , test "todos start uncompleted" <| \() ->
              let
                  model = update (Add "hello") initModel
              in
                  Expect.true "" <| List.all (\todo -> todo.completed == False) model.todos
        , test "update will complete todos" <| \() ->
              let
                  model = (initModel
                          |> update (Add "hello")
                          |> update (Add "hello")
                          |> update (Complete 0)
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
                model = initModel |> update (Add "hello") |> update (Add "hello")
            in
                case model.todos of
                    t1 :: t2 :: [] -> Expect.notEqual t1.id t2.id
                    _ -> Expect.fail ""
        ]
