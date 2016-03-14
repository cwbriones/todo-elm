module Main (..) where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects

import Http

-- MODEL

type alias Todo =
  { completed : Bool
  , order : Int
  , id : String
  , url : String
  }

init = ["one", "two", "three"]

-- VIEW

view address model =
  let
    filterView = \(url, label) -> li [class "filter"] [a [href url] [text label]]

    filters = List.map filterView
      [ ("", "All")
      , ("#/active", "Active")
      , ("#/completed", "Completed")
      ]

    appFooter = footer [class "footer"]
      [ span [class "todo-count"] [strong [] [text (toString (List.length model))], text " items left"]
      , ul [class "filters"] filters
      ]

    infoFooter = footer [class "info"]
      [ p [] [text "Double-click to edit a todo"]
      , p [] [text "Written by Christian Briones"]
      ]

    todoApp = section [class "todoapp"]
      [ header [class "header"]
        [ h1 [] [ text "todos" ]
        , input [class "new-todo", placeholder "What needs to be done?", autofocus True] []
        ]
      , section [class "main"] [ul [class "todo-list"] (List.map viewTodo model)]
      , appFooter
      ]
  in
    div []
      [ todoApp
      , infoFooter
      ]

viewTodo todo =
  li [class "view"]
    [ input [class "toggle", type' "checkbox"] []
    , label [] [text "A todo."]
    , button [class "destroy"] []
    , input [class "edit", value "A todo."] []
    ]

-- UPDATE

update action model =
  (model, Effects.none)

app = StartApp.start
    { init = (init, Effects.none)
    , update = update
    , view = view
    , inputs = []
    }

main = app.html
