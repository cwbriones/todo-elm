module Main (..) where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects

import Http

-- MODEL

type alias Todo =
  { completed : Bool
  , title : String
  , order : Int
  , id : String
  , url : String
  }

init = []

initTodo title =
  { completed = False
  , title = title
  , order = 0
  , id = 1
  , url = ""
  }

-- VIEW

view address model =
  let
    filterView = \(url, label) -> li [class "filter"] [a [href url] [text label]]

    filters = List.map filterView
      [ ("", "All")
      , ("#/active", "Active")
      , ("#/completed", "Completed")
      ]

    anySelected = List.any .completed model

    clearCompleted =
      if anySelected then
        [button [class "clear-completed"] [text "Clear Completed"]]
      else
        []

    appFooter = footer [class "footer"]
      ([ span [class "todo-count"] [strong [] [text (toString (List.length model))], text " items left"]
      , ul [class "filters"] filters
      ] ++ clearCompleted)

    infoFooter = footer [class "info"]
      [ p [] [text "Double-click to edit a todo"]
      , p [] [text "Written by Christian Briones"]
      ]

    todoApp = section [class "todoapp"]
      [ header [class "header"]
        [ h1 [] [ text "todos" ]
        , input [class "new-todo", placeholder "What needs to be done?", autofocus True] []
        ]
      , section [class "main"]
        [ input [class "toggle-all", type' "checkbox"] []
        , ul [class "todo-list"] (List.map viewTodo model)
        ]
      , appFooter
      ]
  in
    div []
      [ todoApp
      , infoFooter
      ]

viewTodo todo =
  let
    attributes =
      if todo.completed then
        [class "view", class "completed"]
      else
        [class "view"]
  in
    li attributes
      [ input [class "toggle", type' "checkbox"] []
      , label [] [text todo.title]
      , button [class "destroy"] [text ""]
      , input [class "edit", value todo.title] []
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
