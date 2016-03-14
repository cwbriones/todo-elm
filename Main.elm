module Main (..) where

import StartApp
import Html exposing (..)
import Html.Events exposing (on, onKeyDown, targetValue, keyCode)
import Html.Attributes exposing (..)
import Effects

import Http
import String
import Task exposing (Task)
import Json.Decode as Decode exposing (Decoder, (:=), succeed)
import Json.Encode as Encode

-- MODEL

type alias Todo =
  { completed : Bool
  , title : String
  , order : Int
  , id : String
  , url : String
  }

todoToList : Todo -> List (String, Encode.Value)
todoToList todo =
  [ ("completed", Encode.bool todo.completed)
  , ("title", Encode.string todo.title)
  , ("order", Encode.int todo.order)
  , ("id", Encode.string todo.id)
  , ("url", Encode.string todo.url)
  ]

initTodo : String -> Int -> Todo
initTodo title order =
  { completed = False
  , title = title
  , order = order
  , id = ""
  , url = ""
  }

type alias Model =
  { todos  : List Todo
  , field : String
  , id : Int
  }

init =
  { todos = []
  , field = ""
  , id = 0
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

    anySelected = List.any .completed model.todos

    clearCompleted =
      if anySelected then
        [button [class "clear-completed"] [text "Clear Completed"]]
      else
        []

    appFooter = footer [class "footer"]
      ([ span [class "todo-count"] [strong [] [text (toString (List.length model.todos))], text " items left"]
      , ul [class "filters"] filters
      ] ++ clearCompleted)

    infoFooter = footer [class "info"]
      [ p [] [text "Double-click to edit a todo"]
      , p [] [text "Written by Christian Briones"]
      ]

    is13 code = if code == 13 then Ok () else Err "not the right key code"
    onEnter address value =
      on "keydown"
        (Decode.customDecoder keyCode is13)
        (\_ -> Signal.message address value)

    todoApp = section [class "todoapp"]
      [ header [class "header"]
        [ h1 [] [ text "todos" ]
        , input 
          [ class "new-todo"
          , placeholder "What needs to be done?"
          , autofocus True
          , on "input" targetValue (Signal.message address << UpdateField)
          , onEnter address Create
          , value model.field
          ] []
        ]
      , section [class "main"]
        [ input [class "toggle-all", type' "checkbox"] []
        , ul [class "todo-list"] (List.map viewTodo model.todos)
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

type Action id field todos
  = Destroy id
  | Create
  | UpdateField field
  | MarkAsCompleted id
  | ClearAll
  | Success todos

update action model =
  let
    todos = model.todos
  in
    case action of
      UpdateField field -> ({model | field = field}, Effects.none)
      Destroy id -> (model, Effects.none)
      Create ->
        if model.field /= "" then
          createTodo model
        else
          (model, Effects.none)
      MarkAsCompleted id -> (model, Effects.none)
      ClearAll -> (model, clearAll todos)
      Success newTodos -> ({model | todos = newTodos}, Effects.none)

clearAll todos =
  let
    url = "http://localhost:8080/todos"
    request = httpDelete (succeed (Success [])) url
    neverFailingRequest =
      Task.onError request (\_ -> Task.succeed (Success todos))
  in
    Effects.task neverFailingRequest

loadTodos =
  let
    url = "http://localhost:8080/todos"
    request = Http.get (Decode.list todoDecoder) url
    onSuccess = Success << (List.sortBy .order)
    onError = \_ -> Task.succeed (Success [])
    neverFailingRequest =
      Task.onError (Task.map onSuccess request) onError
  in
    Effects.task neverFailingRequest

-- createTodo : List Todo -> Todo -> Effects.Effects (Action b c (List Todo))
createTodo model =
  let
    url = "http://localhost:8080/todos"
    todo = initTodo model.field model.id
    request = Http.post todoDecoder url (encodeTodo todo)
    onSuccess todo = Success (model.todos ++ [todo])
    onError = \_ -> Task.succeed (Success model.todos)
    neverFailingRequest =
      Task.onError (Task.map onSuccess request) onError

    nextId = model.id + 1
  in
    ( { model | id = nextId, field = ""}, Effects.task neverFailingRequest )

patchTodo : List Todo -> Todo -> Effects.Effects (Action b c (List Todo))
patchTodo todos todo =
  let
    url = todo.url
    request = httpPatch (succeed (Success todos)) url (encodeTodo todo)
    neverFailingRequest =
      Task.onError request (\_ -> Task.succeed (Success todos))
  in
    Effects.task neverFailingRequest

-- Encode / Decode

encodeTodo : Todo -> Http.Body
encodeTodo =
  Http.string
  << Encode.encode 0
  << Encode.object
  << todoToList

todoDecoder =
  let
    constructing = Decode.succeed
    apply = Decode.object2 (<|)
  in
    Decode.succeed Todo
      `apply` ("completed" := Decode.bool)
      `apply` ("title" := Decode.string)
      `apply` ("order" := Decode.int)
      `apply` ("id" := Decode.string)
      `apply` ("url" := Decode.string)

-- HTTP

httpDelete decoder url =
  let request =
    { verb = "DELETE"
    , headers = []
    , url = url
    , body = Http.empty
    }
  in
    Http.fromJson decoder (Http.send Http.defaultSettings request)

httpPatch : Decoder value -> String -> Http.Body -> Task Http.Error value
httpPatch decoder url body =
  let request =
    { verb = "PATCH"
    , headers = []
    , url = url
    , body = body
    }
  in
    Http.fromJson decoder (Http.send Http.defaultSettings request)

-- ENTRY

app = StartApp.start
    { init = (init, loadTodos)
    , update = update
    , view = view
    , inputs = []
    }

main = app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks
