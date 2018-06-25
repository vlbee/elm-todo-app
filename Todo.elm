module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { newTodo : String
    , todos : List Task
    , view : Visibility
    }


type Visibility
    = All
    | Active
    | Complete


type alias Task =
    { task : String
    , complete : Bool
    }


model : Model
model =
    { newTodo = ""
    , todos = todosDefault
    , view = All
    }


todosDefault : List Task
todosDefault =
    [ { task = "buy milk"
      , complete = False
      }
    , { task = "learn Elm"
      , complete = True
      }
    , { task = "attend hackathon"
      , complete = False
      }
    ]


filterTodos : Model -> List Task
filterTodos model =
    case model.view of
        All ->
            model.todos

        Active ->
            List.filter (\task -> not task.complete) model.todos

        Complete ->
            List.filter (\task -> task.complete) model.todos


createTask : String -> Task
createTask string =
    { task = string
    , complete = False
    }


addTodo : Model -> Model
addTodo model =
    if model.newTodo /= "" then
        { model | todos = createTask model.newTodo :: model.todos, newTodo = "" }
    else
        model



-- UPDATE


type Msg
    = Filter Visibility
    | NewTodo String
    | AddTodo
    | KeyDown Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Filter visibility ->
            { model | view = visibility }

        NewTodo input ->
            { model | newTodo = input }

        AddTodo ->
            addTodo model

        KeyDown key ->
            if key == 13 then
                addTodo model
            else
                model


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", value model.newTodo, placeholder "Add a ToDo", onInput NewTodo, onKeyDown KeyDown ]
            []
        , button [ type_ "reset", onClick AddTodo ] [ text "Add" ]
        , ul
            []
            (filterTodos model |> List.map (\item -> li [] [ text item.task ]))
        , button [ onClick (Filter All) ] [ text "All" ]
        , button [ onClick (Filter Active) ] [ text "Active" ]
        , button [ onClick (Filter Complete) ] [ text "Complete" ]
        ]


stylesheetLink : Html Msg
stylesheetLink =
    node "link"
        [ rel "stylesheet"
        , href "https://unpkg.com/tachyons@4.8.0/css/tachyons.min.css"
        ]
        []
