module Select exposing (selectList, Config)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Json

type alias Config a b =
    { items : List a
    , getId : a -> String
    , getLabel : a -> String
    , onSelect : a -> b
    , selected : a
    }

selectList : Config a b -> Html b
selectList config =
    (Html.select (attributes config) (options config))

attributes : Config a b -> List (Html.Attribute b)
attributes config =
    let
        idToMessage =
            (\id -> config.onSelect (findItemById config id))
        handleChange =
            Json.map (\id -> idToMessage id) Html.Events.targetValue
    in
        [ Html.Events.on "change" handleChange ]


options : Config a b -> List (Html b)
options config =
    let
        value =
            (\item -> Html.Attributes.value (config.getId item))
        selected =
            (\item -> Html.Attributes.selected ((config.getId item) == (config.getId config.selected)))
        label =
            (\item -> Html.text (config.getLabel item))
        itemToOption =
            (\item -> Html.option [ (selected item), (value item) ] [ label item ])
    in
        List.map (\item -> itemToOption item) config.items


findItemById : Config a b -> String -> a
findItemById config id =
    let
        candidates =
            List.filter (\item -> (config.getId item) == id) config.items
    in
        case candidates of
            [] ->
                Debug.crash "Selected an ID which was not in the list"
            x :: xs ->
                x
