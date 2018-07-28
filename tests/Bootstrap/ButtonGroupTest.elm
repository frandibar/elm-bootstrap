module Bootstrap.ButtonGroupTest exposing (..)

import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Button as Button
import Html.Styled as Html
import Html.Attributes as Attr
import Test exposing (Test, test, describe)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, class, classes, attribute, checked)


simpleGroup : Test
simpleGroup =
    let
        html =
            ButtonGroup.buttonGroup []
                [ ButtonGroup.button [] [ Html.text "First" ]
                , ButtonGroup.button [] [ Html.text "Second" ]
                ]
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Simple group"
            [ test "expect btn-group" <|
                \() ->
                    html
                        |> Query.has [ tag "div", class "btn-group", attribute <| Attr.attribute "role" "group" ]
            , test "expect btn class" <|
                \() ->
                    html
                        |> Query.findAll [ tag "button" ]
                        |> Query.each
                            (Query.has [ class "btn" ])
            ]


groupWithOptions : Test
groupWithOptions =
    let
        html =
            ButtonGroup.buttonGroup [ ButtonGroup.small, ButtonGroup.vertical ]
                [ ButtonGroup.button [ Button.primary ] [ Html.text "First" ] ]
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Optioned group"
            [ test "expect classes" <|
                \() ->
                    html
                        |> Query.has
                            [ classes
                                [ "btn-group"
                                , "btn-group-sm"
                                , "btn-group-vertical"
                                ]
                            ]
            , test "Expect button classes" <|
                \() ->
                    html
                        |> Query.children []
                        |> Query.index 1
                        |> Query.has
                            [ class "btn-primary"
                            , text "First"
                            ]
            ]


linkGroup : Test
linkGroup =
    let
        html =
            ButtonGroup.linkButtonGroup []
                [ ButtonGroup.linkButton [ Button.primary ] [ Html.text "First" ] ]
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Link group"
            [ test "Expect button classes" <|
                \() ->
                    html
                        |> Query.children []
                        |> Query.index 1
                        |> Query.has
                            [ tag "a"
                            , class "btn-primary"
                            , text "First"
                            ]
            ]


checkGroup : Test
checkGroup =
    let
        html =
            ButtonGroup.checkboxButtonGroup []
                [ ButtonGroup.checkboxButton False [ Button.primary ] [ Html.text "First" ] ]
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Checkbox group"
            [ test "Except label with classes" <|
                \() ->
                    html
                        |> Query.children []
                        |> Query.index 1
                        |> Query.has
                            [ tag "label"
                            , class "btn-primary"
                            , text "First"
                            ]
            , test "Expect checkbox input" <|
                \() ->
                    html
                        |> Query.children []
                        |> Query.index 1
                        |> Query.children []
                        |> Query.index 0
                        |> Query.has
                            [ tag "input"
                            , attribute <| Attr.attribute "type" "checkbox"
                            , checked False
                            ]
            ]


radioGroup : Test
radioGroup =
    let
        html =
            ButtonGroup.radioButtonGroup []
                [ ButtonGroup.radioButton False [ Button.primary ] [ Html.text "First" ] ]
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Radio group"
            [ test "Except label with classes" <|
                \() ->
                    html
                        |> Query.children []
                        |> Query.index 1
                        |> Query.has
                            [ tag "label"
                            , class "btn-primary"
                            , text "First"
                            ]
            , test "Expect radio input" <|
                \() ->
                    html
                        |> Query.children []
                        |> Query.index 1
                        |> Query.children []
                        |> Query.index 0
                        |> Query.has
                            [ tag "input"
                            , attribute <| Attr.attribute "type" "radio"
                            , checked False
                            ]
            ]


toolbar : Test
toolbar =
    let
        html =
            ButtonGroup.toolbar []
                [ ButtonGroup.buttonGroupItem []
                    [ ButtonGroup.button [] [ Html.text "Button" ] ]
                ]
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Toolbar"
            [ test "expect toolbar" <|
                \() ->
                    html
                        |> Query.has
                            [ tag "div"
                            , class "btn-toolbar"
                            ]
            , test "Expect button group" <|
                \() ->
                    html
                        |> Query.children []
                        |> Query.index 1
                        |> Query.has
                            [ tag "div"
                            , class "btn-group"
                            ]
            , test "Expect button" <|
                \() ->
                    html
                        |> Query.children []
                        |> Query.index 1
                        |> Query.children []
                        |> Query.index 0
                        |> Query.has
                            [ tag "button"
                            , class "btn"
                            ]
            ]
