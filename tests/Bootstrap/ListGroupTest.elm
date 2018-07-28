module Bootstrap.ListGroupTest exposing (contextual, contextualListGroup, customListGroup, vanillaListGroup)

import Bootstrap.ListGroup as ListGroup
import Expect
import Html.Styled exposing (text, toUnstyled)
import Html.Styled.Attributes as Attr exposing (href)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (attribute, class, classes, disabled, tag)


vanillaListGroup : Test
vanillaListGroup =
    let
        items =
            [ ( "basic", ListGroup.li [] [ text "basic" ] )
            , ( "active", ListGroup.li [ ListGroup.active ] [ text "active" ] )
            , ( "disabled", ListGroup.li [ ListGroup.disabled ] [ text "disabled" ] )
            ]

        vanilla =
            ListGroup.ul (List.map Tuple.second items)
                |> toUnstyled
                |> Query.fromHtml

        keyed =
            ListGroup.keyedUl items
                |> toUnstyled
                |> Query.fromHtml

        tests html =
            [ test "expect ul with list-group class" <|
                \() ->
                    html
                        |> Query.has [ tag "ul", class "list-group" ]
            , test "expect three li's with list-group-item class" <|
                \() ->
                    html
                        |> Query.findAll [ tag "li", class "list-group-item" ]
                        |> Query.count (Expect.equal 3)
            , test "expect li has content" <|
                \() ->
                    html
                        |> Query.findAll [ tag "li" ]
                        |> Query.index 0
                        |> Query.has [ Selector.text "basic" ]
            , test "expect li with active class" <|
                \() ->
                    html
                        |> Query.findAll [ tag "li" ]
                        |> Query.index 1
                        |> Query.has [ class "active" ]
            , test "expect li with disabled class and attribute" <|
                \() ->
                    html
                        |> Query.findAll [ tag "li" ]
                        |> Query.index 2
                        |> Query.has [ class "disabled", Selector.disabled True ]
            ]
    in
    describe "basic ListGroup"
        [ describe "vanilla ListGroup" (tests vanilla)
        , describe "keyed ListGroup" (tests keyed)
        ]


customListGroup : Test
customListGroup =
    let
        anchorItems =
            [ ( "item 1", ListGroup.anchor [ ListGroup.attrs [ href "javascript:void();" ] ] [ text "List item 1" ] )
            , ( "item 2", ListGroup.anchor [ ListGroup.active, ListGroup.attrs [ href "javascript:void();" ] ] [ text "List item 2" ] )
            , ( "item 3", ListGroup.anchor [ ListGroup.disabled, ListGroup.attrs [ href "http://www.google.com" ] ] [ text "List item 3" ] )
            ]

        buttonItems =
            [ ( "item 1", ListGroup.button [] [ text "List item 1" ] )
            , ( "item 2", ListGroup.button [ ListGroup.active ] [ text "List item 2" ] )
            , ( "item 3", ListGroup.button [ ListGroup.disabled ] [ text "List item 3" ] )
            ]

        tests itemTag html =
            [ test "expect div with list-group class" <|
                \() ->
                    html
                        |> Query.has [ tag "div", class "list-group" ]
            , test "expect three items with list-group-item class" <|
                \() ->
                    html
                        |> Query.findAll [ tag itemTag, class "list-group-item", class "list-group-item-action" ]
                        |> Query.count (Expect.equal 3)
            , test "expect item has content" <|
                \() ->
                    html
                        |> Query.findAll [ tag itemTag ]
                        |> Query.index 0
                        |> Query.has [ Selector.text "List item 1" ]
            , test "expect item with active class" <|
                \() ->
                    html
                        |> Query.findAll [ tag itemTag ]
                        |> Query.index 1
                        |> Query.has [ class "active" ]
            , test "expect item with disabled class and attribute" <|
                \() ->
                    html
                        |> Query.findAll [ tag itemTag ]
                        |> Query.index 2
                        |> Query.has [ class "disabled", Selector.disabled True ]
            ]

        anchors =
            ListGroup.custom (List.map Tuple.second anchorItems)
                |> toUnstyled
                |> Query.fromHtml

        keyedAnchors =
            ListGroup.keyedCustom anchorItems
                |> toUnstyled
                |> Query.fromHtml

        buttons =
            ListGroup.custom (List.map Tuple.second buttonItems)
                |> toUnstyled
                |> Query.fromHtml

        keyedButtons =
            ListGroup.keyedCustom buttonItems
                |> toUnstyled
                |> Query.fromHtml
    in
    describe "custom ListGroup"
        [ describe "anchors" (tests "a" anchors)
        , describe "keyed anchors" (tests "a" keyedAnchors)
        , describe "buttons" (tests "button" buttons)
        , describe "keyed buttons" (tests "button" keyedButtons)
        ]


contextual : String -> Query.Single msg -> Test
contextual name html =
    describe name
        [ test "expect primary" <|
            \() ->
                html
                    |> Query.find [ class "list-group-item-primary" ]
                    |> Query.has [ Selector.text "primary" ]
        , test "expect secondary" <|
            \() ->
                html
                    |> Query.find [ class "list-group-item-secondary" ]
                    |> Query.has [ Selector.text "secondary" ]
        , test "expect success" <|
            \() ->
                html
                    |> Query.find [ class "list-group-item-success" ]
                    |> Query.has [ Selector.text "success" ]
        , test "expect info" <|
            \() ->
                html
                    |> Query.find [ class "list-group-item-info" ]
                    |> Query.has [ Selector.text "info" ]
        , test "expect warning" <|
            \() ->
                html
                    |> Query.find [ class "list-group-item-warning" ]
                    |> Query.has [ Selector.text "warning" ]
        , test "expect danger" <|
            \() ->
                html
                    |> Query.find [ class "list-group-item-danger" ]
                    |> Query.has [ Selector.text "danger" ]
        , test "expect light" <|
            \() ->
                html
                    |> Query.find [ class "list-group-item-light" ]
                    |> Query.has [ Selector.text "light" ]
        , test "expect dark" <|
            \() ->
                html
                    |> Query.find [ class "list-group-item-dark" ]
                    |> Query.has [ Selector.text "dark" ]
        ]


contextualListGroup : Test
contextualListGroup =
    let
        contextualList =
            ListGroup.ul
                [ ListGroup.li [ ListGroup.primary ] [ text "primary" ]
                , ListGroup.li [ ListGroup.secondary ] [ text "secondary" ]
                , ListGroup.li [ ListGroup.success ] [ text "success" ]
                , ListGroup.li [ ListGroup.info ] [ text "info" ]
                , ListGroup.li [ ListGroup.warning ] [ text "warning" ]
                , ListGroup.li [ ListGroup.danger ] [ text "danger" ]
                , ListGroup.li [ ListGroup.light ] [ text "light" ]
                , ListGroup.li [ ListGroup.dark ] [ text "dark" ]
                ]
                |> toUnstyled
                |> Query.fromHtml

        contextualButtonList =
            ListGroup.custom
                [ ListGroup.button [ ListGroup.primary ] [ text "primary" ]
                , ListGroup.button [ ListGroup.secondary ] [ text "secondary" ]
                , ListGroup.button [ ListGroup.success ] [ text "success" ]
                , ListGroup.button [ ListGroup.info ] [ text "info" ]
                , ListGroup.button [ ListGroup.warning ] [ text "warning" ]
                , ListGroup.button [ ListGroup.danger ] [ text "danger" ]
                , ListGroup.button [ ListGroup.light ] [ text "light" ]
                , ListGroup.button [ ListGroup.dark ] [ text "dark" ]
                ]
                |> toUnstyled
                |> Query.fromHtml
    in
    describe "contextual ListGroup"
        [ contextual "ListGroup with contextual items" contextualList
        , contextual "custom ListGroup with contextual items" contextualButtonList
        ]
