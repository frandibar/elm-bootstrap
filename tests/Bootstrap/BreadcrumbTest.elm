module Bootstrap.BreadcrumbTest exposing (..)

import Bootstrap.Breadcrumb as Breadcrumb
import Expect
import Html as H
import Html.Attributes as HA
import Html.Styled exposing (Html, div, text, toUnstyled)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, tag)


testWithoutItems : Test
testWithoutItems =
    let
        html =
            Breadcrumb.container []
    in
        describe "Test without items"
            [ test "Expect an empty text node only" <|
                \() ->
                    html
                        |> Expect.equal (text "")
            ]


testWithTwoItems : Test
testWithTwoItems =
    let
        html =
            div []
                [ Breadcrumb.container
                    [ Breadcrumb.item [] [ text "home" ]
                    , Breadcrumb.item [] [ text "page" ]
                    ]
                ]
                |> toUnstyled
                |> Query.fromHtml
    in
        describe "Tests with two items"
            [ test "Expect the navigation with an aria-label" <|
                \() ->
                    html
                        |> Query.find [ tag "nav" ]
                        |> Query.has [ attribute <| HA.attribute "aria-label" "breadcrumb" ]
            , test "Expect the navigation with the role 'navigation'" <|
                \() ->
                    html
                        |> Query.find [ tag "nav" ]
                        |> Query.has [ attribute <| HA.attribute "role" "navigation" ]
            , test "Expect the orderen list with the class 'breadcrumb'" <|
                \() ->
                    html
                        |> Query.find [ tag "ol" ]
                        |> Query.has [ class "breadcrumb" ]
            , test "Expect two list items" <|
                \() ->
                    html
                        |> Query.findAll [ tag "li" ]
                        |> Query.count (Expect.equal 2)
            , test "Expect list items with class the 'breadcrumb-item'" <|
                \() ->
                    html
                        |> Query.findAll [ tag "li" ]
                        |> Query.each (Query.has [ class "breadcrumb-item" ])
            , test "Expect first element to contain 'home'" <|
                \() ->
                    html
                        |> Query.findAll [ tag "li" ]
                        |> Query.index 0
                        |> Query.contains [ H.text "home" ]
            , test "Expect second element to contain 'page'" <|
                \() ->
                    html
                        |> Query.findAll [ tag "li" ]
                        |> Query.index 1
                        |> Query.contains [ H.text "page" ]
            , test "Expect first element without an aria-current" <|
                \() ->
                    html
                        |> Query.findAll [ tag "li" ]
                        |> Query.index 0
                        |> Query.hasNot [ attribute <| HA.attribute "aria-current" "page" ]
            , test "Expect second/last element with an aria-current" <|
                \() ->
                    html
                        |> Query.findAll [ tag "li" ]
                        |> Query.index 1
                        |> Query.has [ attribute <| HA.attribute "aria-current" "page" ]
            ]
