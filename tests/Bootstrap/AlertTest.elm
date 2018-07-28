module Bootstrap.AlertTest exposing (..)

import Bootstrap.Alert as Alert
import Html.Styled as Html
import Html.Styled.Attributes
import Test exposing (Test, test, describe)
import Expect
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, class, classes)


simpleAlerts : Test
simpleAlerts =
    let
        alert txt alertFn =
            alertFn [] [ Html.text txt ]

        html =
            Html.div
                []
                [ alert "primary" Alert.simplePrimary
                , alert "secondary" Alert.simpleSecondary
                , alert "success" Alert.simpleSuccess
                , alert "info" Alert.simpleInfo
                , alert "warning" Alert.simpleWarning
                , alert "danger" Alert.simpleDanger
                , alert "light" Alert.simpleLight
                , alert "dark" Alert.simpleDark
                ]
                |> Html.toUnstyled
                |> Query.fromHtml

        expectRoled roleTxt =
            html
                |> Query.find [ class ("alert-" ++ roleTxt) ]
                |> Query.has [ text roleTxt ]
    in
        describe "Simple alerts"
            [ test "Expect 8 alerts" <|
                \() ->
                    html
                        |> Query.findAll [ class "alert" ]
                        |> Query.count (Expect.equal 8)
            , test "Expect primary" <|
                \() ->
                    expectRoled "primary"
            , test "Expect secondary" <|
                \() ->
                    expectRoled "secondary"
            , test "Expect success" <|
                \() ->
                    expectRoled "success"
            , test "Expect info" <|
                \() ->
                    expectRoled "info"
            , test "Expect warning" <|
                \() ->
                    expectRoled "warning"
            , test "Expect danger" <|
                \() ->
                    expectRoled "danger"
            , test "Expect light" <|
                \() ->
                    expectRoled "light"
            , test "Expect dark" <|
                \() ->
                    expectRoled "dark"
            ]


alertWithLink : Test
alertWithLink =
    let
        html =
            Alert.simpleInfo [] [ Alert.link [] [ Html.text "link" ] ]
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Alert with link"
            [ test "Expect link class and text" <|
                \() ->
                    html
                        |> Query.find [ tag "a" ]
                        |> Query.has [ class "alert-link", text "link" ]
            ]


alertWithHeaders : Test
alertWithHeaders =
    let
        html =
            Alert.simpleInfo []
                [ Alert.h1 [] [ Html.text "h1" ]
                , Alert.h2 [] [ Html.text "h2" ]
                , Alert.h3 [] [ Html.text "h3" ]
                , Alert.h4 [] [ Html.text "h4" ]
                , Alert.h5 [] [ Html.text "h5" ]
                , Alert.h6 [] [ Html.text "h6" ]
                ]
                |> Html.toUnstyled
                |> Query.fromHtml

        expectH txt =
            html
                |> Query.find [ tag txt ]
                |> Query.has [ text txt ]
    in
        describe "Alert with headers"
            [ test "Expect link class and text" <|
                \() ->
                    html
                        |> Query.findAll [ class "alert-header" ]
                        |> Query.count (Expect.equal 6)
            , test "Expect h1" <| \() -> expectH "h1"
            , test "Expect h2" <| \() -> expectH "h2"
            , test "Expect h3" <| \() -> expectH "h3"
            , test "Expect h4" <| \() -> expectH "h4"
            , test "Expect h5" <| \() -> expectH "h5"
            , test "Expect h6" <| \() -> expectH "h6"
            ]


alertWithAttributes : Test
alertWithAttributes =
    let
        html =
            Alert.simpleInfo [ Html.Styled.Attributes.class "my-class" ] []
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Alert with attributes"
            [ test "Expect has passed class" <|
                \() ->
                    html
                        |> Query.has [ classes [ "my-class" ] ]
            ]
