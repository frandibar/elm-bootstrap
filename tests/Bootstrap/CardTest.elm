module Bootstrap.CardTest exposing (..)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Text as Text
import Html.Styled as Html
import Html.Styled.Attributes exposing (src)
import Html.Attributes as HA
import Test exposing (Test, test, describe)
import Expect
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, class, classes, attribute)


emptySimpleCard : Test
emptySimpleCard =
    let
        html =
            Card.config []
                |> Card.view
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Simple card no options"
            [ test "expect card class" <|
                \() ->
                    html
                        |> Query.has [ classes [ "card" ] ]
            ]


notSoSimpleCard : Test
notSoSimpleCard =
    let
        html =
            Card.config
                [ Card.align Text.alignXsCenter
                , Card.outlineInfo
                ]
                |> Card.block []
                    [ Block.titleH1 [] [ Html.text "titleh1" ]
                    , Block.text [] [ Html.text "cardtext" ]
                    , Block.link [] [ Html.text "link" ]
                    , Block.quote [] [ Html.text "blockquote" ]
                    ]
                |> Card.view
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Simple card with options and items"
            [ test "expect classes" <|
                \() ->
                    html
                        |> Query.has [ classes [ "card", "border-info", "text-center" ] ]
            , test "expect title" <|
                \() ->
                    html
                        |> Query.find [ tag "h1" ]
                        |> Query.has [ class "card-title", text "titleh1" ]
            , test "expect text paragraph" <|
                \() ->
                    html
                        |> Query.find [ tag "p" ]
                        |> Query.has [ class "card-text", text "cardtext" ]
            , test "expect link" <|
                \() ->
                    html
                        |> Query.find [ tag "a" ]
                        |> Query.has [ class "card-link", text "link" ]
            , test "expect blockquote" <|
                \() ->
                    html
                        |> Query.find [ tag "blockquote" ]
                        |> Query.has [ class "card-blockquote", text "blockquote" ]
            ]


cardFullMonty : Test
cardFullMonty =
    let
        html =
            Card.config [ Card.outlineInfo ]
                |> Card.headerH1 [] [ Html.text "Header" ]
                |> Card.footer [] [ Html.text "Footer" ]
                |> Card.imgTop [ src "/imgtop.jpg" ] []
                |> Card.imgBottom [ src "/imgbottom.jpg" ] []
                |> Card.block [] [ Block.text [] [ Html.text "cardblock" ] ]
                |> Card.view
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Card with everything in it"
            [ test "expect classes" <|
                \() ->
                    html
                        |> Query.has [ classes [ "card", "border-info" ] ]
            , test "expect card header" <|
                \() ->
                    html
                        |> Query.find [ class "card-header" ]
                        |> Query.has [ tag "h1", text "Header" ]
            , test "expect card footer" <|
                \() ->
                    html
                        |> Query.find [ class "card-footer" ]
                        |> Query.has [ text "Footer" ]
            , test "expect card image top" <|
                \() ->
                    html
                        |> Query.find [ class "card-img-top" ]
                        |> Query.has [ attribute <| HA.attribute "src" "/imgtop.jpg" ]
            , test "expect card image bottom" <|
                \() ->
                    html
                        |> Query.find [ class "card-img-bottom" ]
                        |> Query.has [ attribute <| HA.attribute "src" "/imgbottom.jpg" ]
            , test "expect card block" <|
                \() ->
                    html
                        |> Query.find [ class "card-body" ]
                        |> Query.has [ text "cardblock" ]
            ]


group : Test
group =
    let
        html =
            (Card.group <| cardList 3)
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Card group"
            [ test "expect classes" <|
                \() ->
                    html
                        |> Query.has [ class "card-group" ]
            , test "expect 3 cards" <|
                \() ->
                    html
                        |> Query.findAll [ class "card" ]
                        |> Query.count (Expect.equal 3)
            ]


deck : Test
deck =
    let
        html =
            (Card.deck <| cardList 3)
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Card deck"
            [ test "expect classes" <|
                \() ->
                    html
                        |> Query.has [ class "card-deck" ]
            , test "expect 3 cards" <|
                \() ->
                    html
                        |> Query.findAll [ class "card" ]
                        |> Query.count (Expect.equal 3)
            ]


columns : Test
columns =
    let
        html =
            (Card.columns <| cardList 3)
                |> Html.toUnstyled
                |> Query.fromHtml
    in
        describe "Card columns with everything in it"
            [ test "expect classes" <|
                \() ->
                    html
                        |> Query.has [ class "card-columns" ]
            , test "expect 3 cards" <|
                \() ->
                    html
                        |> Query.findAll [ class "card" ]
                        |> Query.count (Expect.equal 3)
            ]


cardList : Int -> List (Card.Config msg)
cardList count =
    List.repeat count <|
        Card.config []
