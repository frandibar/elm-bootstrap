module Bootstrap.GridTest exposing (..)

import Bootstrap.Grid exposing (..)
import Bootstrap.Grid.Col exposing (..)
import Html.Styled as Html
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, classes, tag, text)


orderClass : Test
orderClass =
    test "Correct class generation when using order classes" <|
        \_ ->
            row []
                [ col
                    [ orderXsFirst
                    , orderSm3
                    , orderMd6
                    , orderLg9
                    , orderXlLast
                    ]
                    []
                ]
                |> Html.toUnstyled
                |> Query.fromHtml
                |> Query.find [ class "col" ]
                |> Query.has
                    [ class "order-first"
                    , class "order-sm-3"
                    , class "order-md-6"
                    , class "order-lg-9"
                    , class "order-xl-last"
                    ]
