module Bootstrap.Grid
    exposing
        ( container
        , containerFluid
        , simpleRow
        , row
        , col
        , colBreak
        , Column
        )

{-| Bootstrap includes a powerful mobile-first flexbox grid system for building layouts of all shapes and sizes. It’s based on a 12 column layout and has multiple tiers, one for each media query range.

    Grid.container
        [ Grid.row
            [ Row.topXs ]
            [ Grid.col
                [ Col.xs4 ]
                [ text "col1-row1"]

            , Grid.col
                [ Col.xs8 ]
                [ text "col2-row1"]
            ]
        , Grid.simpleRow
            [ Grid.col
                [ Col.xs4 ]
                [ text "col1-row1"]

            , Grid.col
                [ Col.xs6 ]
                [ text "col2-row1"]

            ]

        ]

# Containers
@docs container, containerFluid

# Rows
@docs row, simpleRow


# Columns
@docs col, colBreak, Column


-}

import Html exposing (Html, div, Attribute)
import Html.Attributes exposing (class, classList)
import Bootstrap.Grid.Internal as GridInternal
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row





{-| Opaque type represetning a column element
-}
type Column msg
    = Column
        { options : List (GridInternal.ColOption msg)
        , children : List (Html msg)
        }
    | ColBreak (Html.Html msg)


{-| Responsive fixed width container, which changes it's maz-width at breakpoint
-}
container : List (Attribute msg) -> List (Html msg) -> Html msg
container attributes children =
    div ([ class "container" ] ++ attributes) children


{-| Full width contaienr spanning the entire viewport
-}
containerFluid : List (Attribute msg) -> List (Html msg) -> Html msg
containerFluid attributes children =
    div ([ class "container-fluid" ] ++ attributes) children


{-| Create a row with no configuration options
-}
simpleRow : List (Column msg) -> Html msg
simpleRow cols =
    row [] cols


{-| Create a row

* `options` List of row options
* `cols` List of columns
-}
row : List (Row.Option msg) -> List (Column msg) -> Html msg
row options cols =
    div
        (GridInternal.rowAttributes options)
        (List.map renderCol cols)


{-| Create a column

* `options` List of column options
* `cols` List of child elments
-}
col : List (Col.Option msg) -> List (Html msg) -> Column msg
col options children =
    Column
        { options = options
        , children = children
        }

{-| Creates a full width column with no content. Handy for creating equal width multi-row columns.
-}
colBreak : List (Html.Attribute msg) -> Column msg
colBreak attributes =
    ColBreak <|
        Html.div
            ([class "w-100"] ++ attributes)
            []

renderCol : Column msg -> Html msg
renderCol column =
    case column of
        (Column { options, children }) ->
            div
                (GridInternal.colAttributes options)
                children
        (ColBreak e) ->
            e



