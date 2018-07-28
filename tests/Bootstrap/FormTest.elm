module Bootstrap.FormTest exposing (..)

import Bootstrap.Form.InputGroup as InputGroup
import Html.Styled as Html
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, classes, tag, text)


inputGroupWithPredecessorsAndSuccessors : Test
inputGroupWithPredecessorsAndSuccessors =
    let
        html =
            InputGroup.config
                (InputGroup.text
                    []
                )
                |> InputGroup.predecessors
                    [ InputGroup.span [] [ Html.text "$" ] ]
                |> InputGroup.successors
                    [ InputGroup.span [] [ Html.text ".00" ] ]
                |> InputGroup.view
                |> Html.toUnstyled
                |> Query.fromHtml
    in
    describe "Simple input group with predecessor and successor"
        [ test "ensure predecessor has correct class" <|
            \() ->
                html
                    |> Query.find [ class "input-group-prepend" ]
                    |> Query.has [ text "$" ]
        , test "ensure successor has correct class" <|
            \() ->
                html
                    |> Query.find [ class "input-group-append" ]
                    |> Query.has [ text ".00" ]
        ]
