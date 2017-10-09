module Main exposing (main)

import Color
import Element exposing (Element, column, el, layout, row, text)
import Element.Attributes exposing (center, height, px, verticalCenter, width)
import Element.Input as Input
import Html exposing (Html, input)
import Html.Attributes as Attr exposing (type_)
import Html.Events exposing (onInput)
import Style exposing (style, styleSheet)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


type alias Model =
    { sign : Sign
    , significand : Int
    , mantissa : Int
    }


type Msg
    = SignificandChange String
    | MantissaChange String
    | NoOp


type Sign
    = Pos
    | Neg


init : Model
init =
    { sign = Pos
    , significand = 0
    , mantissa = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SignificandChange s ->
            let
                newSig =
                    String.toInt s |> Result.map (clamp 0 255) |> Result.withDefault model.significand
            in
            { model | significand = newSig }

        MantissaChange m ->
            let
                newMan =
                    String.toInt m |> Result.map (clamp 0 8388607) |> Result.withDefault model.mantissa
            in
            { model | mantissa = newMan }

        NoOp ->
            model


view : Model -> Html Msg
view model =
    layout myStyle <|
        el None [ center, width (px 800) ] <|
            column None
                []
                [ row None
                    []
                    [ text "Floating point visualization" ]
                , viewFloat model
                , controlRow model
                ]


viewFloat : Model -> Element Styles variation Msg
viewFloat { sign, significand, mantissa } =
    row None [] <| List.concat [ viewSign sign, viewSignificand significand, viewMantissa mantissa ]


controlRow : Model -> Element Styles variation Msg
controlRow { sign, significand, mantissa } =
    row None
        []
        [ el None
            []
            (Element.html <|
                input
                    [ type_ "range"
                    , Attr.min "0"
                    , Attr.max "255"
                    , Attr.step "1"
                    , Attr.value <| toString significand
                    , Attr.style [ ( "width", "300px" ) ]
                    , onInput SignificandChange
                    ]
                    []
            )
        , el None
            [ width (px 25) ]
            (Input.text None
                []
                { onChange = SignificandChange
                , value = toString significand
                , label = Input.hiddenLabel ""
                , options = []
                }
            )
        , el None
            []
            (Element.html <|
                input
                    [ type_ "range"
                    , Attr.min "0"
                    , Attr.max "8388607"
                    , Attr.step "1"
                    , Attr.value (toString mantissa)
                    , Attr.style [ ( "width", "600px" ) ]
                    , onInput MantissaChange
                    ]
                    []
            )
        , el None
            [ width (px 60) ]
            (Input.text None
                []
                { onChange = MantissaChange
                , value = toString mantissa
                , label = Input.hiddenLabel ""
                , options = []
                }
            )
        ]


toBinDigits : Int -> Int -> List Int
toBinDigits padZeroCount x =
    let
        extractDigits n =
            if n <= 0 then
                []
            else
                (n % 2) :: extractDigits (n // 2)

        digs =
            List.reverse <|
                if x <= 0 then
                    [ 0 ]
                else
                    extractDigits x
    in
    List.repeat (padZeroCount - List.length digs) 0 ++ digs


viewSign : Sign -> List (Element Styles variation Msg)
viewSign sign =
    let
        digit =
            case sign of
                Pos ->
                    0

                Neg ->
                    1
    in
    [ digitBox DigitBoxRed digit ]


viewSignificand : Int -> List (Element Styles variation Msg)
viewSignificand significand =
    toBinDigits 8 significand |> List.map (digitBox DigitBoxBlue)


viewMantissa : Int -> List (Element Styles variation Msg)
viewMantissa mantissa =
    toBinDigits 23 mantissa |> List.map (digitBox DigitBoxRed)


digitBox : Styles -> Int -> Element Styles variation Msg
digitBox st d =
    el st [ width (px 40), height (px 40), verticalCenter ] (text <| toString d)


type Styles
    = None
    | DigitBoxRed
    | DigitBoxBlue


myStyle : Style.StyleSheet Styles variation
myStyle =
    styleSheet
        [ style None []
        , style DigitBoxRed
            [ Border.all 2
            , Color.background Color.lightRed
            , Font.center
            , Font.size 35
            ]
        , style DigitBoxBlue
            [ Border.all 2
            , Color.background Color.lightBlue
            , Font.center
            , Font.size 35
            ]
        ]
