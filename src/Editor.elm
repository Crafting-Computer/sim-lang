port module Editor exposing (..)


import Browser
import Html exposing (Html)
import Html.Attributes
import Element as E
import Element.Input as Input
import Element.Font as Font
import HdlParser exposing (Size(..))
import HdlChecker
import HdlEmitter
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Dict exposing (Dict)
import Binary


port setEditorValuePort : String -> Cmd msg
port editorValueChangedPort : (String -> msg) -> Sub msg
port generateTruthTablePort : Encode.Value -> Cmd msg
port receiveTruthTablePort : (String -> msg) -> Sub msg


type alias Model =
  { hdlSource : String
  , hdlOutput : Result String (List HdlEmitter.DefOutput)
  , truthTable : TruthTable
  }


type Msg
  = EditorValueChanged String
  | TruthTableReceived String


type alias TruthTable =
  Dict
    String
    { params : List String
    , outputs : List String
    , body : List (List Int)
    }


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EditorValueChanged newValue ->
      let
        hdlOutput =
          compileHdl newValue
      in
      ({ model
        | hdlSource =
          newValue
        , hdlOutput =
          hdlOutput
      }
      , case hdlOutput of
        Ok defs ->
          generateTruthTable defs
        Err _ ->
          Cmd.none
      )
    TruthTableReceived tableJson ->
      ({ model
        | truthTable =
          Result.withDefault model.truthTable <| Decode.decodeString decodeTruthTable tableJson
      }
      , Cmd.none
      )



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ editorValueChangedPort EditorValueChanged
    , receiveTruthTablePort TruthTableReceived
    ]


view : Model -> Html Msg
view model =
  E.layout
    [ E.htmlAttribute <| Html.Attributes.style "margin" "0 10px"
    , E.htmlAttribute <| Html.Attributes.style "width" "45vw"
    , Font.size 16
    ] <|
    E.column
      []
      [ viewRightPanel model
      ]

viewRightPanel : Model -> E.Element Msg
viewRightPanel model =
  case model.hdlOutput of
    Ok _ -> viewTruthTable model.truthTable
    Err str -> E.html <| Html.pre [ Html.Attributes.style "white-space" "pre-wrap" ] [ Html.text str ]


viewTruthTable : TruthTable -> E.Element Msg
viewTruthTable table =
  E.column [ E.spacing 10 ] <|
  [ E.html <|
    Html.div [] <|
    Dict.foldl
      (\defName defTable viewList ->
        let
          header =
            defTable.params ++ defTable.outputs
        in
        ( Html.table
          [ Html.Attributes.style "table-layout" "fixed"
          , Html.Attributes.style "width" "100%"
          , Html.Attributes.style "border" "1px grey solid"
          , Html.Attributes.style "border-collapse" "collapse"
          ] <|
          Html.caption [ Html.Attributes.style "font-weight" "bold", Html.Attributes.style "margin-bottom" "10px" ] [ Html.text defName ]
          :: Html.thead []
            (List.map
              (\name ->
                Html.th
                [ Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "width" <| (String.fromFloat <| 100 / (toFloat <| List.length header)) ++ "%"
                , Html.Attributes.style "border" "1px grey solid"
                , Html.Attributes.style "background-color" <|
                  if List.member name defTable.params then
                    "lightgreen"
                  else
                    "#ffd8a7"
                ]
                [ Html.text name ]
              )
            header
            )
          :: List.map (\row ->
            Html.tr
            [ Html.Attributes.style "text-align" "center"
            ] <|
            List.map (\value ->
              Html.td
              [ Html.Attributes.style "padding" "10px"
              , Html.Attributes.style "width" <| (String.fromFloat <| 100 / (toFloat <| List.length row)) ++ "%"
              , Html.Attributes.style "border" "1px grey solid"
              ]
              [ Html.text <| String.fromInt <| Binary.toDecimal <| Binary.fromDecimal value ]) row
          )
          defTable.body
        )
        :: viewList
      )
      []
      table
  ]


init : () -> ( Model, Cmd Msg )
init _ =
  let
    hdlSource =
      """{- sample full adder -}

full_adder a b c -> { sum, carry } =
    let
        { sum = s1, carry = c1 } = half_adder a b
        { sum = s2, carry = c2 } = half_adder s1 c
        c3 = or c1 c2
    in
    { sum = s2, carry = c3 }

half_adder a b -> { sum, carry } =
    let
        sum = xor a b
        carry = and a b
    in
    { sum = sum, carry = carry }

xor a[n] b[n] -> [n] = -- [n] specifies a variable sized bus
    let
        nand_a_b = nand a b
    in
    nand
    (nand a nand_a_b)
    (nand b nand_a_b)

or a[n] b[n] -> [n] =
    nand (not a) (not b)

not a[n] -> [n] =
    nand a a

and a[n] b[n] -> [n] =
    let
        nand_a_b = nand a b
    in
    nand nand_a_b nand_a_b
    """
    hdlOutput = compileHdl hdlSource
  in
  ({ hdlSource = hdlSource
  , hdlOutput = hdlOutput
  , truthTable = Dict.empty
  }
  , Cmd.batch
    [ setEditorValuePort hdlSource
    , case hdlOutput of
      Ok defs ->
        generateTruthTable defs
      Err _ ->
        Cmd.none
    ]
  )

generateTruthTable : List HdlEmitter.DefOutput -> Cmd Msg
generateTruthTable defs =
  let
    -- { name : String
    -- , params : List ParamOutput
    -- , outputs : List ParamOutput
    -- , body : String
    -- }
    encodeDefOutput def =
      Encode.object
        [ ( "name", Encode.string def.name )
        , ( "params", Encode.list encodeParamOutput def.params )
        , ( "outputs", Encode.list encodeParamOutput def.outputs )
        , ( "body", Encode.string def.body )
        ]

    -- { name : String
    -- , size : Size
    -- }
    encodeParamOutput param =
      Encode.object
        [ ( "name", Encode.string param.name )
        , ( "size", encodeSize param.size )
        ]

    -- type Size
    --   = IntSize Int
    --   | VarSize String (Maybe Int)
    encodeSize size =
      case size of
        IntSize i ->
          Encode.int i
        VarSize n _ ->
          Encode.string n
  in
  generateTruthTablePort <| Encode.list encodeDefOutput defs

-- Dict
--     String
--     { header : List String
--     , body : List Int
--     }
decodeTruthTable : Decoder TruthTable
decodeTruthTable =
  let
    decodeTable =
      Field.require "params" (Decode.list Decode.string) <| \params ->
      Field.require "outputs" (Decode.list Decode.string) <| \outputs ->
      Field.require "body" (Decode.list <| Decode.list Decode.int) <| \body ->

      Decode.succeed
        { params = params
        , outputs = outputs
        , body = body
        }
  in
  Decode.dict decodeTable


compileHdl : String -> Result String (List HdlEmitter.DefOutput)
compileHdl src =
  case HdlParser.parse src of
    Err err ->
      Err <| "❌ Parse error.\n\n"
      ++ HdlParser.showDeadEnds src  err
    Ok program ->
      case HdlChecker.check program of
        Ok _ ->
          Ok <| HdlEmitter.emit program
        Err problems ->
          Err <| "❌ Check error.\n"
          ++ HdlChecker.showProblems src  problems