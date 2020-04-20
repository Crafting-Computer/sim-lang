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
    { header : List String
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
      ({ model
        | hdlSource =
          newValue
        , hdlOutput =
          compileHdl newValue
      }
      , Cmd.none
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
    [ E.htmlAttribute <| Html.Attributes.style "margin" "0 20px"
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
    Err str -> E.html <| Html.pre [] [ Html.text str ]


viewTruthTable : TruthTable -> E.Element Msg
viewTruthTable table =
  E.column [ E.spacing 10 ] <|
  Dict.foldl
    (\defName defTable viewList ->
      E.row [] [ E.text defName ]
      :: E.row [ E.spacing 10 ] (List.map (\name -> E.text name) defTable.header)
      :: List.map (\row ->
        E.row [ E.spacing 10 ] <|
        List.map (\value -> E.text <| String.fromInt <| Binary.toDecimal <| Binary.fromDecimal value) row
      )
      defTable.body
      ++ viewList
    )
    []
    table


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
    decodeTable : Decoder { header : List String, body : List (List Int) }
    decodeTable =
      Field.require "header" (Decode.list Decode.string) <| \header ->
      Field.require "body" (Decode.list <| Decode.list Decode.int) <| \body ->

      Decode.succeed
        { header = header
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