port module Editor exposing (..)


import Browser
import Html exposing (Html)
import Html.Attributes
import Element as E
import Element.Input as Input
import Element.Font as Font
import HdlParser
import HdlChecker
import HdlEmitter


port setEditorValue : String -> Cmd msg
port editorValueChanged : (String -> msg) -> Sub msg


type alias Model =
  { hdlSource : String
  , hdlOutput : Result String String
  }


type Msg
  = EditorValueChanged String


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


subscriptions : Model -> Sub Msg
subscriptions model =
  editorValueChanged EditorValueChanged


view : Model -> Html Msg
view model =
  E.layout
    [ E.htmlAttribute <| Html.Attributes.style "margin" "0 20px"
    , E.htmlAttribute <| Html.Attributes.style "width" "45vw"
    , Font.size 14
    ] <|
    E.column
      []
      [ viewOutput model.hdlOutput
      ]

viewOutput : Result String String -> E.Element Msg
viewOutput output =
  E.html <|
    Html.pre []
    [ Html.text <|
      case output of
        Ok str -> str
        Err str -> str
    ]

init : () -> ( Model, Cmd msg )
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

-- test out full adder
add_1_1_0 = full_adder 1 1 0
  """
    hdlOutput = compileHdl hdlSource
  in
  ({ hdlSource = hdlSource
  , hdlOutput = hdlOutput
  }
  , setEditorValue hdlSource
  )

compileHdl : String -> Result String String
compileHdl src =
  case HdlParser.parse src of
    Err err ->
      Err <| "❌ Parse error.\n\n"
      ++ HdlParser.showDeadEnds src  err
    Ok program ->
      case HdlChecker.check program of
        Ok _ ->
          Ok <| "🏭 Generated JS code:\n\n"
          ++ HdlEmitter.emit program
        Err problems ->
          Err <| "❌ Check error.\n"
          ++ HdlChecker.showProblems src  problems