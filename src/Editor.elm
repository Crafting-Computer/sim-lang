port module Editor exposing (..)


import Browser
import Html exposing (Html)
import Html.Attributes
import Element as E
import Element.Input as Input
import Element.Font as Font
import Element.Border as Border
import Element.Background as Background
import Element.Events as Events
import Color
import HdlParser exposing (Size(..))
import HdlChecker
import HdlEmitter
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Dict exposing (Dict)
import Binary
import Array.NonEmpty as NonEmptyArray exposing (NonEmptyArray)


port setEditorValuePort : String -> Cmd msg
port editorValueChangedPort : (String -> msg) -> Sub msg
port generateTruthTablePort : Encode.Value -> Cmd msg
port receiveTruthTablePort : (String -> msg) -> Sub msg
port changeTabPort : UnitIndex -> Cmd msg
port addTabPort : UnitIndex -> Cmd msg


type alias Model =
  { units : NonEmptyArray Unit
  , editingActiveUnitName : Bool
  }


type alias Unit =
  { name : String
  , source : String
  , output : Result String (List HdlEmitter.DefOutput)
  , truthTable : TruthTable
  }


type alias UnitIndex =
  Int


type Msg
  = EditorValueChanged String
  | TruthTableReceived String
  | ChangeTab UnitIndex
  | AddTab
  | StartEditingActiveUnitName
  | EditActiveUnitName String
  | StopEditingActiveUnitName


type alias TruthTable =
  Dict
    String
    { params : List String
    , outputs : List String
    , body : List (List Int)
    }


preludeLength : Int
preludeLength =
  1


styles =
  { white = toElmUiColor Color.white
  , lightGrey = toElmUiColor Color.lightGrey
  }


toElmUiColor : Color.Color -> E.Color
toElmUiColor color =
    let
        {red, green, blue, alpha } =
            Color.toRgba color
    in
    E.rgba red green blue alpha


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
        output =
          compileHdl newValue
      in
      (updateActiveUnit
        (\activeUnit ->
          { activeUnit
          | source =
            newValue
          , output =
            output
          }
        )
        model
      , generateTruthTable output
      )
    TruthTableReceived tableJson ->
      (updateActiveUnit
        (\activeUnit ->
          { activeUnit
            | truthTable =
              Result.withDefault activeUnit.truthTable <| Decode.decodeString decodeTruthTable tableJson
          }
        )
        model
      , Cmd.none
      )

    ChangeTab desiredUnitIndex ->
      (setActiveUnit desiredUnitIndex model, changeTabPort desiredUnitIndex)

    AddTab ->
      (addUnit "Untitled Unit" model, addTabPort <| NonEmptyArray.length model.units)

    StartEditingActiveUnitName ->
      ({ model |
        editingActiveUnitName =
          True
      }
      , Cmd.none
      )

    EditActiveUnitName newName ->
      (editActiveUnitName newName model, Cmd.none)

    StopEditingActiveUnitName ->
      ({ model |
        editingActiveUnitName =
          False
      }
      , Cmd.none
      )


editActiveUnitName : String -> Model -> Model
editActiveUnitName newName model =
  updateActiveUnit
    (\activeUnit ->
      { activeUnit |
        name =
          newName
      }
    )
    model


setActiveUnit : UnitIndex -> Model -> Model
setActiveUnit desiredUnitIndex model =
  { model |
    units =
      NonEmptyArray.setSelectedIndex desiredUnitIndex <|
        model.units
  }


addUnit : String -> Model -> Model
addUnit name model =
  let
    newUnit =
      { name = name
      , source = ""
      , output = Ok []
      , truthTable = Dict.empty
      }
  in
  { model |
    units =
      NonEmptyArray.setSelectedIndex (NonEmptyArray.length model.units) <|
        NonEmptyArray.push newUnit model.units
  }


updateActiveUnit : (Unit -> Unit) -> Model -> Model
updateActiveUnit updateUnit model =
  { model |
    units =
      NonEmptyArray.updateSelected updateUnit model.units
  }


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
      , viewTabSelector model
      ]


viewTabSelector : Model -> E.Element Msg
viewTabSelector model =
  let
    _ = Debug.log "AL -> activeUnit" <| activeUnit.name
    activeUnit =
      NonEmptyArray.getSelected model.units
    activeUnitIndex =
      NonEmptyArray.selectedIndex model.units
  in
  E.row
    [ E.htmlAttribute <| Html.Attributes.style "position" "fixed"
    , E.htmlAttribute <| Html.Attributes.style "top" "15px"
    , E.htmlAttribute <| Html.Attributes.style "left" "30px"
    ] <|
    NonEmptyArray.toList
      (NonEmptyArray.indexedMap
        (\index unit ->
          if model.editingActiveUnitName && index == activeUnitIndex then
            Input.text
              [ Events.onLoseFocus StopEditingActiveUnitName
              ]
              { onChange = EditActiveUnitName
              , text = unit.name
              , placeholder = Nothing
              , label = Input.labelHidden "edit active unit name"
              }
          else
            Input.button
              [ Border.width 1
              , E.padding 10
              , if index == activeUnitIndex then
                Background.color styles.white
                else
                Background.color styles.lightGrey
              ]
              { onPress =
                if index == activeUnitIndex then
                  Just StartEditingActiveUnitName
                else
                  Just <| ChangeTab index
              , label =
                E.text unit.name
              }
        )
        model.units
      )
    ++ [ viewAddTab ]


viewAddTab : E.Element Msg
viewAddTab =
  Input.button [ Border.width 1, E.padding 10 ]
    { onPress =
      Just <| AddTab
    , label =
      E.text "+"
    }


viewRightPanel : Model -> E.Element Msg
viewRightPanel model =
  let
    activeUnit =
      getActiveUnit model
  in
  case activeUnit.output of
    Ok _ -> viewTruthTable activeUnit.truthTable
    Err str -> E.html <| Html.pre [ Html.Attributes.style "white-space" "pre-wrap" ] [ Html.text str ]


getActiveUnit : Model -> Unit
getActiveUnit model =
  NonEmptyArray.getSelected model.units


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
    source =
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
    output = compileHdl source
  in
  ({ units =
    NonEmptyArray.fromElement
    { name = "Basics"
    , source = source
    , output = output
    , truthTable = Dict.empty
    }
  , editingActiveUnitName =
    False
  }
  , Cmd.batch
    [ setEditorValuePort source
    , generateTruthTable output
    ]
  )

generateTruthTable : Result String (List HdlEmitter.DefOutput) -> Cmd Msg
generateTruthTable output =
  case output of
    Err _ ->
      Cmd.none
    Ok defs ->
      if List.length defs > preludeLength then
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
              VarSize n ->
                Encode.string n
        in
        generateTruthTablePort <| Encode.list encodeDefOutput defs
      else
        Cmd.none

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