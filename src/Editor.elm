port module Editor exposing (..)


import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Element as E
import Element.Input as Input
import Element.Font as Font
import Element.Border as Border
import Element.Background as Background
import Element.Events as Events
import Dropdown
import FeatherIcons
import Color
import HdlParser exposing (Size(..))
import HdlChecker
import HdlEmitter
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Dict exposing (Dict)
import Array.NonEmpty as NonEmptyArray exposing (NonEmptyArray)
import Binary


port setEditorValuePort : String -> Cmd msg
port editorValueChangedPort : (String -> msg) -> Sub msg
port generateTruthTablePort : Encode.Value -> Cmd msg
port receiveTruthTablePort : (String -> msg) -> Sub msg
port changeTabPort : UnitIndex -> Cmd msg
port addTabPort : UnitIndex -> Cmd msg
port removeTabPort : (UnitIndex, UnitIndex) -> Cmd msg
port storeModelPort : Encode.Value -> Cmd msg
port pageWillClosePort : (() -> msg) -> Sub msg


type alias Model =
  { units : NonEmptyArray Unit
  , editingActiveUnitName : Bool
  , numberFormatDropdownState : Dropdown.State NumberFormat
  , numberFormat : NumberFormat 
  }


type NumberFormat
  = BinaryFormat
  | DecimalFormat
  | HexadecimalFormat


numberFormatToString : NumberFormat -> String
numberFormatToString format =
  case format of
    BinaryFormat ->
      "Binary"
    
    DecimalFormat ->
      "Decimal"
    
    HexadecimalFormat ->
      "Hexadecimal"


numberFormatDropdownOptions =
  [ DecimalFormat, BinaryFormat, HexadecimalFormat ]


numberFormatDropdownConfig =
  let
    containerAttrs =
      [ E.width (E.px 120) ]

    promptElement =
      E.el
        [ E.width <| E.px 120
        , E.centerX
        ]
        (E.text <| "Decimal")

    itemToPrompt item =
      E.el
        [ E.width <| E.px 120
        , E.centerX
        ]
        (E.text <| numberFormatToString item)
       
    
    itemToElement : Bool -> Bool -> NumberFormat -> E.Element Msg
    itemToElement selected highlighted item =
      let
        bgColor =
          if highlighted then
            styles.lightGrey

          else if selected then
            styles.white

          else
            styles.lightGrey
      in
      E.el
        [ Background.color bgColor
        , E.padding 5
        , E.width <| E.px 120
        , E.centerX
        , E.pointer
        ]
        (E.text <| numberFormatToString item)

    selectAttrs =
      [ Border.width 1, Border.rounded 5, E.padding 5 ]
      
    listAttrs =
      [ Border.width 1
      , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
      , E.width E.fill
      ]

    openCloseButtons =
      { openButton = E.html (FeatherIcons.chevronDown |> FeatherIcons.toHtml [])
      , closeButton = E.html (FeatherIcons.chevronUp |> FeatherIcons.toHtml [])
      }
  in
  Dropdown.basic NumberFormatDropdownMsg SelectedNumberFormat itemToPrompt itemToElement
  |> Dropdown.withPromptElement promptElement
  |> Dropdown.withSelectAttributes selectAttrs
  |> Dropdown.withListAttributes listAttrs
  |> Dropdown.withContainerAttributes containerAttrs
  |> Dropdown.withOpenCloseButtons openCloseButtons


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
  | RemoveTab UnitIndex
  | StartEditingActiveUnitName
  | EditActiveUnitName String
  | StopEditingActiveUnitName
  | StoreModel ()
  | NumberFormatDropdownMsg (Dropdown.Msg NumberFormat)
  | SelectedNumberFormat (Maybe NumberFormat)


type alias TruthTable =
  Dict
    String
    { params : List TruthTableParam
    , outputs : List TruthTableParam
    , body : List (List Int)
    }


type alias TruthTableParam =
  { name : String, size : Int }


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


main : Program (Maybe Decode.Value) Model Msg
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
      changeTab desiredUnitIndex model

    AddTab ->
      (addUnit "Untitled Unit" model, addTabPort <| NonEmptyArray.length model.units)

    RemoveTab unitIndex ->
      let
        numberOfUnits =
          NonEmptyArray.length model.units
      in
      if numberOfUnits == 1 then
        ( model, Cmd.none)
      else
        let
          nextUnitIndex =
            if unitIndex == numberOfUnits - 1 then
              unitIndex - 1
            else
              unitIndex
          
          nextUnits =
            NonEmptyArray.removeAtSafe unitIndex model.units
          
          nextModel =
            setActiveUnit
              nextUnitIndex
              { model
                | units =
                  nextUnits
              }
        in
        ( nextModel
        , Cmd.batch
          [ removeTabPort (unitIndex, nextUnitIndex)
          , generateTruthTable ((getActiveUnit >> .output) nextModel)
          ]
        )


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

    StoreModel _ ->
      ( model
      , storeModelPort (encodeModel model)
      )

    NumberFormatDropdownMsg subMsg ->
      let
        ( state, cmd ) =
          Dropdown.update numberFormatDropdownConfig subMsg model.numberFormatDropdownState numberFormatDropdownOptions
      in
      ( { model | numberFormatDropdownState = state }, cmd )

    SelectedNumberFormat format ->
      case format of
        Nothing ->
          ( model, Cmd.none)
        
        Just f ->
          ({ model | numberFormat = f }, Cmd.none)


changeTab : UnitIndex -> Model -> (Model, Cmd Msg)
changeTab desiredUnitIndex model =
  let
    newModel =
      setActiveUnit desiredUnitIndex model
  in
  ( newModel
  , Cmd.batch
    [ changeTabPort desiredUnitIndex
    , generateTruthTable ((getActiveUnit >> .output) newModel)
    ]
  )


encodeModel : Model -> Encode.Value
encodeModel model =
  let
    encodeUnit : Unit -> Encode.Value
    encodeUnit unit =
      Encode.object
        [ ( "name", Encode.string unit.name )
        , ( "source", Encode.string unit.source )
        ]
  in
  NonEmptyArray.encode encodeUnit model.units


decodeModel : Decoder Model
decodeModel =
  let
    decodeUnit : Decoder Unit
    decodeUnit =
      Field.require "name" Decode.string <| \name ->
      Field.require "source" Decode.string <| \source ->

      Decode.succeed
        { name = name
        , source = source
        , output = compileHdl source
        , truthTable = Dict.empty
        }
    
    decodeUnits : Decoder (NonEmptyArray Unit)
    decodeUnits =
      NonEmptyArray.decoder decodeUnit
  in
  Decode.map
    (\units ->
      { units = units
      , editingActiveUnitName = False
      , numberFormatDropdownState = Dropdown.init "number-format-dropdown"
      , numberFormat = DecimalFormat
      }
    )
    decodeUnits


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
    , pageWillClosePort StoreModel
    ]


-- the width of the right panel in "vw"
rightPanelWidth : Int
rightPanelWidth =
  48


view : Model -> Html Msg
view model =
  E.layout
    [ E.htmlAttribute <| Html.Attributes.style "margin" "0 10px"
    , E.htmlAttribute <| Html.Attributes.style "width" <| String.fromInt rightPanelWidth ++ "vw"
    , Font.size 16
    , E.scrollbarY
    ] <|
    E.column
      []
      [ viewRightPanel model
      , viewTabSelector model
      ]


viewTabSelector : Model -> E.Element Msg
viewTabSelector model =
  let
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
              , E.paddingEach
                { left = 10
                , right = 20
                , bottom = 10
                , top = 10
                }
              , if index == activeUnitIndex then
                Background.color styles.white
                else
                Background.color styles.lightGrey
              , E.inFront <| viewCloseTab index
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


viewCloseTab : UnitIndex -> E.Element Msg
viewCloseTab unitIndex =
  Input.button
    [ E.paddingXY 3 3
    , E.centerY, E.alignRight
    , Border.rounded 10
    , E.htmlAttribute <| onClickNoProp <| RemoveTab unitIndex
    ]
    { onPress =
      Nothing
    , label =
      E.el [ E.centerX ] <|
        E.html (FeatherIcons.x |> FeatherIcons.withSize 12 |> FeatherIcons.toHtml [])
    }


onClickNoProp : Msg -> Html.Attribute Msg
onClickNoProp msg =
  Html.Events.custom "click"
    (Decode.succeed
    { message = msg
    , stopPropagation = True
    , preventDefault = False
    }
  )


viewAddTab : E.Element Msg
viewAddTab =
  Input.button [ Border.width 1, E.padding 10 ]
    { onPress =
      Just <| AddTab
    , label =
      E.html (FeatherIcons.plus |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml [])
    }


viewRightPanel : Model -> E.Element Msg
viewRightPanel model =
  let
    activeUnit =
      getActiveUnit model
  in
  case activeUnit.output of
    Ok _ -> viewTruthTable model activeUnit.truthTable
    Err str -> E.html <| Html.pre [ Html.Attributes.style "white-space" "pre-wrap" ] [ Html.text str ]


getActiveUnit : Model -> Unit
getActiveUnit model =
  NonEmptyArray.getSelected model.units


viewTruthTable : Model -> TruthTable -> E.Element Msg
viewTruthTable model table =
  E.column [ E.spacing 10 ] <|
  [ E.html <|
    Html.span
    [ Html.Attributes.style "position" "fixed"
    , Html.Attributes.style "top" "20px"
    , Html.Attributes.style "right" "200px"
    , Html.Attributes.style "margin-bottom" "10px"
    ]
    [ Html.text "Format:" ]
  , E.el
    [ E.htmlAttribute <| Html.Attributes.style "position" "fixed"
    , E.htmlAttribute <| Html.Attributes.style "top" "10px"
    , E.htmlAttribute <| Html.Attributes.style "right" "70px"
    , E.htmlAttribute <| Html.Attributes.style "margin-bottom" "10px"
    , E.htmlAttribute <| Html.Attributes.style "z-index" "999"
    ]
    <| Dropdown.view numberFormatDropdownConfig model.numberFormatDropdownState numberFormatDropdownOptions
  , E.html <|
    Html.div [] <|
    Dict.foldl
      (\defName defTable viewList ->
        let
          headerNames : List String
          headerNames =
            List.map .name <| defTable.params ++ defTable.outputs
          
          headerSizes : List Int
          headerSizes =
            List.map .size <| defTable.params ++ defTable.outputs
        in
        ( Html.table
          [ Html.Attributes.style "overflow-y" "scroll"
          , Html.Attributes.style "border" "1px grey solid"
          , Html.Attributes.style "border-collapse" "collapse"
          ] <|
          Html.caption
            [ Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "top" "25px"
            , Html.Attributes.style "left" <| "calc(" ++ String.fromInt rightPanelWidth ++ "vw + 45px)"
            , Html.Attributes.style "font-weight" "bold"
            , Html.Attributes.style "margin-bottom" "10px"
            ] [ Html.text defName ]
          :: Html.thead []
            (List.indexedMap
              (\index name ->
                Html.th
                [ Html.Attributes.style "position" "sticky"
                , Html.Attributes.style "top" "0px"
                , Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "min-width" <|
                  (String.fromFloat <| (toFloat rightPanelWidth - (toFloat <| List.length headerNames) * 1.55) / (toFloat <| List.length headerNames)) ++ "vw"
                , Html.Attributes.style "border" "1px grey solid"
                , Html.Attributes.style "background-color" <|
                  if index >= List.length defTable.params then
                    "#ffd8a7"
                  else
                    "lightgreen"
                ]
                [ Html.text name ]
              )
              headerNames
            )
          :: List.map (\row ->
            Html.tr
            [ Html.Attributes.style "text-align" "center"
            ] <|
            List.map
              (\(size, value) ->
                let
                  valueString =
                    case model.numberFormat of
                      DecimalFormat ->
                        String.fromInt value
                      
                      BinaryFormat ->
                        let
                          complementedValue =
                            if value < 0 then
                              2 ^ size + value
                            else
                              value
                        in
                        String.join "" <| List.map String.fromInt <|
                        Binary.toIntegers <| Binary.ensureSize size <|
                        Binary.fromDecimal complementedValue

                      HexadecimalFormat ->
                        let
                          complementedValue =
                            if value < 0 then
                              2 ^ size + value
                            else
                              value
                        in
                        Binary.toHex <|
                        Binary.ensureSize size <|
                        Binary.fromDecimal complementedValue
                in
                Html.td
                [ Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "min-width" <|
                  (String.fromFloat <| (toFloat rightPanelWidth - (toFloat <| List.length row) * 1.55) / (toFloat <| List.length row)) ++ "vw"
                , Html.Attributes.style "border" "1px grey solid"
                ]
                [ Html.text valueString
                ]
              )
              (List.map2 Tuple.pair headerSizes row)
          )
          defTable.body
        )
        :: viewList
      )
      []
      table
  ]


init : (Maybe Decode.Value) -> ( Model, Cmd Msg )
init storedModel =
  let
    model =
      case storedModel of
        Nothing ->
          defaultModel

        Just modelJson ->
          Result.withDefault defaultModel <|
            Decode.decodeValue decodeModel modelJson

    activeUnit =
      getActiveUnit model
    
    defaultModel =
      { units =
        NonEmptyArray.fromElement
        { name = "Basics"
        , source = defaultSource
        , output = defaultOutput
        , truthTable = Dict.empty
        }
      , editingActiveUnitName =
        False
      , numberFormatDropdownState =
        Dropdown.init "number-format-dropdown"
      , numberFormat =
        DecimalFormat
      }
    
    defaultSource =
      """xor a[n] b[n] -> [n] =
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
    defaultOutput = compileHdl defaultSource
  in
  ( model
  , Cmd.batch
    [ setEditorValuePort activeUnit.source
    , generateTruthTable activeUnit.output
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
          encodeDefOutput : HdlEmitter.DefOutput -> Encode.Value
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
          encodeParamOutput : HdlEmitter.ParamOutput -> Encode.Value
          encodeParamOutput param =
            Encode.object
              [ ( "name", Encode.string param.name )
              , ( "size", encodeSize param.size )
              ]

          -- type Size
          --   = IntSize Int
          --   | VarSize (Located String)
          encodeSize : Size -> Encode.Value
          encodeSize size =
            case size of
              IntSize i ->
                Encode.int i
              VarSize n ->
                Encode.string n.value
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
    -- { name : String
    -- , size : Int
    -- }
    decodeTruthTableParam : Decoder TruthTableParam
    decodeTruthTableParam =
      Field.require "name" Decode.string <| \name->
      Field.require "size" Decode.int <| \size ->

      Decode.succeed
        { name = name
        , size = size
        }

    decodeTable =
      Field.require "params" (Decode.list decodeTruthTableParam) <| \params ->
      Field.require "outputs" (Decode.list decodeTruthTableParam) <| \outputs ->
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
      ++ HdlParser.showDeadEnds src err
    Ok program ->
      case HdlChecker.check program of
        Ok _ ->
          Ok <| HdlEmitter.emit program
        Err problems ->
          Err <| "❌ Check error.\n"
          ++ HdlChecker.showProblems src  problems