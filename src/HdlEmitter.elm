module HdlEmitter exposing (emit, emitString, DefOutput)

import HdlParser exposing (Def(..), Expr(..), Param, Size(..), BindingTarget(..), bindingTargetToString)
import HdlChecker exposing (getTargetNamesFromDef, getSourceNamesFromDef)
import AssocList as Dict
import List.Extra


type alias DefOutput =
  { name : String
  , params : List ParamOutput
  , outputs : List ParamOutput
  , body : String
  }


type alias ParamOutput =
  { name : String
  , size : Size
  }


emitString : List Def -> String
emitString defs =
  let
    defOutputs =
      emit defs
  in
  emitBlock 0 <|
  List.map
    (\def ->
      emitBlock 0
      [ "function _" ++ def.name ++ "() {"
      , emitBlock 1 [ def.body ]
      , "}"
      , "var " ++ def.name ++ " = _" ++ def.name ++ "();"
      ]
      ++ "\n"
    )
    defOutputs


emit : List Def -> List DefOutput
emit defs =
  let
    paramsToParamsOutput params =
      List.map (\p -> { name = p.name.value, size = p.size.value }) params
  in
  emitPrelude
  ++ List.map
    (\def ->
      case def of
        FuncDef { name, params, outputs } ->
          { name = name.value
          , params = paramsToParamsOutput params
          , outputs = paramsToParamsOutput outputs.value
          , body = emitDef 0 True def
          }
        BindingDef _ ->
          { name = "BINDING IS NOT ALLOWED AT TOP LEVEL"
          , params = []
          , outputs = []
          , body = ""
          }
    )
    defs


emitPrelude : List DefOutput
emitPrelude =
  let
    isize name size =
      { name = name, size = IntSize size }

    nsize name =
      { name = name, size = VarSize <| HdlParser.fakeLocated "n" }
    
    helper : String -> String -> DefOutput
    helper name body =
      { name = name
      , params = []
      , outputs = []
      , body = body
      }
  in
  -- function nand(a, b) { return ~(a & b); }
  [ helper "$b"
  """return function(value) {
  if (typeof value === "string") {
  let highestBit = value[0];
  return ~~parseInt(value.padStart(32, highestBit), 2)
} else {
  return value;
}}"""
  , { name = "nand"
    , params = [ nsize "a", nsize "b"]
    , outputs = [ nsize "" ]
    , body = "return function(a, b) { return ~($b(a) & $b(b)); }"
    }
  , { name = "fill"
    , params = [ isize "a" 1 ]
    , outputs = [ nsize "" ]
    , body = "return function(a) { return -$b(a); }"
    }
  ]


-- and a b -> out =
--   let
--     nand_a_b = nand a b
--   in
--   nand nand_a_b nand_a_b

-- function and(a, b) {
--   var nand_a_b = nand(a, b);
--   return nand(nand_a_b, nand_a_b);
-- }
emitDef : Int -> Bool -> Def -> String
emitDef indent declareVars def =
  case def of
    FuncDef { name, params, locals, body } ->
      let
        localTargets =
          List.concat <| List.Extra.unique <| List.map getTargetNamesFromDef locals
        
        localDeclarations =
          "var " ++ String.join ", " localTargets ++ ";"
        
        paramDeclarations =
          String.join ", " (List.map emitParam params)
        
        emittedBody =
          case locals of
            [] ->
              [ "return function(" ++ paramDeclarations ++ ") {"
              , "  return " ++ emitExpr body.value ++ ";"
              , "}"
              ]

            _ ->
              [ localDeclarations
              , "return function(" ++ paramDeclarations ++ ") {"
              , emitBlock 1 <|
                [ "for (var _ = 0; _ < 2; _++) {"
                , emitLocals 1 False locals
                , "}"
                , "return " ++ emitExpr body.value ++ ";"
                ]
              , "}"
              ]
      in
      case indent of
        0 ->
          emitBlock indent emittedBody
        
        _ ->
          emitBlock indent
            [ "function " ++ "_" ++ name.value ++ "() {"
            , emitBlock 1 emittedBody
            , "}"
            , "var " ++ name.value ++ " = _" ++ name.value ++ "();"
            ]
    
    BindingDef { name, locals, body } ->
      let
        emittedName =
          bindingTargetToString name.value
      in
      case locals of
        [] ->
          emitIndentation indent
          ++ (
            if declareVars then
              "var "
            else
              "("
          )
          ++ emittedName ++ " = " ++ emitExpr body.value
          ++ (
            if declareVars then
              ""
            else
              ")"
          )
          ++ ";"
        locs ->
          emitBlock indent
            [ if declareVars then "var " else "(" ++ emittedName ++ " ="
            , "function () {"
            , emitLocals (indent + 1) declareVars locs
            , "  return " ++ emitExpr body.value ++ ";"
            , "}()" ++ if declareVars then "" else ")" ++  ";"
            ]


emitLocals : Int -> Bool -> List Def -> String
emitLocals indent declareVars defs =
  let
    orderedLocals =
      List.sortWith
        (\d1 d2 ->
          let
            t1 =
              getTargetNamesFromDef d1
            
            n1 =
              getSourceNamesFromDef d1
            
            t2 =
              getTargetNamesFromDef d2
            
            n2 =
              getSourceNamesFromDef d2
            
            firstDependsOnSecond =
              List.any (\n -> List.member n t2) n1
            
            secondDependsOnFirst =
              List.any (\n -> List.member n t1) n2
          in
          if firstDependsOnSecond then
            if secondDependsOnFirst then
              EQ
            else
              GT
          else
            LT
        )
        defs
  in
  String.join "\n" <| List.map (emitDef indent declareVars) orderedLocals


emitIndentation : Int -> String
emitIndentation indent =
  String.repeat (indent * 2) " "


emitBlock : Int -> List String -> String
emitBlock indent lines =
  let
    indentation =
      emitIndentation indent
  in
  indentation
  ++ (
    String.join ("\n" ++ indentation) <|
    List.map (String.replace "\n" ("\n" ++ indentation)) <|
    List.filter (\line -> String.trim line /= "") lines
  )


emitExpr : Expr -> String
emitExpr e =
  case e of
    Binding name ->
      name.value
    Call callee args ->
      callee.value ++ "(" ++ (String.join ", " <| List.map (emitExpr << .value) args) ++ ")"
    Indexing expr (from, to) ->
      let
        shiftRightBinaryPlaces =
          String.fromInt from.value
        
        andFilter =
          "0b" ++ String.repeat (to.value - from.value + 1) "1"
      in
      "$b(" ++ emitExpr expr.value ++ ")" ++ " >>> " ++ shiftRightBinaryPlaces ++ " & " ++ andFilter
    Record r ->
      Dict.foldl
        (\k v str ->
          str ++ k.value ++ " : " ++ emitExpr v.value ++ ", "
        )
        "{ "
        r.value
      ++ " }"
    IntLiteral i ->
      String.fromInt i.value
    BusLiteral l ->
      "\"\" + " ++
      (String.join " + " <|
        List.map
          (emitExpr << .value)
          l.value
      )
    Concat l r ->
      "(" ++ emitExpr l.value ++ ").toString(2)" ++ " + " ++ "(" ++ emitExpr r.value ++ ").toString(2)"


emitParam : Param -> String
emitParam p =
  p.name.value