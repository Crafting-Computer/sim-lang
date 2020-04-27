module HdlEmitter exposing (emit, emitString, DefOutput)

import HdlParser exposing (Def(..), Expr(..), Param, Size(..), BindingTarget(..), bindingTargetToString)
import AssocList as Dict


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
      [ "function " ++ def.name ++ "(" ++ (String.join ", " <| List.map .name def.params) ++ ") {"
      , "  " ++ def.body
      , "}"
      ]
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
          , body = emitDef 0 def
          }
        BindingDef _ ->
          { name = "BINDING IS NOT ALLOWED AT TOP LEVEL" -- TODO: ban binding at top level
          , params = []
          , outputs = []
          , body = ""
          }
    )
    defs


emitPrelude : List DefOutput
emitPrelude =
  let
    nsize name =
      { name = name, size = VarSize "n" }
  in
  -- function nand(a, b) { return ~(a & b); }
  [ { name = "nand"
  , params = [ nsize "a", nsize "b"]
  , outputs = [ nsize "" ]
  , body = "return ~(a & b);"
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
emitDef : Int -> Def -> String
emitDef indent def =
  case def of
    FuncDef { name, params, locals, body } ->
      let
        emittedBody =
          [ String.join "\n" <| List.map (emitDef <| indent + 1) locals
          , "  return " ++ emitExpr body.value ++ ";"
          ]
      in
      emitBlock indent
        ( case indent of
          0 ->
            emittedBody
          _ ->
            ("function " ++ name.value ++ "(" ++ String.join ", " (List.map emitParam params) ++ ") {")
            :: emittedBody
            ++ [ "}" ]
        )
    BindingDef { name, locals, body } ->
      let
        emittedName =
          bindingTargetToString name.value
      in
      case locals of
        [] ->
          emitIndentation indent ++ "var " ++ emittedName ++ " = " ++ emitExpr body.value ++ ";"
        locs ->
          emitBlock indent
            [ "var " ++ emittedName ++ " ="
            , "function () {"
            , String.join "\n" <| List.map (emitDef <| indent + 1) locs
            , "  return " ++ emitExpr body.value ++ ";"
            , "}();"
            ]


emitIndentation : Int -> String
emitIndentation indent =
  String.repeat (indent * 2) " "


emitBlock : Int -> List String -> String
emitBlock indent lines =
  let
    indentation =
      emitIndentation indent
  in
  indentation ++ String.join ("\n" ++ indentation) (List.filter (\line -> String.trim line /= "") lines) ++ "\n"


emitExpr : Expr -> String
emitExpr e =
  case e of
    Binding name ->
      name.value
    Call callee args ->
      callee.value ++ "(" ++ (String.join ", " <| List.map (emitExpr << .value) args) ++ ")"
    Indexing expr (from, to) ->
      "(" ++ "(" ++ emitExpr expr.value ++ ")" ++ " << " ++ String.fromInt from.value ++ " >>> " ++ String.fromInt to.value ++ ")"
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


emitParam : Param -> String
emitParam p =
  p.name.value