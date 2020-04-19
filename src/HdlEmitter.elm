module HdlEmitter exposing (emit)

import HdlParser exposing (Def(..), Expr(..), Param, BindingTarget(..))
import AssocList as Dict

emit : List Def -> String
emit defs =
  emitPrelude ++ "\n"
  ++ String.join
    "\n"
    (List.map (emitDef 0) defs)


emitPrelude : String
emitPrelude =
  emitBlock 0
  [ "function nand(a, b) { return ~(a & b); }"
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
      emitBlock indent
        [ "function " ++ name.value ++ "(" ++ String.join ", " (List.map emitParam params) ++ ") {"
        , String.join "\n" <| List.map (emitDef <| indent + 1) locals
        , "  return " ++ emitExpr body ++ ";"
        , "}"
        ]
    BindingDef { name, locals, body } ->
      let
        emittedName =
          case name of
            BindingName n ->
              n.value
            BindingRecord r ->
              Dict.foldl
                (\k v str ->
                  str ++ k.value ++ " : " ++ v.value ++ ", "
                )
                "{ "
                r
              ++ " }"
      in
      case locals of
        [] ->
          emitIndentation indent ++ "var " ++ emittedName ++ " = " ++ emitExpr body ++ ";"
        locs ->
          emitBlock indent
            [ "var " ++ emittedName ++ " ="
            , "function () {"
            , String.join "\n" <| List.map (emitDef <| indent + 1) locs
            , "  return " ++ emitExpr body ++ ";"
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
      callee.value ++ "(" ++ (String.join ", " <| List.map emitExpr args) ++ ")"
    Indexing expr (from, to) ->
      "(" ++ "(" ++ emitExpr expr ++ ")" ++ " << " ++ String.fromInt from.value ++ " >>> " ++ String.fromInt to.value ++ ")"
    Record r ->
      Dict.foldl
        (\k v str ->
          str ++ k.value ++ " : " ++ emitExpr v ++ ", "
        )
        "{ "
        r.value
      ++ " }"
    IntLiteral i ->
      String.fromInt i.value


emitParam : Param -> String
emitParam p =
  p.name.value