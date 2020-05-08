module HdlEmitter exposing (emit, emitString, DefOutput, ParamOutput)

import HdlParser exposing (Def(..), Expr(..), Param, Size(..), BindingTarget(..), bindingTargetToString)
import HdlChecker exposing (getTargetNamesFromDef, getSourceNamesFromDef)
import AssocList as Dict exposing (Dict)
import Set exposing (Set)
import List.Extra
import EverySet


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
    
    topLevelDefsWithMutualRecursions =
      List.filter
        (\def ->
          case def of
            FuncDef { locals } ->
              checkMutualRecursions locals
            
            BindingDef _ -> -- binding is not allowed at top level
              False
        )
        defs
  in
  emitPrelude
  ++ List.map
    (\def ->
      case def of
        FuncDef { name, params, outputs } ->
          let
            hasMutualRecursions =
              List.member def topLevelDefsWithMutualRecursions
          in
          { name = name.value
          , params = paramsToParamsOutput params
          , outputs = paramsToParamsOutput outputs.value
          , body = emitDef 0 0 hasMutualRecursions defs def
          }
        BindingDef _ ->
          { name = "BINDING IS NOT ALLOWED AT TOP LEVEL"
          , params = []
          , outputs = []
          , body = ""
          }
    )
    defs


checkMutualRecursions : List Def -> Bool
checkMutualRecursions defs =
  List.any
  (\def ->
    let
      _ = Debug.log "AL -> targetNames" <| targetNames
      targetNames =
        getTargetNamesFromDef def

      _ = Debug.log "AL -> sourceNames" <| sourceNames      
      sourceNames =
        getSourceNamesFromDef def
    in
    List.any
      (\name ->
        let
          _ = Debug.log "AL -> name" <| name
          _ = Debug.log "AL -> checkMutualRecursionsHelper name sourceNames defs" <| checkMutualRecursionsHelper name sourceNames defs
        in
        checkMutualRecursionsHelper name sourceNames defs
      )
      targetNames
  )
  defs


checkMutualRecursionsHelper : String -> List String -> List Def -> Bool
checkMutualRecursionsHelper targetName sourceNames allDefs =
  let
    _ = Debug.log "AL -> sourceDefs" <| sourceDefs
    sourceDefs =
      List.filterMap identity <|
      List.map (getDef allDefs) sourceNames
  in
  List.any
    (\sourceDef ->
      let
        nextSourceNames =
          getSourceNamesFromDef sourceDef
      in
      List.member targetName nextSourceNames
      || checkMutualRecursionsHelper targetName nextSourceNames allDefs
    )
    sourceDefs


getDef : List Def -> String -> Maybe Def
getDef defs targetName =
  List.Extra.find
    (\def ->
      List.member targetName <| getTargetNamesFromDef def
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
    , body = "return function(a) { return (a === -1 ? -1 : -$b(a)); }"
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
emitDef : Int -> Int -> Bool -> List Def -> Def -> String
emitDef indent level hasMutualRecursions defsWithMutualRecursions def =
  let
    getDefNames : List Def -> List String
    getDefNames defs =
      List.map
        (\mutualRecursiveDef ->
          case mutualRecursiveDef of
            FuncDef r ->
              r.name.value
            
            BindingDef _ -> -- impossible
              "Binding def can not contain mutual recursions."
        )
        defs
  in
  case def of
    FuncDef { name, params, locals, body } ->
      let
        defNamesWithMutualRecursions =
          getDefNames defsWithMutualRecursions
        
        allUsedNamesWithMutualRecursions =
          getNamesWithMutualRecursionsFromDefs defNamesWithMutualRecursions [ def ]

        paramDeclarations =
          String.join ", " (List.map emitParam params)

        _ = Debug.log "AL -> name" <| name
        _ = Debug.log "AL -> localDeclarations" <| localDeclarations

        localDeclarations =
          List.map
            (\n ->
              let
                _ = Debug.log "AL -> n" <| n
              in
              "var " ++ processName defNamesWithMutualRecursions n ++ " = " ++ "_" ++ n.value ++ "();"
            )
            allUsedNamesWithMutualRecursions
          ++ ( if hasMutualRecursions then
            let
              names =
                List.concat <| List.Extra.unique <| List.map getTargetNamesFromDef locals
            in
            if List.length names > 0 then
              [ "var "
                ++ ( String.join ", " <|
                  names
                )
                ++ ";"
              ]
            else
              []
          else
            []
          )
        
        emittedBody =
          localDeclarations
          ++ ( if List.isEmpty locals then
            [ "return function(" ++ paramDeclarations ++ ") {"
            , "  return " ++ emitExpr defNamesWithMutualRecursions body.value ++ ";"
            , "}"
            ]
          else if hasMutualRecursions then
            [ "return function(" ++ paramDeclarations ++ ") {"
            , emitBlock 1 <|
              [ "for (var _ = 0; _ < 2; _++) {"
              , emitLocals 1 (level + 1) False defsWithMutualRecursions locals
              , "}"
              , "return " ++ emitExpr defNamesWithMutualRecursions body.value ++ ";"
              ]
            , "}"
            ]
          else
            [ "return function(" ++ paramDeclarations ++ ") {"
            , emitBlock 1 <|
              [ emitLocals 0 (level + 1) True defsWithMutualRecursions locals
              , "return " ++ emitExpr defNamesWithMutualRecursions body.value ++ ";"
              ]
            , "}"
            ]
          )
      in
      case level of
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
        defNamesWithMutualRecursions =
          getDefNames defsWithMutualRecursions
        
        emittedName =
          bindingTargetToString name.value
      in
      case locals of
        [] ->
          emitIndentation indent
          ++ (
            if hasMutualRecursions then
              "var "
            else
              "("
          )
          ++ emittedName ++ " = " ++ emitExpr defNamesWithMutualRecursions body.value
          ++ (
            if hasMutualRecursions then
              ""
            else
              ")"
          )
          ++ ";"
        locs ->
          emitBlock indent
            [ if hasMutualRecursions then "var " else "(" ++ emittedName ++ " ="
            , "function () {"
            , emitLocals (indent + 1) (level + 1) hasMutualRecursions defsWithMutualRecursions locs
            , "  return " ++ emitExpr defNamesWithMutualRecursions body.value ++ ";"
            , "}()" ++ if hasMutualRecursions then "" else ")" ++  ";"
            ]


getNamesWithMutualRecursionsFromDefs : List String -> List Def -> List (HdlParser.Located String)
getNamesWithMutualRecursionsFromDefs names defs =
  List.concat <| List.map
    (\d ->
      let
        (dLocals, dBody) =
          case d of
            FuncDef r ->
              (r.locals, r.body)

            BindingDef r ->
              (r.locals, r.body)
      in
      getNamesWithMutualRecursionsFromDefs names dLocals
      ++ getNamesWithMutualRecursionsFromExpr names dBody.value
    )
    defs

getNamesWithMutualRecursionsFromExpr : List String -> Expr -> List (HdlParser.Located String)
getNamesWithMutualRecursionsFromExpr names expr =
  case expr of
    Binding n ->
      if List.member n.value names then
        [ n ]
      else
        []
    
    Call callee args ->
      ( if List.member callee.value names then
        [ callee ]
      else
        []
      )
      ++ (List.concat <| List.map (getNamesWithMutualRecursionsFromExpr names << .value) args)

    Indexing e _ ->
      getNamesWithMutualRecursionsFromExpr names e.value

    Record r ->
      List.concat <| List.map (getNamesWithMutualRecursionsFromExpr names << .value) <| Dict.values r.value

    BusLiteral l ->
      List.concat <| List.map (getNamesWithMutualRecursionsFromExpr names << .value) <| l.value

    Concat e1 e2 ->
        List.concat
          [ getNamesWithMutualRecursionsFromExpr names e1.value
          , getNamesWithMutualRecursionsFromExpr names e2.value
          ]
    
    _ ->
      []


emitLocals : Int -> Int -> Bool -> List Def -> List Def -> String
emitLocals indent level hasMutualRecursions allDefsWithMutualRecursions defs =
  String.join "\n" <| List.map (emitDef indent level hasMutualRecursions allDefsWithMutualRecursions) <| sortDefs defs


type alias Dep =
  Dict (List String) (List String)


sortDefs : List Def -> List Def
sortDefs defs =
  let
    targetNameList =
      List.map getTargetNamesFromDef defs

    allTargetNames =
      List.concat targetNameList

    sourceNameList =
      List.map
        (\def ->
          List.filter
          (\name ->
            List.member name allTargetNames
          )
          (getSourceNamesFromDef def)
        )
        defs

    sortedDefNames =
      sortDependencies <|
        Dict.fromList <|
        List.map2 Tuple.pair targetNameList sourceNameList
  in
  List.reverse <| EverySet.toList <| EverySet.fromList <| List.filterMap identity <| List.map (getDef defs) sortedDefNames


sortDependencies : Dep -> List String
sortDependencies dep =
  let
    (result, _, _) =
      sortDependenciesHelper ([], Set.empty, dep)
  in
  List.reverse result

    
sortDependenciesHelper : (List String, Set String, Dep) -> (List String, Set String, Dep)
sortDependenciesHelper (result0, used0, dep0) =
  let
    (result1, used1, dep1) =
      Dict.foldl
      (\k v (result, used, dep) ->
        if List.all (\value -> Set.member value used) v then
          (k ++ result, List.foldl (\k1 set -> Set.insert k1 set) used k, Dict.filter (\k1 _ -> k /= k1) dep)
        else
          (result, used, dep)
      )
      (result0, used0, dep0)
      dep0
  in
  if Dict.isEmpty dep1 then
    (result1, used1, dep1)
  else if Dict.size dep0 == Dict.size dep1 then
    (List.concat (List.reverse <| Dict.keys dep1) ++ result1, used1, dep1)
  else
    sortDependenciesHelper (result1, used1, dep1)


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


emitExpr : List String -> Expr -> String
emitExpr defNamesWithMutualRecursions e =
  case e of
    Binding name ->
      processName defNamesWithMutualRecursions name
    Call callee args ->
      processName defNamesWithMutualRecursions callee
      ++ "(" ++ (String.join ", " <| List.map (emitExpr defNamesWithMutualRecursions << .value) args) ++ ")"
    Indexing expr (from, to) ->
      let
        shiftRightBinaryPlaces =
          String.fromInt from.value
        
        andFilter =
          "0b" ++ String.repeat (to.value - from.value + 1) "1"
      in
      "$b(" ++ emitExpr defNamesWithMutualRecursions expr.value ++ ")" ++ " >>> " ++ shiftRightBinaryPlaces ++ " & " ++ andFilter
    Record r ->
      Dict.foldl
        (\k v str ->
          str ++ k.value ++ " : " ++ emitExpr defNamesWithMutualRecursions v.value ++ ", "
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
          (emitExpr defNamesWithMutualRecursions << .value)
          l.value
      )
    Concat l r ->
      "(" ++ emitExpr defNamesWithMutualRecursions l.value ++ ").toString(2)" ++ " + " ++ "(" ++ emitExpr defNamesWithMutualRecursions r.value ++ ").toString(2)"


locationToString : (Int, Int) -> String
locationToString (row, col) =
  String.fromInt row ++ "_" ++ String.fromInt col


processName : List String -> HdlParser.Located String -> String
processName defNamesWithMutualRecursions name =
  if List.member name.value defNamesWithMutualRecursions then
    name.value ++ "$" ++ locationToString name.from
  else
    name.value


emitParam : Param -> String
emitParam p =
  p.name.value