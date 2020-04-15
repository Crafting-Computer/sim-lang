module HdlChecker exposing (..)

import HdlParser exposing (fakeLocated, Located, Def(..), Expr(..), BindingTarget(..), Size(..))
import AssocList as Dict
import List.Extra

type Problem
  = DuplicatedName (Located String) (Located String)
  

prelude : List Def
prelude =
  [ preludeFuncDef
    "nand"
    [ { name = "a", size = VarSize "n" }
    , { name = "b", size = VarSize "n" }
    ]
    [ { name = "out", size = VarSize "n" }
    ]
  ]


preludeFuncDef : String -> List { name : String, size: Size} -> List { name : String, size: Size} -> Def
preludeFuncDef name params retType =
  FuncDef
    { name =
      fakeLocated name
    , params = List.map (\p ->
      { name = fakeLocated p.name
      , size = fakeLocated p.size
      }) params
    , retType = List.map (\p ->
      { name = fakeLocated p.name
      , size = fakeLocated p.size
      }) retType
    , locals = []
    , body = Binding <| fakeLocated ""
    }


check : List Def -> Result (List Problem) ()
check defs =
  let
    errors =
      List.foldl
        (\def errs ->
          let
            beforeDefs =
              List.Extra.takeWhile ((/=) def) defs
            afterDefs =
              List.drop (List.length beforeDefs + 1) defs
          in
          case checkDef (prelude ++ beforeDefs) afterDefs def of
            Err error ->
              error ++ errs
            Ok _ ->
              errs
        )
        []
        defs
  in
  case errors of
    [] ->
      Ok ()
    errs ->
      Err errs


checkDef : List Def -> List Def -> Def -> Result (List Problem) ()
checkDef beforeDefs afterDefs def =
  let
    defNames =
      getDefNames def
    
    duplicatedNames =
      List.foldl
        (\other duplicates1 ->
          List.foldl
            (\defName duplicates2 ->
              case List.Extra.find
                (\otherName -> otherName.value == defName.value)
                (getDefNames other)
              of
                Just otherName ->
                  DuplicatedName otherName defName :: duplicates2
                Nothing ->
                  duplicates2
            )
            []
            defNames
          ++ duplicates1
        )
        []
        beforeDefs

    errors =
      duplicatedNames
  in
  case errors of
    [] ->
      Ok ()
    errs ->
      Err errs


getDefNames : Def -> List (Located String)
getDefNames def =
  case def of
    FuncDef { name } ->
      [ name ]
    BindingDef { name } ->
      case name of
        BindingName n ->
          [ n ]
        BindingRecord r ->
          Dict.values r
