module HdlChecker exposing (Problem(..), Type(..), check)

import HdlParser exposing (fakeLocated, Located, Param, Def(..), Expr(..), BindingTarget(..), Size(..))
import AssocList as Dict exposing (Dict)
import List.Extra
import Set
import EverySet

type Problem
  = DuplicatedName (Located String) (Located String)
  | UndefinedName (Located String)
  | CalleeNotFunction (Located String)
  | WrongCallArity (Located String) (List Param) (List (Located Type)) -- callee params argTypes
  | TryIndexingRecordType (Dict String Type) (Located Int, Located Int) -- recordType indices
  | IndexOutOfBounds Int (Located Int) (Located Int) -- expectedBusSize from to
  | FromIndexBiggerThanToIndex (Located Int) (Located Int) -- from to
  | ExpectingBindingGotFunction (Located String) (Located String) -- bindingName functioName
  | ExpectingFunctionGotBinding (Located String) BindingTarget -- functionName bindingName
  | MismatchedTypes (Located Type) (Located Type)
  | ConflictingVarSizeArgs Param (Located Type) (Located Type) -- param previousArgType currentArgType

type Type
  = BusType Size
  | RecordType (Dict String Type)
  | ErrorType (List Problem)


type alias Location =
  { from : (Int, Int)
  , to : (Int, Int)    
  }


prelude : List Def
prelude =
  [ preludeFuncDef
    "nand"
    [ { name = "a", size = VarSize "n" Nothing }
    , { name = "b", size = VarSize "n" Nothing }
    ]
    [ { name = "out", size = VarSize "n" Nothing }
    ]
  ]


preludeFuncDef : String -> List { name : String, size: Size} -> List { name : String, size: Size} -> Def
preludeFuncDef name params outputs =
  FuncDef
    { name =
      fakeLocated name
    , params = List.map (\p ->
      { name = fakeLocated p.name
      , size = fakeLocated p.size
      }) params
    , outputs = List.map (\p ->
      { name = fakeLocated p.name
      , size = fakeLocated p.size
      }) outputs
    , locals = []
    , body = Binding (fakeLocated "<built-in function>")
    }


check : List Def -> Result (List Problem) ()
check defs =
  let
    problems =
      List.foldl
        (\def ps ->
          let
            _ = Debug.log "AL -> def" <| def
            beforeDefs =
              List.Extra.takeWhile ((/=) def) defs
            afterDefs =
              List.drop (List.length beforeDefs + 1) defs
          in
          checkDef (prelude ++ beforeDefs) afterDefs def ++ ps
        )
        []
        defs
  in
  case problems of
    [] ->
      Ok ()
    ps ->
      Err <| List.reverse <| EverySet.toList <| EverySet.fromList ps


checkDef : List Def -> List Def -> Def -> List Problem
checkDef beforeDefs afterDefs def =
  let
    _ = Debug.log "AL -> def" <| def
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

    allDefs =
      beforeDefs ++ (def :: afterDefs)

    typeErrors =
      case def of
        FuncDef { params, locals, body, outputs } ->
          let
            _ = Debug.log "AL -> locals" <| locals
            paramDefs =
              List.map paramToDef params
            
            funcDefs =
              allDefs ++ paramDefs ++ locals

            _ = Debug.log "AL -> bodyType" <| bodyType
            bodyType =
              getLocatedType funcDefs body
            
            _ = Debug.log "AL -> retType" <| retType
            retType =
              outputsToLocatedType params [] outputs

            retTypeErrors =
              matchTypes retType bodyType
          in
          List.foldl
            (\local localErrs ->
              let
                beforeLocal =
                  beforeDefs ++ (def :: paramDefs)
                afterLocal =
                  List.filter ((/=) local) locals ++ afterDefs
              in
              checkDef beforeLocal afterLocal local ++ localErrs
            )
            []
            locals
          ++ retTypeErrors
        
        BindingDef { locals, body } ->
          List.foldl
            (\local localErrs ->
              let
                beforeLocal =
                  beforeDefs ++ [ def ]
                afterLocal =
                  List.filter ((/=) local) locals ++ afterDefs
              in
              checkDef beforeLocal afterLocal local ++ localErrs
            )
            []
            locals
          ++ checkExpr (allDefs ++ locals) body

    problems =
      duplicatedNames ++ typeErrors
  in
  problems


checkExpr : List Def -> Expr -> List Problem
checkExpr defs expr =
  case expr of
    Binding name ->
      case getDef defs name of
        Just _ ->
          []
        Nothing ->
          [ UndefinedName name ]
    Call callee args ->
      let
        _ = Debug.log "AL -> args" <| args
        _ = Debug.log "AL -> defs" <| defs
      in
      case getDef defs callee of
        Just calleeDef ->
          case calleeDef of
            FuncDef { params, outputs, body } ->
              let
                argTypes =
                  List.map (getLocatedType defs) args
              in
              if List.length params /= List.length args then
                [ WrongCallArity callee params argTypes ]
              else
                let
                  paramTypeErrors =
                    List.foldl
                      (\(param, arg) problems ->
                        matchTypes (paramToLocatedType param) (getLocatedType defs arg) ++ problems
                      )
                      []
                      (List.map2 Tuple.pair params args)
                  
                  retType =
                    outputsToLocatedType params argTypes outputs
                in
                case retType.value of
                  ErrorType retTypeProblems ->
                    paramTypeErrors ++ retTypeProblems
                  _ ->
                    paramTypeErrors

            BindingDef _ ->
              [ CalleeNotFunction callee ]

        Nothing ->
          [ UndefinedName callee ]

    Indexing e (from, to) ->
      case getType defs (Indexing e (from, to)) of
        ErrorType problems ->
          problems
        _ ->
          []
    
    Record r ->
      let
        t =
          getType defs (Record r)
      in
      case t of
        RecordType record ->
          Dict.foldl
            (\_ value problems ->
              case value of
                ErrorType p ->
                  p ++ problems
                _ ->
                  problems
            )
            []
            record
        _ ->
          []


paramToLocatedType : Param -> Located Type
paramToLocatedType p =
  { from = p.name.from
  , to = p.size.to
  , value = BusType p.size.value
  }


outputsToLocatedType : List Param -> List (Located Type) -> List Param -> Located Type
outputsToLocatedType params argTypes outputs =
  let
    t =
      outputsToType params argTypes outputs
  in
  case outputs of
    [ single ] ->
      { from = single.size.from
      , to = single.size.to
      , value = t
      }
    first :: rests ->
      { from = first.name.from
      , to = (.name >> .to) <| Maybe.withDefault first (List.Extra.last rests)
      , value = t
      }
    [] ->
      fakeLocated <| ErrorType [] -- impossible


outputsToType : List Param -> List (Located Type) -> List Param -> Type
outputsToType params argTypes outputs =
  let
    (varSizeSubstitutions, _, substitutionProblems) =
      List.foldl
        (\(p, a) (sizeSubsts, argSubsts, problems) ->
          let
            prev =
              (sizeSubsts, argSubsts, problems) 
          in
          case p.size.value of
            VarSize n s1 ->
              case a.value of
                BusType s2 ->
                  case Dict.get n sizeSubsts of
                    Just s3 ->
                      if s3 /= s2 then -- substitution conflict
                        let
                          prevArg =
                            Maybe.withDefault a <| Dict.get n argSubsts
                        in
                        (sizeSubsts, argSubsts, ConflictingVarSizeArgs p prevArg a :: problems)
                      else
                        prev
                    Nothing ->
                      (Dict.insert n s2 sizeSubsts, Dict.insert n a argSubsts, problems)
                _ ->
                  prev
            IntSize _ ->
              prev
        )
        (Dict.empty, Dict.empty, [])
        (List.map2 Tuple.pair params argTypes)
    
    substituteOutputVarSize s =
      case s of
        VarSize n _ as varSize ->
          case Dict.get n varSizeSubstitutions of
            Just s1 ->
              s1
            Nothing ->
              varSize
        IntSize s1 ->
          IntSize s1

    outputType =
      case outputs of
        [ single ] ->
          BusType <| substituteOutputVarSize single.size.value
        many ->
          RecordType <|
            Dict.fromList <|
              List.map
                (\{name, size} ->
                  (name.value, BusType <| substituteOutputVarSize size.value)
                )
                many
  in
  case substitutionProblems of
    [] ->
      outputType
    _ ->
      ErrorType substitutionProblems


matchTypes : Located Type -> Located Type -> List Problem
matchTypes expected actual =
  let
    problem =
      [ MismatchedTypes expected actual ]
    success =
      []
  in
  case (expected.value, actual.value) of
    (BusType expectedSize, BusType actualSize) ->
      let
        matchIntSize s1 s2 =
          if s1 /= s2 then
            problem
          else
            success
      in
      case (expectedSize, actualSize) of
        (IntSize s1, IntSize s2) ->
          matchIntSize s1 s2
        (IntSize s1, VarSize _ s2) ->
          case s2 of
            Nothing ->
              problem
            Just s3 ->
              matchIntSize s1 s3
        (VarSize _ s1, IntSize s2) ->
          case s1 of
            Nothing ->
              success
            Just s3 ->
              matchIntSize s3 s2
        (VarSize n1 s1, VarSize n2 s2) ->
          -- TODO: update variable size value
          if n1 == n2 then
            success
          else
            case (s1, s2) of
              (Just s3, Just s4) ->
                matchIntSize s3 s4
              _ ->
                problem

  
    (RecordType expectedRecord, RecordType actualRecord) ->
      if Set.diff
        (Set.fromList <| Dict.keys expectedRecord)
        (Set.fromList <| Dict.keys actualRecord)
        /= Set.empty
      then
        problem
      else
        List.foldl
          (\key problems ->
            let
              expectedType =
                Dict.get key expectedRecord
              actualType =
                Dict.get key actualRecord
            in
            case (expectedType, actualType) of
              (Nothing, _) ->
                success -- impossible
              (_, Nothing) ->
                success --impossible
              (Just t1, Just t2) ->
                matchTypes -- TODO: locate each key and value in RecordType
                  { from = expected.from
                  , to = expected.to
                  , value = t1
                  }
                  { from = actual.from
                  , to = actual.to
                  , value = t2
                  }
                ++ problems
          )
          []
          (Dict.keys expectedRecord)

    (ErrorType p1, ErrorType p2) ->
      p1 ++ p2

    (ErrorType p1, _) ->
      p1
    
    (_, ErrorType p2) ->
      p2
    
    _ ->
      problem


getLocatedType : List Def -> Expr -> Located Type
getLocatedType defs expr =
  let
    exprLocation =
      locateExpr expr
  in
  { from = exprLocation.from
  , to = exprLocation.to
  , value = getType defs expr
  }


locateExpr : Expr -> Location
locateExpr expr =
  case expr of
    Binding bindingName ->
      { from = bindingName.from
      , to = bindingName.to
      }
    Call callee args ->
      { from = callee.from
      , to = Maybe.withDefault callee.to <| Maybe.map (.to << locateExpr) <| List.Extra.last args
      }
    Indexing e (_, to) ->
      { from = .from <| locateExpr e
      , to = to.to
      }
    Record r ->
      { from = r.from
      , to = r.to
      }

getType : List Def -> Expr -> Type
getType defs expr =
  case expr of
    Binding bindingName ->
      case getDef defs bindingName of
        Just def ->
          case def of
            FuncDef { name } ->
              ErrorType [ ExpectingBindingGotFunction bindingName name ]
            BindingDef { body, size } ->
              case size of
                Just s ->
                  BusType s.value
                Nothing ->
                  getType (List.filter ((/=) def) defs) body
        Nothing ->
          ErrorType [ UndefinedName bindingName ]
    Call callee args ->
      case getDef defs callee of
        Just def ->
          case def of
            FuncDef { params, outputs } ->
              let
                callProblems =
                  checkExpr defs (Call callee args)
              in
              case callProblems of
                [] ->
                  outputsToType params (List.map (getLocatedType defs) args) outputs
                _ ->
                  ErrorType callProblems
            BindingDef { name } ->
              ErrorType [ ExpectingFunctionGotBinding callee name ]
        Nothing ->
          ErrorType [ UndefinedName callee ]
    Indexing e (from, to) ->
      let
        t =
          getType defs e
      in
      case t of
        BusType size ->
          let
            slicedBusType s =
              if from.value >= s || to.value >= s then
                ErrorType [ IndexOutOfBounds s from to ]
              else
                BusType <| IntSize (to.value - from.value + 1)
          in
          if from.value > to.value then
            ErrorType [ FromIndexBiggerThanToIndex from to ]
          else
            case size of
              IntSize s ->
                slicedBusType s
              VarSize n s1 ->
                case s1 of
                  Just s2 ->
                    slicedBusType s2
                  Nothing ->
                    BusType <| VarSize n (Just <| from.value + 1)
        RecordType r ->
          ErrorType [ TryIndexingRecordType r (from, to) ]
        ErrorType problems ->
          ErrorType problems
    Record r ->
      RecordType <|
        Dict.fromList <|
          List.map
            (\(n, e) ->
              (n.value, getType defs e)
            )
            (Dict.toList r.value)


getDef : List Def -> Located String -> Maybe Def
getDef defs bindingName =
  List.Extra.find
    (\def ->
      let
        defNames =
          case def of
            FuncDef { name } ->
              [ name.value ]
            BindingDef { name } ->
              case name of
                BindingName n ->
                  [ n.value ]
                BindingRecord r ->
                  List.map .value <| Dict.keys r
      in
      List.member bindingName.value defNames
    )
    defs

paramToDef : Param -> Def
paramToDef p =
  BindingDef
    { name = BindingName p.name
    , locals = []
    , body = Binding (fakeLocated "")
    , size = Just p.size
    }


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
