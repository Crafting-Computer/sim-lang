module HdlChecker exposing (Problem(..), Type(..), SizeComparator(..), check, showProblems, getTargetNamesFromDef, getSourceNamesFromDef)

import HdlParser exposing (fakeLocated, bindingTargetToString, withLocation, Located, Param, Def(..), Expr(..), BindingTarget(..), Size(..))
import AssocList as Dict exposing (Dict)
import List.Extra
import Binary
import EverySet
import Tuple3

type Problem
  = DuplicatedName (Located String) (Located String)
  | UndefinedName (Located String)
  | WrongCallArity (Located String) (List (Located Type)) (List (Located Type)) -- callee paramTypes argTypes
  | InvalidIndexingTarget (Located Type) (Located Int, Located Int) -- targetType indices
  | FromIndexBiggerThanToIndex (Located Int) (Located Int) -- from to
  | ExpectingRecord (Located Type)
  | MismatchedTypes (Located Type) (Located Type)
  | BindingNotAllowedAtTopLevel (Located BindingTarget) -- bindingName


prelude : List Def
prelude =
  [ preludeFuncDef
    "nand"
    [ { name = "a", size = VarSize "n" }
    , { name = "b", size = VarSize "n" }
    ]
    [ { name = "out", size = VarSize "n" }
    ]
  , preludeFuncDef
    "fill"
    [ { name = "a", size = IntSize 1 }
    ]
    [ { name = "out", size = VarSize "n" }
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
    , outputs = fakeLocated <| List.map (\p ->
      { name = fakeLocated p.name
      , size = fakeLocated p.size
      }) outputs
    , locals = []
    , body = fakeLocated <| Binding (fakeLocated "<built-in function>")
    }


-- Environment and next type variable's id
type alias Ctx =
  { nextId : Int
  , level : Int
  , env : Env
  }


type alias TVarId =
  String


emptyCtx : Ctx
emptyCtx =
  { nextId = 0
  , level = 0
  , env = Dict.empty
  }


getTypeFromCtx : String -> Ctx -> Maybe (Located Type)
getTypeFromCtx name ctx =
  Maybe.map
    Tuple.second
    (getLocatedNameAndTypeFromCtx name ctx)


getLocatedNameAndTypeFromCtx : String -> Ctx -> Maybe (Located String, Located Type)
getLocatedNameAndTypeFromCtx name ctx =
  let
    locatedName =
      List.Extra.find
      (\k ->
        k.value == name
      )
      (Dict.keys ctx.env)
  in
  Maybe.andThen
    (\n ->
      Maybe.map
        (Tuple.pair n)
        (Dict.get n ctx.env)
    )
    locatedName


addToCtx : Located String -> Located Type -> Ctx -> Result (List Problem) Ctx
addToCtx n t c =
  case getLocatedNameAndTypeFromCtx n.value c of
    Nothing ->
      Ok <| addToCtxAllowDuplicates n t c

    Just (n2, _) ->
      Err
        [ DuplicatedName (withLocation n2 n.value) n
        ]


addToCtxAllowDuplicates : Located String -> Located Type -> Ctx -> Ctx
addToCtxAllowDuplicates n t c =
  { c
    | env =
      Dict.insert n t c.env
  }


newTVar : Located a -> Ctx -> (Located Type, Ctx)
newTVar location ctx =
  ( withLocation location <| TVar ("T" ++ String.fromInt ctx.nextId)
  , { ctx
    | nextId =
      ctx.nextId + 1
    }
  )


-- Map identifier to their type
type alias Env =
  Dict (Located String) (Located Type)


-- Map type variable id to type
type Subst
  = Subst (Dict String (Located Type))
  | SubstError (List Problem)


emptySubst : Subst
emptySubst =
  Subst <| Dict.empty


combineSubsts : Subst -> Subst -> Subst
combineSubsts subst1 subst2 =
  case (subst1, subst2) of
    (Subst s1, Subst s2) ->
      Subst <| Dict.union
        (Dict.map
          (\k v -> applySubstToType subst1 v)
          s2
        )
        s1
    
    (Subst _, SubstError _) ->
      subst2
    
    (SubstError _, Subst _) ->
      subst1
    
    (SubstError s1, SubstError s2) ->
      SubstError (s2 ++ s1)



applySubstToType : Subst -> (Located Type) -> (Located Type)
applySubstToType subst t =
  case t.value of
    TBus size _ ->
      let
        name =
          case size of
            VarSize n ->
              n

            IntSize _ ->
              nameFromLocated t
      in
      case subst of
        Subst s ->
          case Dict.get name s of
            Just busType ->
              busType
            
            Nothing ->
              t

        SubstError _ ->
          t
    
    TRecord r ->
      withLocation t <|
        TRecord <|
        Dict.map
          (\k v -> applySubstToType subst v)
          r
    
    TFun from to ->
      withLocation t <|
        TFun (applySubstToType subst from) (applySubstToType subst to)
    
    TVar id ->
      case subst of
        Subst s ->
          case Dict.get id s of
            Just varType ->
              varType
            Nothing ->
              t

        SubstError _ ->
          t
    

applySubstToCtx : Subst -> Ctx -> Ctx
applySubstToCtx subst ctx =
  { ctx
    | env =
      Dict.map
        (\k v -> applySubstToType subst v)
        ctx.env
  }


match : Located Type -> Located Type -> List Problem
match expectedType actualType =
  let
    mismatch =
      [ MismatchedTypes expectedType actualType ]
    
    success =
      []
  in
  case (expectedType.value, actualType.value) of
    (TBus b1 c1, TBus b2 c2) ->
      case (b1, b2) of
        (VarSize n1, VarSize n2) ->
          if n1 /= n2 then
            mismatch
          else
            success
        
        (VarSize _, IntSize _) ->
          mismatch
        
        (IntSize _, VarSize _) ->
          mismatch
        
        (IntSize i1, IntSize i2) ->
          let
            matchSuccess =
              case (c1, c2) of
                (EqualToSize, EqualToSize) ->
                  i1 == i2
                
                (EqualToSize, GreaterThanSize) ->
                  i1 > i2

                (GreaterThanSize, EqualToSize) ->
                  i2 > i1

                (GreaterThanSize, GreaterThanSize) ->
                  i2 >= i1
          in
          if matchSuccess then
            success
          else
            mismatch
    
    (TRecord r1, TRecord r2) ->
      if Dict.isEmpty <| Dict.union (Dict.diff r1 r2) (Dict.diff r2 r1) then
        Dict.foldl
          (\k v1 resultMatch ->
            let
              v2 =
                -- impossible Maybe
                Maybe.withDefault v1 <| Dict.get k r2
            in
            resultMatch ++ match v1 v2
          )
          []
          r1
      else
        mismatch

    (TFun from1 to1, TFun from2 to2) ->
      let
        fromMatch =
          match from1 from2
        
        toMatch =
          match to1 to2
      in
      fromMatch ++ toMatch

    (TVar v1, TVar v2) ->
      if v1 /= v2 then
        mismatch
      else
        success
    
    (TVar _, _) ->
      mismatch

    (_, TVar _) ->
      mismatch

    _ ->
      mismatch


-- t1 is expected type
-- t2 is actual type
unify : Located Type -> Located Type -> Subst
unify t1 t2 =
  let
    mismatch =
      SubstError [ MismatchedTypes t1 t2 ]
  in
  case (t1.value, t2.value) of
    (TBus b1 c1, TBus b2 c2) ->
      case (b1, b2) of
        (VarSize n1, VarSize _) ->
          Subst <| Dict.singleton n1 t2
        
        (VarSize n1, IntSize _) ->
          Subst <| Dict.singleton n1 t2
        
        (IntSize _, VarSize n2) ->
          Subst <| Dict.singleton n2 t1
        
        (IntSize i1, IntSize i2) ->
          let
            n1 =
              nameFromLocated t1
            
            n2 =
              nameFromLocated t2

            subst1 =
              Subst <| Dict.singleton n2 t1

            subst2 =
              Subst <| Dict.singleton n1 t2
          in
          case (c1, c2) of
            (GreaterThanSize, GreaterThanSize) ->
              if i1 >= i2 then
                subst1
              else
                subst2
            
            (GreaterThanSize, EqualToSize) ->
              if i1 >= i2 then
                mismatch
              else
                subst2
            
            (EqualToSize, GreaterThanSize) ->
              if i2 >= i1 then
                mismatch
              else
                subst1

            (EqualToSize, EqualToSize) ->
              if i1 >= i2 then
                emptySubst
              else
                mismatch
    
    (TRecord r1, TRecord r2) ->
      if Dict.isEmpty <| Dict.union (Dict.diff r1 r2) (Dict.diff r2 r1) then
        Dict.foldl
          (\k v1 resultSubst ->
            let
              v2 =
                -- impossible Maybe
                Maybe.withDefault v1 <| Dict.get k r2
            in
            combineSubsts resultSubst (unify v1 v2)
          )
          emptySubst
          r1
      else
        mismatch

    (TFun from1 to1, TFun from2 to2) ->
      let
        fromSubst =
          unify from1 from2
        
        toSubst =
          unify to1 to2
      in
      combineSubsts fromSubst toSubst

    (TVar v1, _) ->
      varBind (withLocation t1 v1) t2
    
    (_, TVar v2) ->
      varBind (withLocation t2 v2) t1

    _ ->
      mismatch


nameFromLocated : Located a -> String
nameFromLocated i =
  let
    locationToString (row, col) =
      String.fromInt row ++ "_" ++ String.fromInt col
  in
  "_located_" ++ locationToString i.from ++ "_" ++ locationToString i.to


varBind : Located TVarId -> Located Type -> Subst
varBind id1 t =
  let
    testContains =
      if contains id1.value t.value then
        SubstError [ MismatchedTypes (withLocation id1 <| TVar id1.value) t ]
      else
        Subst <|
          Dict.singleton id1.value t
  in
  case t.value of
    TVar id2 ->
      if id1.value == id2 then
        emptySubst
      else
        testContains
    
    _ ->
      testContains


contains : TVarId -> Type -> Bool
contains id1 t =
  case t of
    TVar id2 ->
      id1 == id2

    TFun from to ->
      contains id1 from.value || contains id1 to.value
    
    _ ->
      False


type Type
  = TBus Size SizeComparator
  | TRecord (Dict String (Located Type))
  | TFun (Located Type) (Located Type)
  | TVar TVarId


type SizeComparator
  = GreaterThanSize
  | EqualToSize


impossibleLocatedType : Located Type
impossibleLocatedType =
  fakeLocated <| TVar "impossible"


check : List Def -> Result (List Problem) ()
check defs =
  let
    allDefs =
      prelude ++ defs

    ctx =
      List.foldl
        (\d nextCtx ->
          Result.andThen
          (\c ->
            case d of
              FuncDef { name, params, outputs } ->
                addToCtx name (createTFun params outputs) c

              BindingDef _ ->
                Ok c
          )
          nextCtx
        )
        (Ok emptyCtx)
        allDefs
    
    problems =
      List.foldl
        (\def ps ->
          case ctx of
            Ok c ->
              let
                defNames =
                  case def of
                    FuncDef { name } ->
                      [ name.value ]

                    BindingDef { name } ->
                      case name.value of
                        BindingName n ->
                          [ n ]
                        
                        BindingRecord r ->
                          List.map .value <| Dict.values r

                defCtx =
                  { c
                    | env =
                      Dict.filter
                        (\k _ ->
                          not <| List.member k.value defNames
                        )
                        c.env
                  }
              in
              case inferDef defCtx def of
                Err defProblems ->
                  defProblems ++ ps
                
                Ok (_, _, subst) ->
                  case subst of
                    Subst _ ->
                      ps
                    
                    SubstError errs ->
                      errs ++ ps
            
            Err ctxProblems ->
              ctxProblems ++ ps
        )
        []
        defs
  in
  case problems of
    [] ->
      Ok ()
    ps ->
      let
        uniqueProblems =
          List.reverse <| EverySet.toList <| EverySet.fromList ps
      in
      Err <|
        List.map Tuple.second <|
        List.filter
        (\(i, p) ->
          case p of
            MismatchedTypes expectedType actualType ->
              case (expectedType.value, actualType.value) of
                (TVar _, _) ->
                  False
                
                (_, TVar _) ->
                  False
                
                _ ->
                  case List.Extra.getAt (i-1) uniqueProblems of
                    Nothing ->
                      True
                    
                    Just previousProblem ->
                      case previousProblem of
                        MismatchedTypes pt1 pt2 ->
                          -- filter out mismatches that has the same looking error messages
                          -- expectedType's location is not shown so we only compare type values
                          not ((typeToString expectedType.value == typeToString pt1.value) && (actualType == pt2))
                        
                        _ ->
                          True
            
            _ ->
              True
        ) <|
        List.map2 Tuple.pair (List.range 0 <| List.length uniqueProblems) <|
        List.map
          (\p ->
            case p of
              MismatchedTypes t1 t2 ->
                let
                  usedInBuiltIn : Located a -> Bool
                  usedInBuiltIn located =
                    Tuple.first located.from == -1

                  (expectedType, actualType) =
                    let
                      t1UsedInBuiltIn =
                        usedInBuiltIn t1
                      
                      t2UsedInBuiltIn =
                        usedInBuiltIn t2
                    in
                    if t1UsedInBuiltIn && t2UsedInBuiltIn then
                      (t1, t2)
                    else if t1UsedInBuiltIn then
                      (t1, t2)
                    else if t2UsedInBuiltIn then
                      (t2, t1)
                    else
                      (t1, t2)
                in
                MismatchedTypes expectedType actualType
              
              _ ->
                p
          )
          uniqueProblems


inferDef : Ctx -> Def -> Result (List Problem) (Located Type, Ctx, Subst)
inferDef ctx def =
  case def of
    BindingDef { name, locals, body } ->
      if ctx.level <= 0 then
        Err [ BindingNotAllowedAtTopLevel name ]
      else
        inferLocalsAndBody ctx locals body
    
    FuncDef { params, outputs, locals, body } ->
      let
        paramCtx =
          List.foldl
            (\p resultParamCtx ->
              Result.andThen
              (\c ->
                let
                  paramName =
                    p.name
                  
                  (paramType, nextCtx) =
                    newTVar p.size c
                in
                addToCtx paramName paramType nextCtx
              )
              resultParamCtx
            )
            (Ok ctx)
            params
      in
      paramCtx |>
      Result.andThen
        (\c ->
          inferLocalsAndBody c locals body
        ) |>
      Result.andThen
        (\(outputType1, outputCtx1, outputSubst) ->
          let
            outputCtx =
              applySubstToCtx outputSubst outputCtx1
            
            declaredFuncType =
              createTFun params outputs

            paramTypes =
              List.map
                (\p ->
                  case getTypeFromCtx p.name.value outputCtx of
                    Just t ->
                      t
                    Nothing ->
                      impossibleLocatedType -- impossible
                )
                params

            outputType =
              applySubstToType outputSubst outputType1

            actualFuncType =
              createTFunFromTypes paramTypes outputType
            
            funcSubst =
              unify declaredFuncType actualFuncType

            resultFuncType =
              applySubstToType funcSubst actualFuncType
              
            resultSubst =
              combineSubsts outputSubst funcSubst
          in
          case match declaredFuncType resultFuncType of
            [] ->
              Ok (resultFuncType, outputCtx, resultSubst)

            matchErrs ->
              Err matchErrs
        )


inferLocalsAndBody : Ctx -> List Def -> Located Expr -> Result (List Problem) (Located Type, Ctx, Subst)
inferLocalsAndBody ctx locals body =
  inferDefs (incrementLevel ctx) locals |>
  Result.andThen
    (\(localCtx, localSubst) ->
      inferExpr localCtx body |>
      Result.map
        (\(bodyType, bodyCtx, bodySubst) ->
          let
            resultCtx =
              bodyCtx

            resultSubst =
              combineSubsts localSubst bodySubst
          in
          ( applySubstToType resultSubst bodyType
          , resultCtx
          , resultSubst
          )
        )
    )
  


inferExpr : Ctx -> Located Expr -> Result (List Problem) (Located Type, Ctx, Subst)
inferExpr ctx expr =
  case expr.value of
    Binding name ->
      case getTypeFromCtx name.value ctx of
        Just t ->
          Ok (t, ctx, emptySubst)
        
        Nothing ->
          Err [ UndefinedName name ]
      
    
    IntLiteral size ->
      Ok ( withLocation size <| TBus (IntSize <| Binary.width <| Binary.fromDecimal size.value) EqualToSize
      , ctx
      , emptySubst
      )
    
    Indexing e (from, to) ->
      inferExpr ctx e |>
      Result.andThen
        (\(t, resultCtx, s1) ->
          let
            (indexingType, s2) =
              case t.value of
                TBus size _ ->
                  if from.value > to.value then
                    ( Err [ FromIndexBiggerThanToIndex from to ]
                    , emptySubst
                    )
                  else
                    case size of
                      IntSize s ->
                        if to.value >= s then
                          ( Ok <| withLocation expr <| TBus (IntSize (to.value - from.value + 1)) EqualToSize
                          , Subst <| Dict.singleton (nameFromLocated t) (withLocation e <| TBus (IntSize to.value) GreaterThanSize)
                          )
                        else
                          ( Ok <| withLocation expr <| TBus (IntSize (to.value - from.value + 1)) EqualToSize
                          , emptySubst
                          )
                      
                      VarSize n ->
                        ( Ok <| withLocation expr <| TBus (IntSize (to.value - from.value + 1)) EqualToSize
                        , Subst <|
                          Dict.singleton
                          n
                          (withLocation e <| TBus (IntSize to.value) GreaterThanSize)
                        )
                
                TRecord _ ->
                  ( Err [ InvalidIndexingTarget t (from, to) ]
                  , emptySubst
                  )
                
                TFun _ _ ->
                  ( Err [ InvalidIndexingTarget t (from, to) ]
                  , emptySubst
                  )

                TVar n ->
                  if from.value > to.value then
                    ( Err [ FromIndexBiggerThanToIndex from to ]
                    , emptySubst
                    )
                  else
                    ( Ok <| withLocation expr <| TBus (IntSize (to.value - from.value + 1)) EqualToSize
                    , Subst <|
                      Dict.singleton
                      n
                      (withLocation e <| TBus (IntSize to.value) GreaterThanSize)
                    )
                
            resultSubst =
              combineSubsts s1 s2
          in
          Result.map (\indexingT -> (indexingT, resultCtx, resultSubst)) indexingType
        )

    Record r ->
      Result.map (Tuple3.mapFirst (withLocation r << TRecord)) <|
        Dict.foldl
          (\k v result ->
            Result.andThen
            (\(resultDict, resultCtx, resultSubst) ->
              inferExpr resultCtx v |>
                Result.map
                (\(t, nextCtx, nextSubst) ->
                  ( Dict.insert k.value t resultDict
                  , nextCtx
                  , combineSubsts resultSubst nextSubst
                  )
                )
            )
            result
          )
          (Ok (Dict.empty, ctx, emptySubst))
          r.value

    Call callee args ->
      inferExpr ctx (withLocation callee <| Binding callee) |>
      Result.andThen
        (\(funcType, c1, s1) ->
          List.foldl
            (\arg argResult ->
              Result.andThen
              (\(nextTypes, nextCtx, nextSubst) ->
                inferExpr (applySubstToCtx nextSubst nextCtx) arg |>
                Result.map
                (Tuple3.mapFirst
                  (\t ->
                    nextTypes ++ [ t ]
                  )
                )
              )
              argResult
            )
            (Ok ([], c1, emptySubst))
            args |>
          Result.andThen
            (\(argTypes, c2, s2) ->
              let
                (outputType, c3) =
                  newTVar expr c2
                
                s3 =
                  combineSubsts s1 s2
                
                s4 =
                  unify
                    funcType
                    (createTFunFromTypes argTypes outputType)
                
                funcType1 =
                  applySubstToType s4 funcType

                paramTypes =
                  paramTypesFromTFun funcType1
              in
              if List.length paramTypes /= List.length argTypes then
                Err [ WrongCallArity callee paramTypes <|
                  List.map2
                    (\arg argType ->
                      withLocation arg argType.value
                    )
                    args
                    argTypes
                ]
              else
                let
                  callResult =
                    List.foldl
                      (\argType result ->
                        Result.andThen
                          (\(nextType, nextSubst) ->
                            case nextType.value of
                              TFun fromType toType ->
                                Ok ( toType
                                , combineSubsts nextSubst (unify (applySubstToType nextSubst fromType) argType)
                                )

                              _ ->
                                Err [ MismatchedTypes nextType argType ]
                          )
                          result
                      )
                      (Ok (funcType1, combineSubsts s3 s4))
                      argTypes
                in
                Result.map
                  (\(resultType, resultSubst) ->
                    ( applySubstToType resultSubst resultType
                    , c3
                    , resultSubst
                    )
                  )
                  callResult
            )
        )


paramTypesFromTFun : Located Type -> List (Located Type)
paramTypesFromTFun funcType =
  case funcType.value of
    TFun from to ->
      from :: paramTypesFromTFun to
    
    _ ->
      [ ]


incrementLevel : Ctx -> Ctx
incrementLevel ctx =
  { ctx
    | level =
      ctx.level + 1
  }


inferDefs : Ctx -> List Def -> Result (List Problem) (Ctx, Subst)
inferDefs ctx defs =
  let
    declaredCtx =
      List.foldl
        (\d nextCtx ->
          Result.andThen
          (\nextCtx1 ->
            case d of
              FuncDef { name, params, outputs } ->
                addToCtx name (createTFun params outputs) nextCtx1

              BindingDef { name } ->
                let
                  bindings =
                    case name.value of
                      BindingName n ->
                        [ withLocation name n ]
                      
                      BindingRecord r ->
                        Dict.values r
                  
                  bindingCtx =
                    List.foldl
                      (\binding ctx1 ->
                        Result.andThen
                        (\c1 ->
                          let
                            (t, ctx2) =
                              newTVar binding c1
                          in
                          addToCtx binding t ctx2
                        )
                        ctx1
                      )
                      (Ok nextCtx1)
                      bindings
                in
                bindingCtx
          )
          nextCtx
        )
        (Ok ctx)
        defs
  in
  declaredCtx |>
  Result.andThen
  (\declaredCtx1 -> List.foldl
    (\def result ->
      Result.andThen
        (\(resultCtx, resultSubst) ->
          inferDef resultCtx def |>
          Result.andThen
            (\(defType, defCtx, defSubst) ->
              let
                nextSubst =
                  combineSubsts resultSubst defSubst
              in
              case def of
                FuncDef { name, params, locals } ->
                  let
                    funcScopeNames =
                      List.map (.name >> .value) params ++ (List.concat <| List.map getTargetNamesFromDef locals)
                    
                    defCtx1 =
                      { defCtx
                        | env =
                          Dict.filter
                            (\k _ ->
                              not (List.member k.value funcScopeNames)
                            )
                            defCtx.env
                      }
                  in
                  Ok ( addToCtxAllowDuplicates name defType <| defCtx1
                  , nextSubst
                  )
                
                BindingDef { name, locals } ->
                  let
                    bindingScopeNames =
                      List.concat <| List.map getTargetNamesFromDef locals
                    
                    defCtx1 =
                      { defCtx
                        | env =
                          Dict.filter
                            (\k _ ->
                              not (List.member k.value bindingScopeNames)
                            )
                            defCtx.env
                      }
                  in
                  case name.value of
                    BindingName n ->
                      Ok ( addToCtxAllowDuplicates (withLocation name n) defType <| defCtx1
                      , nextSubst
                      )
                    
                    BindingRecord r ->
                      case defType.value of
                        TRecord typeRecord ->
                          Ok ( Dict.foldl
                            (\k v nextCtx ->
                              case Dict.get k.value typeRecord of
                                Just t ->
                                  addToCtxAllowDuplicates v t nextCtx
                                Nothing ->
                                  nextCtx
                            )
                            defCtx1
                            r
                          , nextSubst
                          )
                        
                        _ ->
                          Err <| [ ExpectingRecord defType ]
            )
        )
        result
      )
      (Ok (declaredCtx1, emptySubst))
      defs
  )


createTFunFromTypes : List (Located Type) -> Located Type -> Located Type
createTFunFromTypes paramTypes outputType =
  let
    createTFunHelper : List (Located Type) -> Located Type
    createTFunHelper types =
      case types of
        [ singleType ] ->
          singleType
        
        fromType :: restTypes ->
          { from =
            fromType.from
          , to =
            Maybe.withDefault fromType.to <| Maybe.map .to <| List.Extra.last restTypes
          , value =
            TFun fromType (createTFunHelper restTypes)
          }
        
        [] ->
          impossibleLocatedType -- impossible
  in
  createTFunHelper (paramTypes ++ [ outputType ])


createTFun : List Param -> Located (List Param) -> Located Type
createTFun params outputs =
  let
    paramTypes =
      List.map paramToLocatedType params
    
    outputType =
      paramsToTRecord outputs
  in
  createTFunFromTypes paramTypes outputType
  
  
paramsToTRecord : Located (List Param) -> Located Type
paramsToTRecord params =
  let
    paramTypes =
      List.map paramToLocatedType params.value
    
    paramNames =
      List.map (\p -> p.name.value) params.value
  in
  case paramTypes of
    [ singleType ] ->
      singleType
    
    _ ->
      withLocation params <|
        TRecord <| Dict.fromList <| List.reverse <| 
          List.map2 Tuple.pair paramNames paramTypes



paramToLocatedType : Param -> Located Type
paramToLocatedType p =
  withLocation p.size <| paramToType p


paramToType : Param -> Type
paramToType p =
  TBus p.size.value EqualToSize


showProblems : String -> List Problem -> String
showProblems src problems =
  String.join "\n\n" <| List.map (showProblem src) problems


locatedInPrelude : Located a -> Bool
locatedInPrelude located =
  Tuple.first located.from < 0



showProblem : String -> Problem -> String
showProblem src problem =
  case problem of
    DuplicatedName prevName currName ->
      if locatedInPrelude prevName then
        "I found that you are trying to redefine a built-in function `" ++ prevName.value ++ "` here:\n"
        ++ showLocation src currName ++ "\n"
        ++ "Hint: Try changing your name to avoid conflict with the built-in function."
      else
        "I found a duplicated name `" ++ currName.value ++ "` that is previously defined here:\n"
        ++ showLocation src prevName ++ "\n"
        ++ "but I found it defined again here:\n"
        ++ showLocation src currName ++ "\n"
        ++ "Hint: Try renaming one of the names to avoid conflict."
    UndefinedName undefinedName ->
      "I found an undefined name `" ++ undefinedName.value ++ "` here:\n"
      ++ showLocation src undefinedName ++ "\n"
      ++ "Hint: Try defining `" ++ undefinedName.value ++ "` before use."
    WrongCallArity callee paramTypes locatedArgTypes ->
      let
        argTypes =
          List.map .value locatedArgTypes
        paramLength =
          List.length paramTypes
        argLength =
          List.length argTypes
      in
      "I was expecting " ++ String.fromInt paramLength ++ " arguments but got " ++ String.fromInt argLength ++ ".\n"
      ++ (case locatedArgTypes of
        first :: rests ->
          showLocationRange src first (Maybe.withDefault first <| List.Extra.last rests) ++ "\n"
        [] ->
          ""
      )
      ++ "Hint: "
        ++ ( if paramLength > argLength then
          "Try adding "
          ++ ( case paramLength - argLength of
            1 ->
              "1 argument"
            difference ->
              String.fromInt difference ++ " arguments"
          )
          ++ " of type " ++ (String.join " and " <| List.map (typeToString << .value) <| List.drop argLength paramTypes) ++ " to match the parameter types."
        else
          "Try dropping " ++ String.fromInt (argLength - paramLength) ++ " arguments to match the parameter size."
        )
    InvalidIndexingTarget targetType (from, to) ->
      case targetType.value of
        TRecord _ ->
          "Are you trying to get a value of a record at some index? This doesn't work as you can only index into a bus type.\n"
          ++ showLocationRange src from to ++ "\n"
          ++ "Hint: Try destructing the record to get the values inside:\n\n"
          ++ "{ sum = s1, carry = c1 } = { sum = 0, carry = 1 }\n\n"
          ++ "Note that the record destructure automatically creates two new bindings `s1` and `c1`."
        TFun _ _ ->
          "I found that you are trying to index into a function. This is not allowed.\n"
          ++ showLocationRange src from to ++ "\n"
          ++ "Hint: Try switching from the function to a bus."
        _ -> -- must be BusType (IntLiteral)
          "Are you trying to slice an integer? This is not allowed.\n"
          ++ showLocationRange src from to ++ "\n"
          ++ "Hint: Try specifying the integer value you want directly."
    FromIndexBiggerThanToIndex from to ->
      "I found that the start index " ++ String.fromInt from.value ++ " is greater than " ++ " the end index " ++ String.fromInt to.value ++ ".\n"
      ++ showLocationRange src from to ++ "\n"
      ++ "Hint: Try limiting the start index to between 0 and " ++ (String.fromInt <| to.value) ++ "."
    MismatchedTypes expectedType actualType ->
      let
        (prettyExpectedType, prettyActualType) =
          prettifyTypes expectedType actualType
      in
      "I'm expecting to find the type " ++ typeToString prettyExpectedType.value ++ " here:\n"
      ++ showLocation src prettyActualType ++ "\n"
      ++ "but got the type " ++ typeToString prettyActualType.value ++ ".\n"
    BindingNotAllowedAtTopLevel bindingName ->
      "I found a binding called `" ++ bindingTargetToString bindingName.value ++ "` at the top level of this unit here:\n"
      ++ showLocation src bindingName ++ "\n"
      ++ "but you are not allowed to define binding at the top level.\n"
      ++ "Hint: Try defining a function instead of a binding."
    ExpectingRecord recordType ->
      "I'm expecting a record here:\n"
      ++ showLocation src recordType


prettifyTypes : Located Type -> Located Type -> (Located Type, Located Type)
prettifyTypes t1 t2 =
  let
    subst =
      prettifyTypesHelper t1 t2
  in
  (applySubstToType subst t1, applySubstToType subst t2)


prettifyTypesHelper : Located Type -> Located Type -> Subst
prettifyTypesHelper t1 t2 =
  case (t1.value, t2.value) of
    (TRecord r1, TRecord r2) ->
      Dict.foldl
        (\k v1 resultSubst ->
          let
            v2 =
              Maybe.withDefault v1 <| Dict.get k r2
          in
          combineSubsts resultSubst (unify v1 v2)
        )
        emptySubst
        r1
    
    _ ->
      emptySubst


showLocation : String -> Located a -> String
showLocation src located =
  let
    (fromRow, fromCol) =
      located.from
    (toRow, toCol) =
      located.to
  in
  HdlParser.showProblemLocationRange fromRow fromCol toRow toCol src


showLocationRange : String -> Located a -> Located a -> String
showLocationRange src start end =
  let
    (fromRow, fromCol) =
      start.from
    (toRow, toCol) =
      end.to
  in
  HdlParser.showProblemLocationRange fromRow fromCol toRow toCol src


typeToString : Type -> String
typeToString t =
  case t of
    TBus s c ->
      case s of
        VarSize _ ->
          sizeToString s

        IntSize i ->
          case c of
            GreaterThanSize ->
              "[" ++ String.fromInt (i + 1) ++ "]"
            
            EqualToSize ->
              sizeToString s
      
    
    TRecord r ->
      "{ " ++
      ( String.join ", " <|
        List.map
          (\(k, v) ->
            k ++ " = " ++ typeToString v.value
          )
          (Dict.toList r)
      ) ++ " }"
    
    TFun from to ->
      let
        fromType =
          typeToString from.value
        
        toType =
          typeToString to.value
      in
      fromType ++ " -> " ++ toType
    
    TVar id ->
      id
    

sizeToString : Size -> String
sizeToString s =
  "["
  ++ (case s of
    IntSize i ->
      String.fromInt i
    VarSize n ->
      n
  )
  ++ "]"

-- c = nand a b
-- target names : [ c ]
-- { a = first, b = second } = nand a b
-- target names : [ first, second ]
getTargetNamesFromDef : Def -> List String
getTargetNamesFromDef def =
  case def of
    FuncDef { name } ->
      [ name.value ]
    
    BindingDef { name } ->
      case name.value of
        BindingName n ->
          [ n ]
        
        BindingRecord r ->
          List.map .value <| Dict.values r


-- c = nand a b
-- source names : [ nand, a, b ]
getSourceNamesFromDef : Def -> List String
getSourceNamesFromDef def =
  let
    (l, b) =
      case def of
        FuncDef { locals, body } ->
          (locals, body)

        BindingDef { locals, body } ->
          (locals, body)
  in
  ( List.concat <|
    List.map
      getSourceNamesFromDef
      l
  ) ++ getNamesFromExpr b.value


getNamesFromExpr : Expr -> List String
getNamesFromExpr expr =
  case expr of
    Binding name ->
      [ name.value ]
    
    Call callee args ->
      callee.value :: (List.concat <| List.map (getNamesFromExpr << .value) args)
  
    Indexing e _ ->
      getNamesFromExpr e.value
    
    Record r ->
      List.concat <| List.map (getNamesFromExpr << .value) <| Dict.values r.value
    
    IntLiteral _ ->
      []