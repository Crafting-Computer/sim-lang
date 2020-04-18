module HdlParser exposing (parse, fakeLocated, showDeadEnds, Def(..), BindingTarget(..), Located, Expr(..), Param, Size(..))


import Parser.Advanced exposing (..)
import Set exposing (Set)
import AssocList as Dict exposing (Dict)
import List.Extra


type Def
  = FuncDef
    { name : Located String
    , params : List Param
    , outputs : Located (List Param)
    , locals : List Def
    , body : Expr
    }
  | BindingDef
    { name : BindingTarget
    , locals : List Def
    , body : Expr
    , size : Maybe (Located Size)
    }


type BindingTarget
  = BindingName (Located String)
  | BindingRecord (Dict (Located String) (Located String))


type Expr
  = Binding (Located String)
  | Call (Located String) (List Expr)
  | Indexing Expr (Located Int, Located Int)
  | Record (Located (Dict (Located String) Expr))


type alias Param =
  { name : Located String
  , size : Located Size
  }


type Size
  = IntSize Int
  | VarSize String (Maybe Int)


type Problem
  = ExpectingName
  | ExpectingInt
  | ExpectingLeftBracket
  | ExpectingRightBracket
  | ExpectingLet
  | ExpectingIn
  | ExpectingEqual
  | ExpectingEOF
  | ExpectingArrow
  | ExpectingStartOfLineComment
  | ExpectingStartOfMultiLineComment
  | ExpectingEndOfMultiLineComment
  | ExpectingLeftParen
  | ExpectingRightParen
  | ExpectingIndent
  | ExpectingDotDot
  | ExpectingLeftBrace
  | ExpectingRightBrace
  | ExpectingComma


type Context
  = BindingDefContext BindingTarget
  | FuncDefContext (Located String)
  | LocalsContext


type alias HdlParser a =
  Parser Context Problem a


type alias Located a =
  { from : (Int, Int)
  , value : a
  , to : (Int, Int)
  }


located : HdlParser a -> HdlParser (Located a)
located parser =
  succeed Located
    |= getPosition
    |= parser
    |= getPosition


reserved : Set String
reserved =
  Set.fromList
    [ "let", "in" ]


parse : String -> Result (List (DeadEnd Context Problem)) (List Def)
parse src =
  run (succeed identity |= defs |. end ExpectingEOF) src


showDeadEnds : String -> List (DeadEnd Context Problem) -> String
showDeadEnds src deadEnds =
  let
    deadEndGroups =
      List.Extra.groupWhile (\d1 d2 -> d1.row == d2.row && d1.col == d2.col) deadEnds
  in
  String.join "\n" <| List.map (showDeadEndsHelper src) deadEndGroups


showDeadEndsHelper : String -> ((DeadEnd Context Problem), List (DeadEnd Context Problem)) -> String
showDeadEndsHelper src (first, rests) =
  let
    location =
      showProblemLocation first.row first.col src
    problems =
      List.map (.problem >> showProblem) (first :: rests)
    context =
      showProblemContextStack first.contextStack
  in
  location ++ "\n"
  ++ "I'm expecting " ++ String.join " or " problems
  ++ (if String.isEmpty context then "" else " in the " ++ context)
  ++ "."


showProblemContextStack : List { row : Int, col : Int, context : Context } -> String
showProblemContextStack contexts =
  let
    _ = Debug.log "AL -> contexts" <| contexts
  in
  String.join " of the " <| List.map (.context >> showProblemContext) contexts


showProblemContext : Context -> String
showProblemContext context =
  case context of
    BindingDefContext bindingName ->
      let
        nameStr =
          case bindingName of
            BindingName n ->
              n.value
            BindingRecord r ->
              Dict.foldl
                (\k v str ->
                  str ++ k.value ++ " = " ++ v.value ++ ", "
                )
                "{ "
                r
              ++ "}"
      in
      "`" ++ nameStr ++ "`" ++ " definition"
    FuncDefContext funcName ->
      "`" ++ funcName.value ++ "`" ++ " definition"
    LocalsContext ->
      "local definitions"


showProblemLocation : Int -> Int -> String -> String
showProblemLocation row col src =
  let
    rawLine =
      getLine row src
    line =
      String.fromInt row ++ "| " ++ (String.trimLeft <| rawLine)
    offset =
      String.length line - String.length rawLine - 1
    offsettedCol =
      offset + col
    underline =
      makeUnderline line offsettedCol offsettedCol
  in
  line ++ "\n" ++ underline


makeUnderline : String -> Int -> Int -> String
makeUnderline row minCol maxCol =
  String.toList row
    |> List.indexedMap (\i _ -> toUnderlineChar minCol maxCol i)
    |> String.fromList


toUnderlineChar : Int -> Int -> Int -> Char
toUnderlineChar minCol maxCol col =
  if minCol <= col && col <= maxCol then
    '^'
  else
    ' '


getLine : Int -> String -> String
getLine row src =
  Maybe.withDefault ("CAN'T GET LINE AT ROW " ++ String.fromInt row) -- impossible
    <| List.Extra.getAt (row - 1) <| String.split "\n" src


showProblem : Problem -> String
showProblem problem =
  case problem of
    ExpectingName ->
      "a name"
    ExpectingInt ->
      "an integer"
    ExpectingLeftBracket ->
      "a '['"
    ExpectingRightBracket ->
      "a ']'"
    ExpectingLet ->
      "the keyword 'let'"
    ExpectingIn ->
      "the keyword 'in'"
    ExpectingEqual ->
      "a '='"
    ExpectingEOF ->
      "the end of program"
    ExpectingArrow ->
      "an '->'"
    ExpectingStartOfLineComment ->
      "the start of line comment '--'"
    ExpectingStartOfMultiLineComment ->
      "the start of multi-line comment '{-'"
    ExpectingEndOfMultiLineComment ->
      "the end of multi-line comment '-}'"
    ExpectingLeftParen ->
      "a '{'"
    ExpectingRightParen ->
      "a '}'"
    ExpectingIndent ->
      "an indentation"
    ExpectingDotDot ->
      "a '..'"
    ExpectingLeftBrace ->
      "a '{'"
    ExpectingRightBrace ->
      "a '}'"
    ExpectingComma ->
      "a ','"


defs : HdlParser (List Def)
defs =
  loop [] <| \revDefs ->
    oneOf
    [ succeed (\d -> Loop (d :: revDefs))
      |. sps
      |= oneOf
        [ bindingDef
        , funcDef          
        ]
      |. sps
    , succeed ()
      |> map (\_ -> Done (List.reverse revDefs))
    ]


bindingDef : HdlParser Def
bindingDef =
  succeed identity
    |. checkIndent
    |= oneOf
      [ backtrackable <| map BindingName name
      , succeed (BindingRecord << Dict.fromList) |= sequence
        { start = Token "{" ExpectingLeftBrace
        , separator = Token "," ExpectingComma
        , end = Token "}" ExpectingRightBrace
        , spaces = sps
        , item =
          succeed Tuple.pair
            |= name
            |. sps
            |. token (Token "=" ExpectingEqual)
            |. sps
            |= name
        , trailing = Forbidden
        }
      ]
    |> andThen
    (\bindingName ->
      inContext (BindingDefContext bindingName) <|
      succeed (\(defLocals, defBody) ->
        BindingDef { name = bindingName
        , locals = Maybe.withDefault [] defLocals
        , body = defBody
        , size = Nothing
        }
      )
      |. backtrackable sps
      |. token (Token "=" ExpectingEqual)
      |. sps
      |= (indent <|
          succeed Tuple.pair
          |= optional locals
          |. sps
          |= expr
        )
    )


funcDef : HdlParser Def
funcDef =
  succeed identity
    |. checkIndent
    |= name
    |> andThen
    (\funcName -> inContext (FuncDefContext funcName) <|
      succeed
      (\defParams defRetType (defLocals, defBody) ->
        FuncDef
          { name = funcName
          , params = defParams
          , outputs = defRetType
          , locals = Maybe.withDefault [] defLocals
          , body = defBody
          }
      )
      |. sps
      |= params
      |= outputs
      |. sps
      |. token (Token "=" ExpectingEqual)
      |. sps
      |= (indent <|
          succeed Tuple.pair
          |= optional locals
          |. sps
          |= expr
        )
    )


checkIndent : HdlParser ()
checkIndent =
  succeed (\indentation column ->
    let
      _ = Debug.log "AL -> column" <| column
    in
    indentation <= column
    )
    |= getIndent
    |= getCol
    |> andThen checkIndentHelp


checkIndentHelp : Bool -> HdlParser ()
checkIndentHelp isIndented =
  if isIndented then
    succeed ()
  else
    problem ExpectingIndent


locals : HdlParser (List Def)
locals =
  inContext LocalsContext <|
  succeed (\ds ->
    let
      _ = Debug.log "AL -> ds" <| ds
    in
    ds
  )
    |. checkIndent
    |. keyword (Token "let" ExpectingLet)
    |. sps
    |= (indent <|
      succeed identity
        |= lazy (\_ -> defs)
        |. sps
    )
    |. checkIndent
    |. keyword (Token "in" ExpectingIn)


indent : HdlParser a -> HdlParser a
indent parser =
  succeed (\indentation ->
    indentation + 2
  )
  |= getIndent
  |> andThen (\newIndentation ->
    let
      _ = Debug.log "AL -> newIndentation" <| newIndentation
    in
    withIndent newIndentation parser
  )


expr : HdlParser Expr
expr =
  oneOf
    [ group
    , bindingOrCall
    , record
    ]


record : HdlParser Expr
record =
  succeed (\locatedList -> Record <|
    { from = locatedList.from
    , to = locatedList.to
    , value = Dict.fromList locatedList.value
    })
    |= ( located <| sequence
    { start = Token "{" ExpectingLeftBrace
    , separator = Token "," ExpectingComma
    , end = Token "}" ExpectingRightBrace
    , spaces = sps
    , item =
      succeed Tuple.pair
        |= name
        |. sps
        |. token (Token "=" ExpectingEqual)
        |. sps
        |= lazy (\_ -> expr)
    , trailing = Forbidden
    }
    )


group : HdlParser Expr
group =
  succeed (\g i ->
    case i of
      Just indexes ->
        Indexing g indexes
      Nothing ->
        g
  )
    |. checkIndent
    |. token (Token "(" ExpectingLeftParen)
    |= lazy (\_ -> expr)
    |. token (Token ")" ExpectingRightParen)
    |= optional indexing


indexing : HdlParser (Located Int, Located Int)
indexing =
  succeed (\from to -> Tuple.pair from (Maybe.withDefault from to))
    |. token (Token "[" ExpectingLeftBracket)
    |. sps
    |= located integer
    |. sps
    |= (optional <|
      succeed identity
        |. token (Token ".." ExpectingDotDot)
        |. sps
        |= located integer
    )
    |. sps
    |. token (Token "]" ExpectingRightBracket)


integer : HdlParser Int
integer =
  map (\str -> Maybe.withDefault 0 <| String.toInt str) <|
    getChompedString <|
    succeed ()
      |. chompIf Char.isDigit ExpectingInt
      |. chompWhile Char.isDigit


binding : HdlParser Expr
binding =
  succeed (\n i ->
    case i of
      Just indexes ->
        Indexing (Binding n) indexes
      Nothing ->
        Binding n
    )
    |. checkIndent
    |= name
    |= optional indexing


bindingOrCall : HdlParser Expr
bindingOrCall =
  succeed
    (\callee args ->
      let
        _ = Debug.log "AL -> callee" <| callee
        _ = Debug.log "AL -> args" <| args
      in
      case args of
        [] ->
          Binding callee
        list ->
          Call callee list
    )
    |= name
    |. sps
    |=  ( loop [] <| \revExprs ->
      oneOf
        [ succeed (\n -> Loop (n :: revExprs))
          |= oneOf
            [ binding
            , group
            ]
          |. sps
        , succeed ()
          |> map (\_ -> Done (List.reverse revExprs))
        ]
    )


optional : HdlParser a -> HdlParser (Maybe a)
optional parser =
  oneOf
    [ parser |> map Just
    , succeed Nothing
    ]


outputs : HdlParser (Located (List Param))
outputs =
  let
    singleRetType =
      map (\s -> [ Param (fakeLocated "") s ]) size
    manyRetTypes =
      sequence
        { start = Token "{" ExpectingLeftBrace
        , separator = Token "," ExpectingComma
        , end = Token "}" ExpectingRightBrace
        , spaces = sps
        , item = param
        , trailing = Forbidden
        }
  in
  succeed identity
    |. token (Token "->" ExpectingArrow)
    |. sps
    |= (located <| oneOf
      [ manyRetTypes
      , singleRetType
      ]
    )


params : HdlParser (List Param)
params =
  loop [] <| \revParams -> oneOf
    [ succeed (\p -> Loop (p :: revParams))
      |= param
      |. sps
    , succeed ()
        |> map (\_ -> Done (List.reverse revParams))
    ]


param : HdlParser Param
param =
  succeed (\n s1 ->
    case s1 of
      Just s2 ->
        Param n s2
      Nothing ->
        Param n { from = n.from, to = n.to, value = IntSize 1 }
    )
    |= name
    |= optional size


size : HdlParser (Located Size)
size =
  succeed identity
    |. token (Token "[" ExpectingLeftBracket)
    |. sps
    |= (located <| oneOf
      [ map IntSize integer
      , map (\n -> VarSize n.value Nothing) name
      ])
    |. sps
    |. token (Token "]" ExpectingRightBracket)


name : HdlParser (Located String)
name =
  located <| variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = reserved
    , expecting = ExpectingName
    }


sps : HdlParser ()
sps =
  loop 0 <| ifProgress <|
    oneOf
      [ lineComment (Token "--" ExpectingStartOfLineComment)
      , multiComment (Token "{-" ExpectingStartOfMultiLineComment) (Token "-}" ExpectingEndOfMultiLineComment) Nestable
      , spaces
      ]


ifProgress : HdlParser a -> Int -> HdlParser (Step Int ())
ifProgress parser offset =
  succeed identity
    |. parser
    |= getOffset
    |> map (\newOffset -> if offset == newOffset then Done () else Loop newOffset)


fakeLocated : a -> Located a
fakeLocated value =
  { from = (-1, -1)
  , to = (-1, -1)
  , value = value
  }