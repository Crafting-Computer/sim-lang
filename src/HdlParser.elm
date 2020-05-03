module HdlParser exposing (parse, fakeLocated, withLocation, showDeadEnds, showProblemLocation, showProblemLocationRange, bindingTargetToString, Def(..), BindingTarget(..), Located, Expr(..), Param, Size(..))


import Parser.Advanced exposing (..)
import Set exposing (Set)
import AssocList as Dict exposing (Dict)
import List.Extra
import EverySet


type Def
  = FuncDef
    { name : Located String
    , params : List Param
    , outputs : Located (List Param)
    , locals : List Def
    , body : Located Expr
    }
  | BindingDef
    { name : Located BindingTarget
    , locals : List Def
    , body : Located Expr
    , size : Maybe (Located Size)
    }


type BindingTarget
  = BindingName String
  | BindingRecord (Dict (Located String) (Located String))


type Expr
  = Binding (Located String)
  | Call (Located String) (List (Located Expr))
  | Indexing (Located Expr) (Located Int, Located Int)
  | Record (Located (Dict (Located String) (Located Expr)))
  | BusLiteral (Located (List (Located Expr)))
  | IntLiteral (Located Int)


type alias Param =
  { name : Located String
  , size : Located Size
  }


type Size
  = IntSize Int
  | VarSize String


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
  | ExpectingSpaces


type Context
  = BindingDefContext (Located BindingTarget)
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
  run (succeed identity |= withIndent 0 defs |. end ExpectingEOF) src


showDeadEnds : String -> List (DeadEnd Context Problem) -> String
showDeadEnds src deadEnds =
  let
    deadEndGroups =
      List.Extra.groupWhile (\d1 d2 -> d1.row == d2.row && d1.col == d2.col) <| deadEnds
  in
  String.join "\n" <| List.map (showDeadEndsHelper src) deadEndGroups


showDeadEndsHelper : String -> ((DeadEnd Context Problem), List (DeadEnd Context Problem)) -> String
showDeadEndsHelper src (first, rests) =
  let
    location =
      showProblemLocation first.row first.col src
    problems =
      List.map (.problem >> showProblem) <| EverySet.toList <| EverySet.fromList <| first :: rests
    context =
      showProblemContextStack first.contextStack
  in
  location ++ "\n"
  ++ "I'm expecting " ++ String.join " or " problems
  ++ (if String.isEmpty context then "" else " in the " ++ context)
  ++ "."


showProblemContextStack : List { row : Int, col : Int, context : Context } -> String
showProblemContextStack contexts =
  String.join " of the " <| List.map (.context >> showProblemContext) contexts


showProblemContext : Context -> String
showProblemContext context =
  case context of
    BindingDefContext bindingName ->
      let
        nameStr =
          bindingTargetToString bindingName.value
      in
      "`" ++ nameStr ++ "`" ++ " definition"
    FuncDefContext funcName ->
      "`" ++ funcName.value ++ "`" ++ " definition"
    LocalsContext ->
      "local definitions"


bindingTargetToString : BindingTarget -> String
bindingTargetToString target =
  case target of
    BindingName n ->
      n
    BindingRecord r ->
      "{ "
      ++ (String.join ", " <|
        List.map
          (\(k, v) ->
            k.value ++ " : " ++ v.value
          )
          (List.reverse <| Dict.toList r)
      )
      ++ " }"


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
      makeUnderline line offsettedCol (offsettedCol + 1)
  in
  line ++ "\n" ++ underline


showProblemLocationRange : Int -> Int -> Int -> Int -> String -> String
showProblemLocationRange startRow startCol endRow endCol src =
  String.join "\n" <|
  List.map
  (\row ->
    let
      rawLine =
        getLine row src
      line =
        String.fromInt row ++ "| " ++ (String.trimLeft <| rawLine)
      offset =
        String.length line - String.length rawLine - 1
      underlineStartCol =
        if row == startRow then
          offset + startCol
        else
          1
      underlineEndCol =
        if row == endRow then
          offset + endCol
        else
          String.length line
      underline =
        makeUnderline line underlineStartCol underlineEndCol
    in
    line ++ "\n" ++ underline
  )
  (List.range startRow endRow)


makeUnderline : String -> Int -> Int -> String
makeUnderline row minCol maxCol =
  String.toList row
    |> List.indexedMap (\i _ -> toUnderlineChar minCol maxCol i)
    |> String.fromList


toUnderlineChar : Int -> Int -> Int -> Char
toUnderlineChar minCol maxCol col =
  if minCol <= col && col < maxCol then
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
      "a '('"
    ExpectingRightParen ->
      "a ')'"
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
    ExpectingSpaces ->
      "a space or newline"


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
  checkIndent
  <| (succeed identity
    |= oneOf
      [ backtrackable <| map (\n -> withLocation n <| BindingName n.value) name
      , succeed (\locatedList -> withLocation locatedList <|
        BindingRecord <|
          Dict.fromList locatedList.value
      )
      |= located (sequence
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
        })
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
          |= located expr
        )
    )
  )


funcDef : HdlParser Def
funcDef =
  checkIndent
  <| (succeed identity
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
          |= located expr
        )
    )
  )


checkIndent : HdlParser a -> HdlParser a
checkIndent parser =
  succeed (\indentation column ->
    (parser, indentation < column, column - 1)
    )
    |= getIndent
    |= getCol
    |> andThen checkIndentHelp


checkIndentHelp : (HdlParser a, Bool, Int) -> HdlParser a
checkIndentHelp (parser, isIndented, actualIndentation) =
  if isIndented then
    withIndent actualIndentation parser
  else
    problem ExpectingIndent


locals : HdlParser (List Def)
locals =
  checkIndent
  <| (inContext LocalsContext <|
  succeed identity
    |. keyword (Token "let" ExpectingLet)
    |. sps
    |= (indent <|
      succeed identity
        |= lazy (\_ -> defs)
        |. sps
    )
    |. (checkIndent
    <| keyword (Token "in" ExpectingIn)
    )
  )


indent : HdlParser a -> HdlParser a
indent parser =
  succeed (\indentation ->
    indentation + 1
  )
  |= getIndent
  |> andThen (\newIndentation ->
    withIndent newIndentation parser
  )


expr : HdlParser Expr
expr =
  oneOf
    [ group
    , bindingOrCall
    , record
    , busLiteral
    , intLiteral
    ]


record : HdlParser Expr
record =
  checkIndent <|
  succeed (\locatedList -> Record <|
    withLocation locatedList <|
      Dict.fromList locatedList.value
    )
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
        |= (located <| oneOf
          [ group
          , bindingOrCall
          , intLiteral
          , busLiteral
          ]
        )
    , trailing = Forbidden
    }
    )


busLiteral : HdlParser Expr
busLiteral =
  checkIndent <|
  succeed BusLiteral
    |= ( located <| sequence
    { start = Token "[" ExpectingLeftBracket
    , separator = Token "," ExpectingComma
    , end = Token "]" ExpectingRightBracket
    , spaces = sps
    , item =
      succeed identity
        |= (located <|
          oneOf
            [ group
            , bindingOrCall
            , intLiteral
            ]
        )
    , trailing = Forbidden
    }
    )


intLiteral : HdlParser Expr
intLiteral =
  let
    invalid = 
      ExpectingInt
    expecting =
      ExpectingInt
  in
  checkIndent <|
  map IntLiteral <| located <|
    number
    { int = Ok identity
    , hex = Ok identity
    , octal = Err invalid
    , binary = Ok identity
    , float = Err invalid
    , invalid = invalid
    , expecting = expecting
    }


group : HdlParser Expr
group =
  checkIndent <|
  succeed (\g i ->
    case i of
      Just indexes ->
        Indexing g indexes
      Nothing ->
        g.value
    )
    |. token (Token "(" ExpectingLeftParen)
    |. sps
    |= lazy (\_ -> located expr)
    |. sps
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
  checkIndent <|
  succeed (\n i ->
    case i of
      Just indexes ->
        Indexing (withLocation n <| Binding n) indexes
      Nothing ->
        Binding n
    )
    |= name
    |= optional indexing


bindingOrCall : HdlParser Expr
bindingOrCall =
  checkIndent <|
  ( succeed Tuple.pair
    |= name
    |=  ( loop [] <| \revExprs ->
      oneOf
        [ succeed (\n -> Loop (n :: revExprs))
          |. backtrackable sps1
          |= (located <| oneOf
            [ binding
            , group
            , record
            , intLiteral
            ]
          )
        , succeed ()
          |> map (\_ -> Done (List.reverse revExprs))
        ]
    )
    |> andThen
      (\(callee, args) ->
        case args of
          [] ->
            succeed
              (\i -> case i of
                Just indexes ->
                  Indexing (withLocation callee <| Binding callee) indexes
                Nothing ->
                  Binding callee
              )
              |= optional indexing
          list ->
            succeed (Call callee list)
      )
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
      , map (\n -> VarSize n.value) name
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


sps1 : HdlParser ()
sps1 =
  getChompedString sps
  |> andThen (\str ->
    if String.isEmpty str then
      problem ExpectingSpaces
    else
      succeed ()
  )


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


withLocation : Located a -> b -> Located b
withLocation loc value =
  { from =
    loc.from
  , to =
    loc.to
  , value =
    value
  }