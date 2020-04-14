module HdlParser exposing (parse, Program)

import Parser.Advanced exposing (..)
import Set exposing (Set)
import AssocList as Dict exposing (Dict)


type alias Program =
  List Def


type Def
  = FuncDef
    { name : Located String
    , params : List Param
    , retType : List Param
    , locals : List Def
    , body : Expr
    }
  | BindingDef
    { name : BindingTarget
    , locals : List Def
    , body : Expr
    }


type BindingTarget
  = BindingName (Located String)
  | BindingRecord (Dict (Located String) (Located String))


type Expr
  = Binding (Located String)
  | Call (Located String) (List Expr)
  | Indexing Expr (Int, Int)
  | Record (Dict (Located String) Expr)


type alias Param =
  { name : Located String
  , size : Located Int
  }


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
  = DefContext


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


parse : String -> Result (List (DeadEnd Context Problem)) Program
parse string =
  run (succeed identity |= defs |. end ExpectingEOF) string


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
  succeed
    (\defName (defLocals, defBody) ->
      BindingDef { name = defName
      , locals = Maybe.withDefault [] defLocals
      , body = defBody
      }
    )
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
    |. backtrackable sps
    |. token (Token "=" ExpectingEqual)
    |. sps
    |= (indent <|
        succeed Tuple.pair
        |= optional locals
        |. sps
        |= expr
      )


funcDef : HdlParser Def
funcDef =
  succeed
    (\defName defParams defRetType (defLocals, defBody) ->
      FuncDef
        { name = defName
        , params = defParams
        , retType = defRetType
        , locals = Maybe.withDefault [] defLocals
        , body = defBody
        }
    )
    |. checkIndent
    |= name
    |. sps
    |= params
    |= retType
    |. sps
    |. token (Token "=" ExpectingEqual)
    |. sps
    |= (indent <|
        succeed Tuple.pair
        |= optional locals
        |. sps
        |= expr
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
  succeed (Record << Dict.fromList)
    |= sequence
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


indexing : HdlParser (Int, Int)
indexing =
  succeed (\from to -> Tuple.pair from (Maybe.withDefault from to))
    |. token (Token "[" ExpectingLeftBracket)
    |. sps
    |= integer
    |. sps
    |= (optional <|
      succeed identity
        |. token (Token ".." ExpectingDotDot)
        |. sps
        |= integer
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


retType : HdlParser (List Param)
retType =
  let
    singleRetType =
      succeed (\p-> [ p ])
        |= param
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
    |= oneOf
      [ manyRetTypes
      , singleRetType
      ]


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
  succeed (\n s ->
    case s of
      Just size ->
        Param n size
      Nothing ->
        Param n { from = n.from, to = n.to, value = 1 }
    )
    |= name
    |= optional (
      succeed identity
      |. token (Token "[" ExpectingLeftBracket)
      |. sps
      |= (located <| integer)
      |. sps
      |. token (Token "]" ExpectingRightBracket)
    )


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