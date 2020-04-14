module HdlParser exposing (parse, Program)

import Parser.Advanced exposing (..)
import Set exposing (Set)


type alias Program =
  List Def


type Def
  = FuncDef
    { name : Located String
    , params : List Param
    , retSize : Located Int
    , locals : List Def
    , body : Expr
    }
  | BindingDef
    { name : Located String
    , locals : List Def
    , body : Expr
    }


type Expr
  = Binding (Located String)
  | Call (Located String) (List Expr)
  | Indexing Expr (Int, Int)


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


type Context
  = DefContext


type alias HdlParser a =
  Parser Context Problem a


type alias Located a =
  { start : (Int, Int)
  , value : a
  , end : (Int, Int)
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
      |= def
      |. sps
    , succeed ()
      |> map (\_ -> Done (List.reverse revDefs))
    ]


def : HdlParser Def
def =
  succeed
    (\defName defHeader (defLocals, defBody) ->
      let
        _ = Debug.log "AL -> defName" <| defName
      in
      case defHeader of
        Just (defParams, defRetSize) ->
          FuncDef { name = defName
          , params = defParams
          , retSize = defRetSize
          , locals = Maybe.withDefault [] defLocals
          , body = defBody
          }
        Nothing ->
          BindingDef { name = defName
          , locals = Maybe.withDefault [] defLocals
          , body = defBody
          }
    )
    |. checkIndent
    |= name
    |. sps
    |= (optional <|
      succeed Tuple.pair
        |= params
        |= retSize
      )
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
    ]


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


retSize : HdlParser (Located Int)
retSize =
  succeed identity
    |. token (Token "->" ExpectingArrow)
    |. sps
    |. token (Token "[" ExpectingLeftBracket)
    |. sps
    |= (located <| integer)
    |. sps
    |. token (Token "]" ExpectingRightBracket)


params : HdlParser (List Param)
params =
  loop [] <| \revParams -> oneOf
    [ succeed (\p -> Loop (p :: revParams))
      |= (
        succeed Param
          |= name
          |. token (Token "[" ExpectingLeftBracket)
          |. sps
          |= (located <| integer)
          |. sps
          |. token (Token "]" ExpectingRightBracket)
      )
      |. sps
    , succeed ()
        |> map (\_ -> Done (List.reverse revParams))
    ]


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