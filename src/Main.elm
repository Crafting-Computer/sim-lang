module Main exposing (main)

import Html exposing (div, p, pre, text)
import HdlParser exposing (parse)


-- MAIN


source =
  -- "nand_a_b = nand a b\n"
  """
and1 a[1] b[1] -> [1] =
  let
    nand_a_b = nand a b
    c = nand a b
  in
  nand nand_a_b nand_a_b

not a[1] -> [1] =
  nand a a
  """
main =
  case parse source of
    Err err ->
      div []
        [ pre [] [ text source]
        , text ("error: " ++ Debug.toString err)
        ]

    Ok program ->
      let
        _ = Debug.log "program" program
      in
      div []
        [ pre [] [ text source]
        , p [] [ text (Debug.toString program) ]
        ]
