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
    in
    nand nand_a_b nand_a_b
  """
main =
  case parse source of
    Err err ->
      div []
        [ pre [] [ text source]
        , text ("error: " ++ Debug.toString err)
        ]

    Ok program ->
      div []
        [ pre [] [ text source]
        , p [] [ text (Debug.toString program) ]
        ]
