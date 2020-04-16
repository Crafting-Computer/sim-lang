module Main exposing (main)

import Html exposing (div, p, pre, text)
import HdlParser exposing (parse)
import HdlChecker exposing (check)


-- MAIN


source =
  -- "nand_a_b = nand a b\n"
--   """
-- and1 a[1] b[1] -> out[1] =
--   let
--     nand_a_b = nand a b[0..2]
--     c =
--       nand
--       a
--       b
--   in
--   nand nand_a_b[0] nand_a_b

-- not a[1] -> out[1] =
--   nand a a
--   """
  """
xor a[n] b[n] -> [n] =
  and a b

and a[n] b[n] -> [n] =
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
      let
        _ = Debug.log "program" program
      in
      div []
        [ pre [] [ text source]
        , p [] [ text (Debug.toString program) ]
        , p [] [ text (Debug.toString <| check program) ]
        ]
