module Main exposing (main)

import Html exposing (div, p, pre, text)
import HdlParser exposing (parse)


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
xor a b -> out =
  let
    nand_a_b = nand a b
  in
  nand
  (nand a nand_a_b)
  (nand b nand_a_b)  

half_adder a b -> { sum, carry } =
  let
    sum = xor a b
    carry = and a b
  in
  { sum = sum, carry = carry }

full_adder a b c -> { sum, carry } =
  let
    { sum = s1, carry = c1 } = half_adder a b
    { sum = s2, carry = c2 } = half_adder s1 c
    c3 = or c1 c2
  in
  { sum = s2, carry = c3 }
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
