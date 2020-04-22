module Main exposing (main)

import Html exposing (div, p, pre, text)
import HdlParser exposing (parse, showDeadEnds)
import HdlChecker exposing (check)
import HdlEmitter exposing (emitString)


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
  -- """nand2 a[n] b[n] -> [n] =
  -- let
  --   nand_in_another_name a[n] -> [n] = nand a b
  -- in
  -- nand_in_another_name a
  -- """
  """
recursive =
    let
      b = c
      c = b
    in
    c
  """
  -- "half_adder a b -> { sum, carry } =\n  let\n    sum = xor a b\n    carry = and a b\n  in\n  { sum = sum, carry = carry }"
  -- """
  -- """
  -- not a -> [1] =
  --   nand a a
  -- """
main =
  case parse source of
    Err err ->
      div []
        [ pre [] [ text source]
        , pre [] [ text <| 
          "❌ Parse error.\n\n"
          ++ showDeadEnds source err
        ]
        ]

    Ok program ->
      div []
        [ pre [] [ text source]
        , pre [] [ text "✔️ Passed parser." ]
        , pre [] [ text (
          case check program of
            Ok _ ->
              "✔️ Passed checker.\n\n"
              ++ "🏭 Generated JS code:\n\n"
              ++ emitString program
            Err problems ->
              "❌ Check error.\n"
              ++ HdlChecker.showProblems source problems
        )]
        ]
