module Main exposing (main)

import Html exposing (div, p, pre, text)
import HdlParser exposing (parse, showDeadEnds)
import HdlChecker exposing (check)
import HdlEmitter exposing (emitString)


-- MAIN


source =
  -- "nand_a_b = nand a b\n"
  -- """and a[1] b[1] -> [1] =
  -- let
  --     nand_a_b = nand a b
  -- in
  -- nand nand_a_b nand_a_b
  -- """
--   """{- sample full adder -}

-- full_adder a b c -> { sum, carry } =
--     let
--         { sum = s1, carry = c1 } = half_adder a b
--         { sum = s2, carry = c2 } = half_adder s1 c
--         c3 = or c1 c2
--     in
--     { sum = s2, carry = c3 }

-- half_adder a b -> { sum, carry } =
--     let
--         sum = xor a b
--         carry = and a b
--     in
--     { sum = sum, carry = carry }

-- xor a[n] b[n] -> [n] = -- [n] specifies a variable sized bus
--     let
--         nand_a_b = nand a b
--     in
--     nand
--     (nand a nand_a_b)
--     (nand b nand_a_b)

-- or a[n] b[n] -> [n] =
--     nand (not a) (not b)

-- not a[n] -> [n] =
--     nand a a

-- and a[n] b[n] -> [n] =
--     let
--         nand_a_b = nand a b
--     in
--     nand nand_a_b nand_a_b
--     """
  -- """nand2 a[n] b[n] -> [n] =
  -- let
  --   nand_in_another_name a[n] -> [n] = nand a b
  -- in
  -- nand_in_another_name a
  -- """
  -- "not a[2] -> [1] = nand a a"
  -- """
  -- not a[1] -> [1] = nand a a
  -- using_not a[2] -> [2] = not a
  -- """ -- TODO
  -- "not a[n] -> [1] = nand a a"
  -- "not a[2] -> [1] = nand a a"
  -- "or a b -> [1] = nand (not a) (not b)"
  -- "combine a b -> { a, b } = { a = a, c = b }"
  -- "f i -> [1] = let a = nand 0 0 0 in i"
  -- "head bus[n] -> [1] = bus[0]"
  -- "head bus[16] -> [1] = bus[0]"
  -- "head bus[2] -> [1] = bus[2]"
  -- "first4 bus[3] -> [4] = bus[3..4]"
  -- """
  -- first bus[3] -> [1] =
  --   let
  --     one = bus[0]
  --     two = bus[1]
  --     three = bus[3]
  --   in
  --   three
  -- """
  -- "first4 bus[3] -> [4] = bus[0..3]"
  -- "f i -> [1] =\n let\n  a = a\n  in\n i"
--   """
-- recursive i -> [1] =
--     let
--         b = c
--         d = b
--         c = d
--     in
--     nand i c
--   """
  -- """
  -- s_r_latch s r -> [1] =
  --     let
  --         nar a b -> [1] = nand a b
  --         q =
  --             nand s not_q
  --         not_q =
  --             nand r q
  --     in
  --     q
  -- """
--   """f i[1] -> [3] =
--     let
--         bus =
--             [ 0, 1, { a = 2 }, 2 ]
--     in
--     bus
--   """
    "f a[1] b[2] c[1] -> [4] =\n let\n  bus = [1, a, 1, b]\n in\n bus"
  -- """
  -- f i -> [1] =
  --   let
  --     a = a
  --   in
  --   i
  -- """
  -- "not a[2] -> [1] = nand a a"
--   """not a -> [1] = nand a a
-- using_not a[2] -> [2] = not a
--   """
  -- "not a -> [1] = nand a b"
  -- "half_adder a b -> { sum, carry } =\n  let\n    sum = xor a b\n    carry = and a b\n  in\n  { sum = sum, carry = carry }"
  -- """
  -- """
  -- not a -> [1] =
  --   nand a a
  -- """
  -- """
  -- f i -> [1] =
  --   let
  --     b = c
  --     a = b
  --     b = a
  --     c = 1
  --   in
  --   a
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
