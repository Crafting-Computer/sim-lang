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
  """{-
{a, b, c, d, e, f, g, h} = {input, 0, 0, 0, 0, 0, 0, 0} if sel == 000
                           {0, input, 0, 0, 0, 0, 0, 0} if sel == 001
                           etc.
                           {0, 0, 0, 0, 0, 0, 0, input} if sel == 111
-}
dmux_8_way input[n] sel[3] ->
    { a[n], b[n], c[n], d[n], e[n], f[n], g[n], h[n] } =
    let
        { a = a, b = b, c = c, d = d } =
            dmux_4_way input sel[0..1]
        { a = e, b = f, c = g, d = h } =
            dmux_4_way input sel[0..1]
        sel_when_0 output[n] -> [n] =
            and output (not (fill sel[2]))
        sel_when_1 output[n] -> [n] =
            and output (fill sel[2])
    in
    { a = sel_when_0 a
    , b = sel_when_0 b
    , c = sel_when_0 c
    , d = sel_when_0 d
    , e = sel_when_1 e
    , f = sel_when_1 f
    , g = sel_when_1 g
    , h = sel_when_1 h
    }

dmux_4_way_test sel[2] -> { a[1], b[1], c[1], d[1] } =
    dmux_4_way 1 sel

{-
{a, b, c, d} = {input, 0, 0, 0} if sel == 00
               {0, input, 0, 0} if sel == 01
               {0, 0, input, 0} if sel == 10
               {0, 0, 0, input} if sel == 11
-}
dmux_4_way input[n] sel[2] -> { a[n], b[n], c[n], d[n] } =
    let
        { a = a, b = b } =
            dmux input sel[0]
        { a = c, b = d } =
            dmux input sel[0]
    in
    { a = and a (not (fill sel[1]))
    , b = and b (not (fill sel[1]))
    , c = and c (fill sel[1])
    , d = and d (fill sel[1])
    }

dmux_test sel[1] -> { a[1], b[1] } =
    dmux 1 sel

{-
{a, b} = {input, 0} if sel == 0
         {0, input} if sel == 1
-}
dmux input[n] sel[1] -> { a[n], b[n] } =
    let
        a =
            and input (not (fill sel))
        b =
            and input (fill sel)
    in
    { a = a, b = b }

mux_4_way_test i[1] -> { r1[4], r2[1] } =
    let
        r1 =
            mux_4_way 10 11 12 13 0b01
        r2 =
            mux_4_way 0 1 0 0 0b01
    in
    { r1 = r1, r2 = r2 }

mux_4_way a[n] b[n] c[n] d[n] sel[2] -> [n] =
    let
        sel_a_b =
            mux a b sel[0]
        sel_c_d =
            mux c d sel[0]
    in
    mux sel_a_b sel_c_d sel[1]

mux a[n] b[n] sel[1] -> [n] =
    let
        sel_a =
            and (not (fill sel)) a
        sel_b =
            and (fill sel) b
    in
    or sel_a sel_b

xor a[n] b[n] -> [n] =
    let
        nand_a_b = nand a b
    in
    nand
    (nand a nand_a_b)
    (nand b nand_a_b)

or a[n] b[n] -> [n] =
    nand (not a) (not b)

not a[n] -> [n] =
    nand a a

and a[n] b[n] -> [n] =
    let
        nand_a_b = nand a b
    in
    nand nand_a_b nand_a_b
  """
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
