# elm-hdl
Elm-like Hardware Description Language

```elm
{- sample full adder -}

full_adder a b c -> { sum, carry } =
  let
    { sum = s1, carry = c1 } = half_adder a b
    { sum = s2, carry = c2 } = half_adder s1 c
    c3 = or c1 c2
  in
  { sum = s2, carry = c3 }

half_adder a b -> { sum, carry } =
  let
    sum = xor a b
    carry = and a b
  in
  { sum = sum, carry = carry }

xor a[n] b[n] -> [n] = -- [n] specifies a variable sized bus
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
```

# Development
Run editor:
```
elm-live src/Editor.elm --start-page public/index.html -- --output=public/elm.js
```

# License
MIT