# sim-lang
*A delightful language for circuit design inspired by Elm.*

# Sim Tutorial
Sim aims to make circuit design as simple and fun as possible by using intuitive interfaces and powerful abstractions.

**Try the online Sim editor [here](https://alienkevin.github.io/sim-lang/)!**

## Basics

Let's start by looking at how to implement a logical not gate in Sim:

```elm
not a[1] -> [1] =
  nand a a
```

Notice that we first specify the interface of the circuit by declaring:
```elm
not a[1] -> [1]
```
This means that the `not` circuit takes one input pin called `a` that has a size of `1` and produces an output of size `1`.
We can use `not` as follows:
```elm
not_0 = not 0
-- not_0 is now 1
not_1 = not 1
-- not_1 is now 0
```
At first, the way we called `not` may seem odd because you may be used to call syntax like this:
```c
not_0 = not(0)
```
In Sim, to make function calls cleaner, we do not use parentheses around arguments. In addition, we use space instead of comma to separate arguments:
```elm
nand a a
```
instead of
```c
nand(a, a)
```
Now you may also wonder where is `nand` defined. The answer is that unlike the `not` gate which is defined by the user, `nand` gate is a built-in circuit provided by Sim. It's the most fundamental logic gate and we can build all other gates and circuits from the `nand` gate.

## Implementing And Gate
The `not` gate we implemented just now is very simple, let's now check out a more complex circuit - the `and` gate.

We start by specifying the interface of the `and` gate.

Looking at the truth table of `and`, we notice that it expects two inputs `a` and `b` and produces a single output.

|a|b|result|
|-|-|---|
|0|0|0|
|0|1|0|
|1|0|0|
|1|1|1|

So we can declare the header of the `and` gate as:
```elm
and a[1] b[1] -> [1]
```
Great!
Not since `a and b` is equivalent to `not (a nand b)`, we can implement the body of the `and` gate as:
```elm
not (nand a b)
```
Combining the header and the body, we got:
```elm
and a[1] b[1] -> [1] =
  not (nand a b)
```

## Implementing a full adder
```elm
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

xor a[n] b[n] -> [n] =
  let
    nand_a_b = nand a b
  in
  nand
  (nand a nand_a_b)
  (nand b nand_a_b)

or a[n] b[n] -> [n] =
  nand (not a) (not b)

and a[n] b[n] -> [n] =
  not (nand a b)

not a[n] -> [n] =
  nand a a
```

# Development
## Set up

* Follow instructions [here](https://guide.elm-lang.org/install/) to install `Elm` which is powers the Sim compiler and editor.

* Follow instructions [here](https://github.com/wking-io/elm-live) to install `elm-live` which is used for building and running Sim.

## Commands
* Run editor:
```
elm-live src/Editor.elm --start-page public/index.html -- --output=public/elm.js
```

* Build optimized version of editor:
```
./build.sh
```
Note: you may need to enable execution permission before running the command:
```
chmod +x ./build.sh
```

* Run Sim compiler on a source string:
```
elm-live src/Main.elm --start-page debug/index.html -- --output=debug/elm.js
```

# License
MIT