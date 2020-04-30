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
> Note: line comments in Sim starts with `--`.
> Multi-line comments starts with `{-` and ends with `-}`.

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

|a|b|a and b|
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

Now since `a and b` is equivalent to `not (a nand b)`, we can implement the body of the `and` gate as:
```elm
not (nand a b)
```
Combining the header and the body, we got:
```elm
and a[1] b[1] -> [1] =
    not (nand a b)
```

## Implementing Or Gate
Following the same procedure for implementing `and` gate, we start by specifying the interface of the `or` gate.

Looking at the truth table of `or`, we notice that it expects two inputs `a` and `b` and produces a single output.

|a|b|a or b|
|-|-|---|
|0|0|0|
|0|1|1|
|1|0|1|
|1|1|1|

So we can declare the header of the `or` gate as:
```elm
or a[1] b[1] -> [1]
```
Not bad!

Using De Morgan's laws, we can rewrite `a or b` as `not ((not a) and (not b))`. Since `not (_ and _)` is equivalent to `_ nand _`, we can further rewrite `a or b` as `(not a) nand (not b)`. We can implement the body of the `or` gate as:
```elm
nand (not a) (not b)
```
Combining the header and the body, we got:
```elm
or a[1] b[1] -> [1] =
    nand (not a) (not b)
```

## Implementing Xor Gate
Following the same procedure for implementing `or` gate, we start by specifying the interface of the exclusive or gate.

Looking at the truth table of `xor`, we notice that it expects two inputs `a` and `b` and produces a single output.

|a|b|a xor b|
|-|-|---|
|0|0|0|
|0|1|1|
|1|0|1|
|1|1|0|

So we can declare the header of the `xor` gate as:
```elm
xor a[1] b[1] -> [1]
```
Not bad!

Now since `a xor b` is equivalent to `((not a) and b) or (a and (not b))`, we can implement the body of the `xor` gate as:
```elm
or (and (not a) b) (and a (not b))
```
We can finish the implementation right here and combine the header and the body as usual. However, the logic is getting cluttered in a single line. We can improve the readability by using the `let` expression:
```elm
let
    first = and (not a) b
    second = and a (not b)
in
or first second
```
`let` expression allows us to give descriptive names to subexpressions. It is esepcially useful when the expression is becoming to cluttered or we wish to reuse certain expressions multiple times. As you will see later, `let` expression also reduce repeated code, boost performance, and reduce the complexity and cost of circuits.

Combining the header and the body, we got:
```elm
xor a[1] b[1] -> [1] =
    let
        first = and (not a) b
        second = and a (not b)
    in
    or first second
```

## Optimizing Xor Gate
If we just care about creating a `xor` gate that works, we can happily wrap up here. However, if we want to produce the circuit as a part of a real computer, we need to worry about both cost and efficiency. Two most basic ways to assess cost and efficiency is by calculating the total number of logic gates used and the amount of gate delays.

### Decrease Gate Counts
First, let's count the number of gates used.
> The reason we care is because `xor` gate is a foundamental logic gate that's used in adders and many other circuits. We will likely end up using tens if not hundreds of `xor` gate in our CPU so even reducing the subgate count by one or two can save us loads of extraneous gates, silicon space, and production cost.

Counting gates in Sim is easy, we just need to track the number of function calls like so:
```elm
xor a[1] b[1] -> [1] =
    let
        first = and (not a) b -- 2 calls: 1 and, 1 not
        second = and a (not b) -- 2 calls: 1 and, 1 not
    in
    or first second -- 1 call: 1 or
```
Adding up the number of calls in the body of `xor` got us `2 + 2 + 1 = 5`. So it seems that we used `5` logic gates to construct the `xor` gate. However, don't forget that each `not` and `and` gate is composed of one or several `nand` gates. Indeed, if we look up our definition for `not`:
```elm
not a[1] -> [1] =
    nand a a
```
We used one `nand` gate so the number of `nand` gates remains the same.
If we look up our definition for `and`:
```elm
and a[1] b[1] -> [1] =
    not (nand a b)
```
We used one `not` gate and one `nand` gate, so `1 + 1 = 2` nand gates in total for our `and` gate.

Looking up our definition for `or` gate:
```elm
or a[1] b[1] -> [1] =
    nand (not a) (not b)
```
We used `2` not gates and `1` nand gate in the implementation, so `2 + 1 = 3` nand gates in total for our `or` gate.

Now let's redo our calculation for `xor`. Based on our previous analysis, we used `2` and gates, `2` not gates, and `1` or gate in the implementation. So it's `2 (and) * 2 (nand) + 2 (not) * 1 (nand) + 1 (or) * 3 (nand) = 4 + 2 + 3 = 9` nand gates.

Alright, let's attempt to optimize the `xor` gate. One thing we can try is to expand the definition to using only nand gates and see some share parts that can be reused. We will use a more concise syntax from now on to represent common logic gates like `not` and `and`. Here's a table for your reference:
|gate|symbol|precedence|
|:--:|:----:|:--------:|
|not |-a    |0|
|and |a * b |1|
|or  |a + b |2|
> Note: The symbols are borrowed from decimal number algebra to boolean algebra. They are similar algebraic properties and operator precedence. Also, smaller number means higher precedence.

In the beginning we came up with a definition for `xor` like so:
```
((not a) and b) or (a and (not b))
```
Now we can make it cleaner by using algebraic symbols:
```
-a*b + a*-b
```

The following expansion steps require knowledge of boolean algebra. We show them just to illustrate how optimization may look like but don't expect you to conduct optimization like this yourself.

```
-a*b + a*-b
-(-(-a*b + a*-b))
-(-(-a*b) * -(a*-b))
-(-a*b) NAND -(a*-b)
-(-a*b + -b*b) NAND -(a*-b + a*-a)
-(b*(-a + -b)) NAND -(a*(-b + -a))
(b NAND (-a + -b)) NAND (a NAND (-a + -b))
(b NAND -(a * b)) NAND (a NAND -(a * b))
(b NAND (a NAND b)) NAND (a NAND (a NAND b))
```
Now you may notice we have a shared subexpression `-a + -b`. We can express this optimized version in Sim:
```elm
xor a[1] b[1] -> [1] =
    let
        nand_a_b = nand a b
        first = nand b nand_a_b
        second = nand a nand_a_b
    in
    nand first second
```
We can easily see that this version uses only `4` nand gates which saves `5` gates compared to our original version!

### Decrease Gate Delays
Gate delays is a rough way to calculate the execution speed of a given circuit.
> The reason we care about gate delays is because even though we can imagine that electric signal float instantly from input pins to output pins, it takes a small amount of time for the electrons to actually travel through the gates and wires. While saving a few nanoseconds in one gate does not sound very exciting, saving millions of nanoseconds in millions of gates making up a computer is very crucial to performance.

Let's start by calculating the gate delays of the `not` gate.

#### Not Gate Delays
```elm
not a[1] -> [1] =
    nand a a
```

![not_gate_delays](/media/not_gate_delays.png)

Notice that any electric signal passes through `1` nand gate before reaching the output light bulb. This implies `1` gate delay.

#### And Gate Delays
```elm
and a[1] b[1] -> [1] =
    not (nand a b)
```

![and_gate_delays](/media/and_gate_delays.png)

Notice that any electric signal passes through `2` nand gates before reaching the output light bulb. This implies `2` gate delays.

#### Or Gate Delays
```elm
or a[1] b[1] -> [1] =
    nand (not a) (not b)
```

![or_gate_delays](/media/or_gate_delays.png)

Notice that any electric signal passes through `2` nand gates before reaching the output light bulb. This implies `2` gate delays.

Finally, we can calculate the gate delays of our original `xor` gate:
```elm
xor a[1] b[1] -> [1] =
    let
        first = and (not a) b
        second = and a (not b)
    in
    or first second
```
![xor_gate_delays](/media/xor_gate_delays.png)

Notice that any electric signal passes through
  * `1` not gate (1 gate delay)
  * `1` and gate (2 gate delays)
  * `1` or gate (2 gate delays)

before reaching the output light bulb. This implies `1 + 2 + 2 = 5` gate delays.

Now let's calculate the gate delays of our optimized `xor` gate from the previous section:
```elm
xor a[1] b[1] -> [1] =
    let
        nand_a_b = nand a b
        first = nand b nand_a_b
        second = nand a nand_a_b
    in
    nand first second
```
![xor_optimized_gate_delays](/media/xor_optimized_gate_delays.png)

Notice that any electric signal passes through `3` nand gates before reaching the output light bulb. This implies `3` gate delays. Compared to our original implementation, the optimized version saves us  `2` gate delays!

**In conclusion, our optimization saves us 5 gates and 2 gate delays. This means half the cost and almost double the performance!**

## Implementing Mux
Since you should be familiar with Sim by now, we will use the multiplexer's truth table to directly derive its implementation.

| a | b |sel|result|
|:-:|:-:|:-:|:-----:|
|0|0|0|0|
|0|0|1|0|
|0|1|0|0|
|0|1|1|1|
|1|0|0|1|
|1|0|1|0|
|1|1|0|1|
|1|1|1|1|

Simply put, the multiplexer outputs the value of `a` when `sel = 0` and outputs the value of `b` when `sel = 1`. So we can condense our truth table to be:

| a | b |sel|result|
|:-:|:-:|:-:|:-----:|
|a|b|0|a|
|a|b|1|b|

Now we have our truth table, we can derive the implementation like so:

```elm
mux a[1] b[1] sel[1] -> [1] =
    let
        sel_a =
            and (not sel) a
        sel_b =
            and sel b
    in
    or sel_a sel_b
```

Let's check for optimizations:

```
a*-sel + b*sel
-(-(a*-sel + b*sel))
-(-(a*-sel) * -(b*sel))
(a NAND -sel) NAND (b NAND sel)
```

Indeed, we can save `2` gates by switching from `or` to `nand` and still `2` more gates by switching from `and` to `nand`. Here's the optimized version:

```elm
mux a[1] b[1] sel[1] -> [1] =
    let
        sel_a =
            nand (not sel) a
        sel_b =
            nand sel b
    in
    nand sel_a sel_b
```

Since we use 1 bit input pins quite often, we can omit the `[1]` and Sim will assume it's 1 bit:

```elm
mux a b sel -> [1] =
    let
        sel_a =
            nand (not sel) a
        sel_b =
            nand sel b
    in
    nand sel_a sel_b
```

Here's a problem though, what if we want to select between two values that are 2 bits, 8 bits, or 32 bits? Do we have to write a separate `mux2`, `mux8`, and `mux16` even though the logic for any bit is the same? What if we can declare a general `mux` function that works for any sized input pins like so where `n` can be any positive number?

```elm
mux a[n] b[n] sel -> [1]
```

But this requires our `nand` and `not` gate to handle arbitrary sized inputs as well... Do we need to reimplement all the previous circuits? The answer is you just need to change `[1]` to `[n]` and you are done!

So here's our updated `not` gate:
```elm
not a[n] -> [n] =
    nand a a
```

What about `nand` gate? Sim has already done it for us if you check out `nand`'s header:

```elm
nand a[n] b[n] -> [n]
```

Perfect! Now let's generalize our `mux` to n bits:

```elm
mux a[n] b[n] sel -> [n] =
    let
        sel_a = nand (not sel) a
        sel_b = nand sel b
    in
    nand sel_a sel_b
```

The above does not work surprisingly as Sim complains:

```
I'm expecting to find the type [n] here:
1| mux a[n] b[n] sel -> [n] =
                 ^^^          
but got the type [1].
```

Why does Sim tell us this message? Let's check the place we used `sel`:

```elm
sel_a = nand (not sel) a
```

and

```elm
sel_b = nand sel b
```

Notice that `sel` is passed in as an argument to the `nand` function which expects both its input to have a size of `n`. However, `sel` has a fixed size of `1`. It will be neat if we can expand a 1-bit value into multiple bits by copying its value multiple times. That is exactly what the built-in function `fill` does:

```elm
fill a[1] -> [n]
```

Finally, we can finish our generalized implementation of `mux` by expanding `sel` to size `n`:


```elm
mux a[n] b[n] sel -> [n] =
    let
        sel_a = nand (not (fill sel)) a
        sel_b = nand (fill sel) b
    in
    nand sel_a sel_b
```

Before we move on, we need to understand a little caveat of `Sim`. Currently, you can only use one bus size variable `n` and it acts as a global integer constant. So the below implementation will not work:

```elm
mux a[n] b[n] sel -> [n] =
    let
        sel_a = nand (fill (not sel)) a
        sel_b = nand (fill sel) b
    in
    nand sel_a sel_b
```

Because `fill` requires its argument to be of size `1`, meaning that `not` takes in an argument of size `1`. If you look at `not`'s header:

```elm
not a[n] -> [n] =
    nand a a
```

You will see that we just implied that `n = 1` in the `not` function. Since the `n` value is global, we implied that all `n = 1` which is not what we want. This is not a problem you should concern about as it's a problem that `Sim` needs to solve. Just watch out for weired bugs like this in the future.

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
sed -i 's+src="elm.js"+src="/public/elm.js"+' public/index.html
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

# Change Log
## Release v0.1.0
* Store units in localStorage
* Can remove tabs
* Fix parse error underlining