# Sim
Sim aims to make circuit design as simple and fun as possible by using intuitive interfaces and powerful abstractions.

**Try the online Sim editor [here](https://alienkevin.github.io/sim-lang/)!**

Table of Contents
=================

   * [Create a Computer from Scratch Using Sim](#create-a-computer-from-scratch-using-sim)
      * [Introduction](#introduction)
      * [Basics](#basics)
      * [And Gate](#and-gate)
      * [Or Gate](#or-gate)
      * [Xor Gate](#xor-gate)
      * [Optimizing Xor Gate](#optimizing-xor-gate)
         * [Decrease Gate Counts](#decrease-gate-counts)
         * [Decrease Gate Delays](#decrease-gate-delays)
            * [Not Gate Delays](#not-gate-delays)
            * [And Gate Delays](#and-gate-delays)
            * [Or Gate Delays](#or-gate-delays)
      * [4-way Or Gate](#4-way-or-gate)
      * [8-way Or Gate](#8-way-or-gate)
      * [16-way Or Gate](#16-way-or-gate)
      * [Mux](#mux)
      * [4-way Mux](#4-way-mux)
      * [8-way Mux](#8-way-mux)
      * [Dmux](#dmux)
      * [4-way Dmux](#4-way-dmux)
      * [8-way Dmux](#8-way-dmux)
      * [Arithmetic Circuits](#arithmetic-circuits)
      * [Half Adder](#half-adder)
      * [Full Adder](#full-adder)
      * [2-bit Full Adder](#2-bit-full-adder)
      * [4-bit Full Adder](#4-bit-full-adder)
      * [8-bit Full Adder](#8-bit-full-adder)
      * [16-bit Adder](#16-bit-adder)
   * [Development](#development)
      * [Set up](#set-up)
      * [Commands](#commands)
   * [License](#license)
   * [Change Log](#change-log)
      * [Release v0.6.0](#release-v060)
      * [Release v0.5.0](#release-v050)
      * [Release v0.4.0](#release-v040)
      * [Release v0.3.0](#release-v030)
      * [Release v0.2.0](#release-v020)
      * [Release v0.1.0](#release-v010)

# Create a Computer from Scratch Using Sim

## Introduction

Even though Sim is very powerful, it's important to realize that Sim is just a language that expresses logic gates and the wires that connect them. My goal of creating Sim is not for you to learn a new language. Instead, we wish that you can express the elegant ideas and logics of electric circuits in a hopefully equally elegant form. Just as a traditional Chinese proverb says: "Languages carry philosophies" (文以載道), ideas and expressions of them are inseparable.

In order to master both the ideas and their expressions, we will guide you through the fundamentals of circuit design by creating a 16-bit computer from scratch. Don't worry too much, we will start simple and slow at first and gradually release the joy of explorations and creations to you after learning the basics.

Here's a roadmap for building our computer:
1. [Logic circuits](#basics)
2. [Arithmetic circuits](#implementing-arithmetic-circuits)
3. Arithmetic Logic Unit
4. Memory
5. Central Processing Unit

We will cover each section in-depth so let's get started!

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

## And Gate
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

## Or Gate
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

## Xor Gate
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
`let` expression allows us to give descriptive names to subexpressions. It is especially useful when the expression is becoming to cluttered or we wish to reuse certain expressions multiple times. As you will see later, `let` expression also reduce repeated code, boost performance, and reduce the complexity and cost of circuits.

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
If we just care about creating a `xor` gate that works, we can happily wrap up here. However, if we want to produce the circuit as a part of a real computer, we need to worry about both cost and efficiency. The two most basic ways to assess cost and efficiency is by calculating the total number of logic gates used and the amount of gate delays.

### Decrease Gate Counts
First, let's count the number of gates used.
> The reason we care is because `xor` gate is a fundamental logic gate that's used in adders and many other circuits. We will likely end up using tens if not hundreds of `xor` gate in our CPU so even reducing the subgate count by one or two can save us loads of extraneous gates, silicon space, and production cost.

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
> Note: The symbols are borrowed from decimal number algebra to boolean algebra. They are similar algebraic properties and operator precedence. Also, a smaller number means higher precedence.

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

## 4-way Or Gate

Sometimes, we need to `or` more than two numbers together. One use case we will see later is checking whether a number equal to zero by `or`ing all its digits. If the result is `1` then we have at least one `1` in the number which makes it not equal to zero. Otherwise, the number only contains `0` as its digits so it must be equal to zero.

Because the truth table is rather big, we will show the gate diagram instead:

![or_4_way horizontal](/media/or_4_way_horizontal.png)

So far, we have seen most logic gates laid out horizontally like the above: inputs on the left and outputs on the right. This layout also suggests that we have four inputs and one output like so:

```elm
or_4_way a[1] b[1] c[1] d[1] -> [1]
```

The problem with the above header is verbosity. While it's manageable to lay out all inputs individually for our 4-way or gate, consider laying out all inputs for a 8-way or 16-way or gate:

8-way Or Gate:
```elm
or_8_way a[1] b[1] c[1] d[1] e[1] f[1] g[1] h[1] -> [1]
```

16-way Or Gate:
```elm
or_16_way a[1] b[1] c[1] d[1] e[1] f[1] g[1] h[1] i[1] j[1] k[1] l[1] m[1] n[1] o[1] p[1] -> [1]
```

It's getting very hard to keep track of the letters and count how many inputs we have specified. In summary, laying out inputs individually for a large number of inputs is a messy strategy which we will avoid most of the time unless there are no other ways. Is there a better way here? For that we need a **bus**, which is a bunch of 1-bit values stringed together. So instead of listing out our four inputs like above:

```elm
or_4_way a[1] b[1] c[1] d[1] -> [1]
```

we will condense them into a 4-bit bus:

```elm
or_4_way input[4] -> [1]
```

See how much cleaner our code becomes? To get the individual bits of our bus `input`, all we need to do is to specify the index we are looking for:

```elm
-- get the 0th index
input[0]

-- get the 1th index
input[1]

-- get the 2nd index
input[2]

-- get the 3rd index
input[3]
```

Here's how a 4-bit number `0101` or `6` in decimals will be laid out:

![bus indices demo](/media/bus_indices.png)

Notice that **the first or the 0th index starts from the right and grow to the left**. In Mathematics, we call the 0th digit the least significant bit and the 3rd digit the most significant bit of a 4-bit binary number. The reason why we start the indices at zero is because:

![bus indices as exponents](/media/bus_indices_as_exponents.png)

The binary number `0101` has a decimal value of:

```
0 * 2^3 + 1 * 2^2 + 0 * 2^1 + 1 * 2^0 = 4 + 2 = 6
```

> Note: From now on, we may refer a binary number as `0b0101` instead of spelling out something like the binary number `0101`. The `0b` is a prefix that signifies that the digits following it is in binary. Similarly, `0x` is a prefix that signifies that the digits following it is in hexadecimal (16-bits). By default, Sim uses base 10 or decimals but you can specify binary numbers by prefixing `0b` and hexadecimal numbers by prefixing `0x`.

Now that we understand what a bus is, we will show a 4-way or gate in bus layout:

![or_r_way bus layout](/media/or_4_way_vertical.png)

We can implement `or_4_way` in Sim like so:

```elm
or_4_way input[4] -> [1] =
    or (or input[0] input[1]) (or input[2] input[3])
```

## 8-way Or Gate

We can follow the same logic as a 4-way or gate to implement an 8-way or gate.

We have been showing you how to implement circuits up till now. It's time for you to experiment a little. Before we set you loose, we have a new concept to cover. It turns out that `input[0]` and the like are not the only way to access the bits of a bus. You can also slice a section of a bus like so:

```elm
-- Create a 3-bit slice from the bus `input`
input[0..2]
```

`input[0..2]` literally means: Get me the bits from index `0` to `2` of the `input` bus. So you will get the bits `input[0]`, `input[1]`, and `input[2]` by using `input[0..2]`.

Here's an example:

```elm
get_first_3_bits input[4] -> [3] =
    input[0..2]
```

Now you have all the needed knowledge to create `or_8_way` yourself!

Here's the header:

```elm
or_8_way input[8] -> [1]
```

If you get stuck, check out the hints:

<details>
<summary>Hints</summary>
Fill in the blanks below:
<pre>
<code>
or_8_way input[8] -> [1] =
    or (____ input[_.._]) (____ input[_.._])
</code>
</pre>
</details>

## 16-way Or Gate

Try implement a 16-way or gate using 8-way or gates.

Here's the header:
```elm
or_16_way input[16] -> [1]
```

<details>
<summary>Hints</summary>
Fill in the blanks below:
<pre>
<code>
or_16_way input[16] -> [1] =
    or (____ input[_.._]) (____ input[_.._])
</code>
</pre>
</details>

## Mux
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

Since we use 1-bit input pins quite often, we can omit the `[1]` and Sim will assume it's 1 bit:

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
        sel_a = nand (fill (not sel)) a
        sel_b = nand (fill sel) b
    in
    nand sel_a sel_b
```

## 4-way Mux

Now that you have learned to how to construct basic logic gates such as `and`, `or`, and `mux`, we will let you carry on the work to create the rest of the computer. Don't panic. We will specify exactly what you need to construct and cover new concepts in details as usual.

Here's the truth table for the 4-way mux:

| a | b | c | d |sel|result|
|:-:|:-:|:-:|:-:|:-:|:-----:|
|a|x|x|x|00|a|
|x|b|x|x|01|b|
|x|x|c|x|10|c|
|x|x|x|d|11|d|

Here's the header for the 4-way mux:
```elm
{-
    4-way multiplexer:
    result = a if sel == 00
             b if sel == 01
             c if sel == 10
             d if sel == 11
-}
mux_4_way a[n] b[n] c[n] d[n] sel[2] -> [n]
```

<details>
<summary>Hints</summary>
Fill in the blanks below:
<pre>
<code>
mux_4_way a[n] b[n] c[n] d[n] sel[2] -> [n] =
    let
        sel_a_b =
            ___________________
        sel_c_d =
            ___________________
    in
    ____ sel_a_b sel_c_d ____
</code>
</pre>
</details>

Now implement the body by yourself and check the truth table of your function with the expected truth table above.

> Note: By default the Sim editor outputs all values in the truth table in decimals. However, we always supply the expected truth table with all values in binary. We will add an option to output in binary in the future.

## 8-way Mux

| a | b | c | d | e | f | g | h |sel|result|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-----:|
|a|x|x|x|x|x|x|x|000|a|
|x|b|x|x|x|x|x|x|001|b|
|x|x|c|x|x|x|x|x|010|c|
|x|x|x|d|x|x|x|x|011|d|
|x|x|x|x|e|x|x|x|100|e|
|x|x|x|x|x|f|x|x|101|f|
|x|x|x|x|x|x|g|x|110|g|
|x|x|x|x|x|x|x|h|111|h|

```elm
{-
    8-way multiplexer:
    result = a if sel == 000
             b if sel == 001
             c if sel == 010
             d if sel == 011
             e if sel == 100
             f if sel == 101
             g if sel == 110
             h if sel == 111
-}
mux_8_way a[n] b[n] c[n] d[n] e[n] f[n] g[n] h[n] sel[3] -> [n]
```

<details>
<summary>Hints</summary>
Fill in the blanks below:
<pre>
<code>
mux_8_way a[n] b[n] c[n] d[n] e[n] f[n] g[n] h[n] sel[3] -> [n] =
    let
        sel_a_b_c_d =
            ___________________
        sel_e_f_g_h =
            ___________________
    in
    ____ sel_a_b_c_d sel_e_f_g_h ____
</code>
</pre>
</details>

## Dmux

Dmux is short for demultiplexer and does the inverse of what mux or multiplexer does. Given an input, the dmux select its output between several paths based on an address.

| input | sel | a | b |
|:-----:|:---:|:-:|:-:|
| input |  0  | input | 0 |
| input |  1  | 0 | input |

```elm
{-
    {a, b} = {input, 0} if sel == 0
             {0, input} if sel == 1
-}
dmux input[n] sel[1] -> { a[n], b[n] }
```

What's that `{ a[n], b[n] }` output? It's our first time seeing a function that returns more than one output. In Mathematics, we usually think of a function as a machine that accepts one or multiple inputs and produces a single output. Single output also makes working with return values a lot easier. However, for our `dmux` function, we clearly need to return two values. What do we do? To contain multiple outputs, we use a **record**. In Sim, a record is a bunch of key-value pairs:

```elm
{ a = 0
, b = 1
, c = 0
}
```

where `a`, `b`, and `c` are keys and `0`, `1`, and `0` are the keys' values.

The great thing about the record is that we always know the names of our many output values, just like we do for our function inputs. Plus, a record is a single value, just like `0` and `input`!

If you have ideas on how to use records and how to implement `dmux`, go ahead and implement it. If you get stuck any time, check out the hints:

<details>
<summary>Hints</summary>
Fill in the blanks below:
<pre>
<code>
dmux input[n] sel[1] -> { a[n], b[n] } =
    let
        a =
            ___________________
        b =
            ___________________
    in
    { a = a, b = b }
</code>
</pre>
</details>

## 4-way Dmux

| input | sel | a | b | c | d |
|:-----:|:---:|:-:|:-:|:-:|:-:|
| input |  00 | input | 0 | 0 | 0 |
| input |  01 | 0 | input | 0 | 0 |
| input |  10 | 0 | 0 | input | 0 |
| input |  11 | 0 | 0 | 0 | input |

```elm
{-
    {a, b, c, d} = {input, 0, 0, 0} if sel == 00
                   {0, input, 0, 0} if sel == 01
                   {0, 0, input, 0} if sel == 10
                   {0, 0, 0, input} if sel == 11
-}
dmux_4_way input[n] sel[2] -> { a[n], b[n], c[n], d[n] }
```

<details>
<summary>Hints</summary>
Fill in the blanks below:
<pre>
<code>
dmux_4_way input[n] sel[2] -> { a[n], b[n], c[n], d[n] } =
    let
        { a = a, b = b } =
            ___________________
        { a = c, b = d } =
            ___________________
    in
    { a = ___________________
    , b = ___________________
    , c = ___________________
    , d = ___________________
    }
</code>
</pre>
</details>

## 8-way Dmux

| input | sel | a | b | c | d | e | f | g | h |
|:-----:|:---:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| input | 000 | input | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| input | 001 | 0 | input | 0 | 0 | 0 | 0 | 0 | 0 |
| input | 010 | 0 | 0 | input | 0 | 0 | 0 | 0 | 0 |
| input | 011 | 0 | 0 | 0 | input | 0 | 0 | 0 | 0 |
| input | 100 | 0 | 0 | 0 | 0 | input | 0 | 0 | 0 |
| input | 101 | 0 | 0 | 0 | 0 | 0 | input | 0 | 0 |
| input | 110 | 0 | 0 | 0 | 0 | 0 | 0 | input | 0 |
| input | 111 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | input |

```elm
{-
    {a, b, c, d, e, f, g, h} =
        {input, 0, 0, 0, 0, 0, 0, 0} if sel == 000
        {0, input, 0, 0, 0, 0, 0, 0} if sel == 001
        ...
        {0, 0, 0, 0, 0, 0, 0, input} if sel == 111
-}
dmux_8_way input[n] sel[3] -> { a[n], b[n], c[n], d[n], e[n], f[n], g[n], h[n] }
```

<details>
<summary>Hints</summary>
Fill in the blanks below:
<pre>
<code>
dmux_8_way input[n] sel[3] ->
    { a[n], b[n], c[n], d[n], e[n], f[n], g[n], h[n] } =
    let
        { a = a, b = b, c = c, d = d } =
            ___________________
        { a = e, b = f, c = g, d = h } =
            ___________________
        sel_when_0 output[n] -> [n] =
            ___________________
        sel_when_1 output[n] -> [n] =
            ___________________
    in
    { a = ___________________
    , b = ___________________
    , c = ___________________
    , d = ___________________
    , e = ___________________
    , f = ___________________
    , g = ___________________
    , h = ___________________
    }
</code>
</pre>
</details>

🎉 We just completed all the common logic circuits for our computer!

## Arithmetic Circuits

Logic is fun but as the name computer suggests, we need to conduct arithmetic computations in order to do more interesting stuff. We will learn how to add and subtract two binary numbers with an added bonus of multiplication.

## Half Adder

Let's start from the bare minimum - adding two 1-bit numbers together. Binary addition works the same way as decimal addition. The only difference is that it's much simpler! A bit can only be either `0` or `1`. Addition of two bits have only four cases:

| a | b |
|:-:|:-:|
| 0 | 0 |
| 0 | 1 |
| 1 | 0 |
| 1 | 1 |

For the first three cases, the sum is very straight forward:

| a | b |sum|
|:-:|:-:|:-:|
| 0 | 0 | 0 + 0 = 0 |
| 0 | 1 | 0 + 1 = 1 |
| 1 | 0 | 1 + 0 = 1 |

The fourth case is a little tricky. In decimals, `1 + 1` equals `2`. However, the highest digit we got in binary is `1`. What do we do now? Let's think about what we do in this situation when adding two decimals. If we add `1` and `9`, we get the sum equal to `0` with a carry of `1`. Because the carry is at a more significant digit, we right the result as carry (`1`) then sum (`0`) or `10`.

Similarly in binary, we also get a sum of `0` and a carry of `1` or `10`. However, `10` in binary means `(1 + 2 ^ 1) + (0 * 2 ^ 0) = 2 + 0 = 2`. This makes sense because we expect `1 + 1` to equal `2` no matter what base we are in!

Ok. Let's add the fourth case to our truth table while introducing a new carry output:

| a | b |carry|sum|
|:-:|:-:|:---:|:-:|
| 0 | 0 |  0  | 0 |
| 0 | 1 |  0  | 1 |
| 1 | 0 |  0  | 1 |
| 1 | 1 |  1  | 0 |

It's time for you to implement the half adder in Sim:

```elm
half_adder a b -> { carry, sum }
```

<details>
<summary>Hints</summary>
Look at carry and sum separately. Do their truth tables look familiar?
</details>

## Full Adder

Half adder works well for adding two 1-bit numbers. However, when we need to add two multi-bit numbers, we need to take account of the carry bit from the previous less significant digit. That's when the full adder comes in:

| a | b | c |carry|sum|
|:-:|:-:|:-:|:---:|:-:|
| 0 | 0 | 0 |  0  | 0 |
| 0 | 0 | 1 |  0  | 1 |
| 0 | 1 | 0 |  0  | 1 |
| 0 | 1 | 1 |  1  | 0 |
| 1 | 0 | 0 |  0  | 1 |
| 1 | 0 | 1 |  1  | 0 |
| 1 | 1 | 0 |  1  | 0 |
| 1 | 1 | 1 |  1  | 1 |

```elm
full_adder a b c -> { carry, sum }
```

<details>
<summary>Hints</summary>
<pre>
<code>
full_adder a b c -> { carry, sum } =
    let
        { carry = ____, sum = ____ } =
            ___________________
        { carry = ____, sum = ____ } =
            ___________________
        ____ =
            ___________________
    in
    { carry = ____, sum = ____ }
</code>
</pre>
</details>

## 2-bit Full Adder

There are only very few things you can do with 1-bit numbers. It's time to spice up our full adder to accept multi-bit numbers.

The full truth table has `(2 ^ 2) ^ 3 = 64` cases so we will not show it here. You can do some quick checks by adding the numbers yourself to see if your 2-bit full adder works.

```elm
full_adder2 a[2] b[2] c -> { carry, sum[2] }
```

The idea for the 2-bit full adder is:
* Calculate the sum of the first bits, i.e. `c0, s0 = a[0] + b[0]`
* Calculate the sum of the second bits
  and the carry of the previous sum, i.e. `c1, s1 = a[1] + b[1] + c0`

There's a little challenge though. How do we combine the two sums `s0` and `s1` together as the final 2-bit sum? For this, we need the **bus literal**. Bus literals are a list of bits stringed together, hence the name bus. Here's an example:

```elm
[ 0, 1, 1, 0, 1 ]
```

Note that a bus is much stricter than a list or array in other languages. For instance, buses can only contain 1-bit numbers. The followings **is not legal**:

```elm
-- 2 or 0b10 occupies 2 bit (bigger than 1 bit).
[ 0, 1, 2 ]

-- Elements can only be 1-bit numbers.
-- records like { a = 2 } and any other type are not allowed.
[ 0, { a = 2} ]
```

One important property of bus literal is that it preserves leading zeros:

```elm
-- Leading zeros are preserved
[ 0, 0, 1, 0 ]
```

This is not that important for a single bus literal. However, sometimes we need to concatenate or combine two buses together using `++`:

```elm
-- Prefixing a `1` to our bus literal
-- results in [ 1, 0, 0, 1, 0 ]
[ 1 ] ++ [ 0, 0, 1, 0]
```

Concat the bus literals results in `[ 1, 0, 0, 1, 0 ]` (preserving leading zeros) instead of `[ 1, 0, 1, 0 ]` (discarding leading zeros). Compare this to int literals with the same values:

```elm
-- Prefixing a `1` to our int literal
-- results in 0b110
0b1 ++ 0b0010
```

Concatenating the int literals results in `0b110` (discarding leading zeros) instead of `0b10010` (preserving leading zeros).

If you now understand bus literals and have ideas on how to implement the 2-bit full adder, give it a go. If you get stuck any time, check out the hints:

<details>
<summary>Hints</summary>
<pre>
<code>
full_adder2 a[2] b[2] c -> { carry, sum[2] } =
    let
        { carry = ____, sum = ____ } =
            ___________________
        { carry = ____, sum = ____ } =
            ___________________
    in
    { carry = ____, sum = [ ____, ____ ] }
</code>
</pre>
</details>

## 4-bit Full Adder

```elm
full_adder4 a[4] b[4] c -> { carry, sum[4] }
```

<details>
<summary>Hints</summary>
<pre>
<code>
full_adder4 a[4] b[4] c -> { carry, sum[4] } =
    let
        { carry = ____, sum = ____ } =
            ___________________
        { carry = ____, sum = ____ } =
            ___________________
    in
    { carry = ____, sum = [ ____, ____ ] }
</code>
</pre>
</details>

## 8-bit Full Adder

By far you should get the patterns of a multi-bit full adder. We can repeat this pattern any many time we want to generate arbitrary-sized full adders. However, we will only go until 16-bit which is the size of our computer registers (more on that later).

```elm
full_adder8 a[8] b[8] c -> { carry, sum[8] }
```

## 16-bit Adder

Our last adder in the series will be a half adder because we don't need to compose the 16-bit adder even more to create bigger adders.

```elm
adder16 a[16] b[16] -> { carry, sum[16] }
```

## Subtraction and Negative Numbers

Our new 16-bit adder looks all fancy and well. However, now you might be curious about how subtraction works in binary. But before we talk about subtraction, we need to talk about negative numbers. There are two reasons:
* A bigger number - a smaller number = a negative number
* a - b = a + (-b)

So how do we represent negative numbers in binary?

### Option 1: Sign + Magnitude

Let's refer to the decimal system with "+" and "-" and encode "+" as `0` and "-" as `1`.

|n in decimal |n in binary|-n in decimal|-n in binary|
|:-----:|:----:|:-----:|:----:|
|+1     |0001  |-1     |1001  |
|+2     |0010  |-2     |1010  |
|+3     |0011  |-3     |1011  |
|+4     |0100  |-4     |1100  |
|+5     |0101  |-5     |1101  |
|+6     |0110  |-6     |1110  |
|+7     |0111  |-7     |1111  |

What about `0`? We unfortunately get two representations of `0`: `+0` and `-0`.

|+0  |-0  |
|:--:|:--:|
|0000|1000|

Two zeros are an extreme pain to deal with both in the hardware and software level. For example, a simple check whether a number is zero need to handle two cases - whether it's a positive or a negative zero.

This looks fine until we try to do a subtraction, say `7 - 3`. Because subtraction is just the addition of the negated, `7 - 3` is just `(+7) + (-3)`. If we look up the above table for `+7` and `-3` and add them together:

```
       s|mag
   +7  0|111
+  -3  1|011
――――――――――――
   +4 10|010
```

Notice that we get `010` which is `2` in decimals but we expect `7 - 3` to be `4`. We don't even want to think about the messy carry of the sign bit. This system of tagging an extra sign bit in front of the magnitude is looks familiar and fine at first sight, but is very tricky to deal with.

### Option 2: 1's Complement

In Mathematics, the method of complements is a technique to encode a symmetric range of positive and negative integers. For a given number of places half of the possible representations of numbers encode the positive numbers, the other half represents their respective negative numbers.

We obtain the 1's complement of a binary number by subtracting it from `1111`. Let's say we have a binary number `0110`. We get the 1's complement of `0110` like so:

```
   1111
-  0110
―――――――
   1001
```

Another example:

```
   1111
-  1010
―――――――
   0101
```

Do you notice any patterns?

<details>
<summary>Our Answer</summary>
Yes, we simply flip each bit to get the 1's complement.
</details>


|n in decimal |n in binary|-n in decimal|-n in binary|
|:-----:|:----:|:-----:|:----:|
|+1     |0001  |-1     |1110  |
|+2     |0010  |-2     |1101  |
|+3     |0011  |-3     |1100  |
|+4     |0100  |-4     |1011  |
|+5     |0101  |-5     |1010  |
|+6     |0110  |-6     |1001  |
|+7     |0111  |-7     |1000  |

What about `0`? We unfortunately still get two representations of `0`: `+0` and `-0`.

|+0  |-0  |
|:--:|:--:|
|0000|1111|

Let's try the same `7 - 3` by looking up the table for the binary representations of `+7` and `-3` and add them together:

```
   +7  0111
+  -3  1100
――――――――――――
   +4 10011
```

We get back `0011` or `3` which is not the right answer. However, if we add the carry bit `1` to `0011`:

```
  0011
+    1
――――――
  0100
```

Voila! We get back `0100` which is the correct answer `+4` in binary!

This seems like a neat trick and works every time. However, as computer scientists, we want to understand exactly how things work instead of memorizing tricks. In this case, the explanation is simple with some clever tweaks:

```
  a - b
= a + (-b)
= a + (-b) + 1111 - 1111
= a + (1111 - b)  - 1111
```
Notice that `1111 - b` is our 1's complement of `b`. Continue from the previous step:
```
= a + (1111 - b)  - 1111
= result - 1111
```
Notice the `result` means the result of adding `a` and the 1's complement of `b`. This is analogous to the result `10011` we got previously when adding `0111` (`+7`) and `1100` (`-3`). Continue from the previous step:
```
= result - 1111
= result - (10000 - 1)
= result - 10000 + 1
```
We rewrote `1111` with `10000 - 1` and extract the `-1` out of the parentheses to become a `+1`. Continue:
```
= result - 10000 + 1
= result + 1
```
Subtracting any 4-bit number by `10000` does nothing to the 4-bits. Since we are dealing with 4-bit numbers only, we don't care what happens to the `1` on the fifth bit.

Now you should understand why we add the `1` from the carry bit to our result.

## Option 3: 2's Complement
While 1's complement is much better than the sign + magnitude approach, we still need to deal with two zeros and had to add the carry bit when doing addition. Enter 2's complement which elegantly solves both issues. All you need to do is add `1` to a number's 1's complement to get its 2's complement.

|n in decimal |n in binary|-n in decimal|-n in binary|
|:-----:|:----:|:-----:|:----:|
|+1     |0001  |-1     |1111  |
|+2     |0010  |-2     |1110  |
|+3     |0011  |-3     |1101  |
|+4     |0100  |-4     |1100  |
|+5     |0101  |-5     |1011  |
|+6     |0110  |-6     |1010  |
|+7     |0111  |-7     |1001  |
|       |      |-8     |1000  |

Note that the numbers we can represent with 4 bits range from `-8` to `+7`. The reason for the extra `-8` is because we freed one slot by removing the double zeros!

Just in case you are not convinced, let's try to get the negative zero by taking the 2's complement of zero:

```
  0000
  1111  flip all bits to get 1's complement
+    1  add one to get 2's complement
――――――
 10000  2's complement of 0
```

Since we only care about 4 bits, the result `10000` is effectively still `0`.

Let's try the same `7 - 3` by looking up the table for the binary representations of `+7` and `-3` and add them together:

```
   +7  0111
+  -3  1101
――――――――――――
   +4 10100
```

Voila! We got `7 - 3 = 4` as expected.

Let's see why 2's complement works:

```
  a - b
= a + (-b)
= a + (-b) + 1111 - 1111
= a + (1111 - b)  - 1111         same steps as the 1's complement
= a + (1111 - b + 1) - 1111 - 1  add then subtract 1
```
Notice that `(1111 - b + 1)` is the 2's complement of `b`. It also matches our algorithm of getting the 1's complement of a number then add `1` to get its 2's complement. Continue:
```
= a + (1111 - b + 1) - 1111 - 1  add then subtract 1
= result - 1111 - 1
= result - 10000
= result
```

After going through three viable options for representing negative numbers in binary, we and most computer scientists choose 2's complement for its simplicity. Enough theory, let's try to implement a subtractor! So what circuits do we need? Because `a - b = a + (-b)`, we just need to create a negator and add the negated `b` to `a` as if we are subtracting `b` from `a`.

## 16-bit Negator

How do we negate a binary number? We just take its 2's complement:
* Flip all bits
* Add 1

Try implement the negator on your own.

Here's the header:
```elm
neg16 input[16] -> [16]
```

<details>
<summary>Hints</summary>
Does flipping bits sound similar to a logic gate we created before?

Which circuit we designed earlier allows you add two numbers?
</details>

## Arithmetic Logic Unit

After implementing all the logic and arithmetic circuits, we can now combine them into a powerful machine called the Arithmetic Logic Unit or ALU. For our ALU, it will be able to do addition, subtraction, negation, `and`, `or`, and `not`. More precisely, given two 16-bit inputs `x` and `y`, the ALU can do:
```
x+y, x-y, y-x, 0, 1, -1, x, y, -x, -y, !x, !y,
x+1, y+1, x-1, y-1, x&y, x|y
```
according to `6` control bits:
```
zx,nx,zy,ny,f,no
```
The ALU logic manipulates the x and y inputs
and operates on the resulting values, as follows:
```
if (zx == 1) set x = 0            16-bit constant
if (nx == 1) set x = !x           bitwise not
if (zy == 1) set y = 0            16-bit constant
if (ny == 1) set y = !y           bitwise not
if (f == 1)  set out = x + y      integer 2's complement addition
if (f == 0)  set out = x & y      bitwise and
if (no == 1) set out = !out       bitwise not
```
According to the final `out` value, we will compute two 1-bit outputs:
```
if the ALU output == 0, zr is set to 1; otherwise zr is set to 0;
if the ALU output < 0, ng is set to 1; otherwise ng is set to 0.
```

Here's the header of the ALU:
```elm
alu
    x[16] y[16] -- 16-bit inputs
    zx -- zero the x input?
    nx -- negate the x input?
    zy -- zero the y input?
    ny -- negate the y input?
    f  -- compute out = x + y if f == 1 or x & y if f == 0
    no -- negate the output?
    ->
    { out[16] -- 16-bit output
    , zr -- 1 if out == 0, 0 otherwise
    , ng -- 1 if out < 0, 0 otherwise
    }
```

The ALU seems very complicated at first sight. However, it only uses the functions we defined earlier and apply the operations that depend on the control bits sequentially.

<details>
<summary>Hints</summary>
Fill in the blanks below:
<pre>
<code>
alu
    x[16] y[16] -- 16-bit inputs
    zx -- zero the x input?
    nx -- negate the x input?
    zy -- zero the y input?
    ny -- negate the y input?
    f  -- compute out = x + y if f == 1 or x & y if f == 0
    no -- negate the output?
    ->
    { out[16] -- 16-bit output
    , zr -- 1 if out == 0, 0 otherwise
    , ng -- 1 if out < 0, 0 otherwise
    } =
    let
        x1 =
            ____ ____ ____ zx
        x2 =
            ____ ____ ____ nx
        y1 =
            ____ ____ ____ zy
        y2 =
            ____ ____ ____ ny
        out1 =
            ____ ____ ____ f
        out2 =
            ____ ____ ____ no
        zr =
            ____ ___________________
        ng =
            ___________________
    in
    { out = out2, zr = zr, ng = ng }
</code>
</pre>
</details>

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

## Release v0.6.0
* Add concatenation operator (++)
* Fix binary parsing and display
* Fix line comment newline counting problem

## Release v0.5.0
* Add busLiteral expression, e.g. [0, 1, 0, 0]
* Better generate and display truth tables of different sizes
* Lock caption and header of truth table when scrolling

## Release v0.4.0
* Fix indentation checking for record, intLiteral, and bindingOrCall
* Detect binding record assignment mismatches
* Pop parameters and local names from context when out of scope
* Fix the emitted JS of indexing expr another time
* Fix indexing on IntSize type inference
* Filter out duplicated parser problems

## Release v0.3.0
* Fix the emitted JS of indexing expr
* Fix EqualToSize comparison in unify
* Check for duplicated names

## Release v0.2.0
* Properly show 2's complement in decimal in truth table
* Add fill function to prelude
* Allow spaces inside parenthesized group expr
* Fix some checker error messages

## Release v0.1.0
* Store units in localStorage
* Can remove tabs
* Fix parse error underlining