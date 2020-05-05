# Sim

Sim aims to make circuit design as simple and fun as possible by using intuitive interfaces and powerful abstractions. Sim is created as a part of the [Crafting Computer](https://kevinli.gitbook.io/crafting-computer/) course.

**Try the online Sim editor** [**here**](https://alienkevin.github.io/sim-lang/)**!**

## Development

### Set up

* Follow instructions [here](https://guide.elm-lang.org/install/) to install `Elm` which is powers the Sim compiler and editor.
* Follow instructions [here](https://github.com/wking-io/elm-live) to install `elm-live` which is used for building and running Sim.

### Commands

* Run editor:

  ```bash
  sed -i 's+src="elm.js"+src="/public/elm.js"+' public/index.html
  elm-live src/Editor.elm --start-page public/index.html -- --output=public/elm.js
  ```

* Build optimized version of editor:

  ```bash
  ./build.sh
  ```

  Note: you may need to enable execution permission before running the command:

  ```bash
  chmod +x ./build.sh
  ```

* Run Sim compiler on a source string:

  ```bash
  elm-live src/Main.elm --start-page debug/index.html -- --output=debug/elm.js
  ```

## Credits

Thanks to all the wonderful projects below that inspired Sim:

* [Nand to Tetris](https://www.nand2tetris.org/)
* [Elm](http://elm-lang.org/)
* [Type inference for beginners — Part 1](https://medium.com/@dhruvrajvanshi/type-inference-for-beginners-part-1-3e0a5be98a4b)

## License

MIT

## Change Log

### Release v0.7.0

* Constrain VarSize names to the function they are defined in
* Add ALU tutorial

### Release v0.6.0

* Add concatenation operator \(++\)
* Fix binary parsing and display
* Fix line comment newline counting problem

### Release v0.5.0

* Add busLiteral expression, e.g. \[0, 1, 0, 0\]
* Better generate and display truth tables of different sizes
* Lock caption and header of truth table when scrolling

### Release v0.4.0

* Fix indentation checking for record, intLiteral, and bindingOrCall
* Detect binding record assignment mismatches
* Pop parameters and local names from context when out of scope
* Fix the emitted JS of indexing expr another time
* Fix indexing on IntSize type inference
* Filter out duplicated parser problems

### Release v0.3.0

* Fix the emitted JS of indexing expr
* Fix EqualToSize comparison in unify
* Check for duplicated names

### Release v0.2.0

* Properly show 2's complement in decimal in truth table
* Add fill function to prelude
* Allow spaces inside parenthesized group expr
* Fix some checker error messages

### Release v0.1.0

* Store units in localStorage
* Can remove tabs
* Fix parse error underlining
