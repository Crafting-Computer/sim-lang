module CheckerTest exposing (suite)

import Test exposing (Test, describe)
import Expect
import HdlChecker exposing (Type(..), Problem(..))
import HdlParser exposing (Size(..))
import AssocList as Dict

suite : Test
suite =
  describe "Checker"
    [ describe "Prelude"
      [ Test.test "Using prelude functions" <|
        \_ ->
          let
            src =
              "not a[1] -> [1] = nand a a"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      ]
    , describe "Bus Size"
      [ Test.test "Omit param bus size and default to 1" <|
        \_ ->
          let
            src =
              "not a -> [1] = nand a a"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      , Test.test "Return type bus size too large" <|
        \_ ->
          let
            src =
              "not a[2] -> [1] = nand a a"
            expected =
              Err [ MismatchedTypes { from = (1,13), to = (1,16), value = BusType (IntSize 1) } { from = (1,19), to = (1,27), value = BusType (IntSize 2) }]
          in
          Expect.equal expected (check src)
      , Test.test "Arg type bus size too large" <|
        \_ ->
          let
            src =
              "not a -> [1] = nand a a\nusing_not a[2] -> [2] = not a"
            expected =
              Err [MismatchedTypes { from = (1,5), to = (1,6), value = BusType (IntSize 1) } { from = (2,29), to = (2,30), value = BusType (IntSize 2) }]
          in
          Expect.equal expected (check src)
      , Test.test "Use user-defined functions" <|
        \_ ->
          let
            src =
              "not a -> [1] = nand a a\nor a b -> [1] = nand (not a) (not b)"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      , Test.test "Variable parameter bus size" <|
        \_ ->
          let
            src =
              "not a[n] -> [n] = nand a a"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      , Test.test "VarSize output does not match declared IntSize return type" <|
        \_ ->
          let
            src =
              "not a[n] -> [1] = nand a a"
            expected =
              Err [MismatchedTypes { from = (1,13), to = (1,16), value = BusType (IntSize 1) } { from = (1,19), to = (1,27), value = BusType (VarSize "n" Nothing) }]
          in
          Expect.equal expected (check src)
      ]
      , describe "Undefined names"
      [ Test.test "Undefined function name" <|
        \_ ->
          let
            src =
              "or a b -> [1] = nand (not a) (not b)"
            expected =
              Err [UndefinedName { from = (1,31), to = (1,34), value = "not" },UndefinedName { from = (1,23), to = (1,26), value = "not" }]
          in
          Expect.equal expected (check src)
      , Test.test "Undefined function names" <|
        \_ ->
          let
            src =
              "half_adder a b -> { sum, carry } =\n  let\n    sum = xor a b\n    carry = and a b\n  in\n  { sum = sum, carry = carry }"
            expected =
              Err [UndefinedName { from = (3,11), to = (3,14), value = "xor" },UndefinedName { from = (4,13), to = (4,16), value = "and" }]
          in
          Expect.equal expected (check src)
      , Test.test "Undefined binding name" <|
        \_ ->
          let
            src =
              "not a -> [1] = nand a b"
            expected =
              Err [UndefinedName { from = (1,23), to = (1,24), value = "b" }]
          in
          Expect.equal expected (check src)
      ]
      , describe "Records"
      [ Test.test "Record output" <|
        \_ ->
          let
            src =
              "combine a b -> { a, b } = { a = a, b = b }"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      , Test.test "Record output does not match declared record return type" <|
        \_ ->
          let
            src =
              "combine a b -> { a, b } = { a = a, c = b }"
            expected =
              Err [ MismatchedTypes
                { from = (1,16), to = (1,24), value = RecordType (Dict.fromList [("a",BusType (IntSize 1)),("b",BusType (IntSize 1))]) }
                { from = (1,27), to = (1,43), value = RecordType (Dict.fromList [("a",BusType (IntSize 1)),("c",BusType (IntSize 1))]) }
              ]
          in
          Expect.equal expected (check src)
      , Test.test "Record assignment in locals" <|
        \_ ->
          let
            src =
              "combine a b -> { a, b } =\n  let { a = a1, b = b1 } = { a = a, b = b } in\n  { a = a1, b = b1 }"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      , Test.test "Undefined name in record literal" <|
        \_ ->
          let
            src =
              "combine a b -> { a, b } =\n  let { a = a1, b = b1 } = { a = a, b = b } in\n  { a = a1, b = c1 }"
            expected =
              Err [UndefinedName { from = (3,17), to = (3,19), value = "c1" }]
          in
          Expect.equal expected (check src)
      ]
      , describe "Int literal"
      [ Test.test "direct assignment of decimal" <|
        \_ ->
          let
            src =
              "a = 199"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      , Test.test "direct assignment of hexadecimal" <|
        \_ ->
          let
            src =
              "a = 0xFF"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      , Test.test "direct assignment of binary" <|
        \_ ->
          let
            src =
              "a = 0b1011101"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      , Test.test "use in record literals" <|
        \_ ->
          let
            src =
              "r = { a = 0, b = 10 }"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      , Test.test "use in call" <|
      \_ ->
        let
          src =
            "a = nand 28 29"
          expected =
            Ok ()
        in
        Expect.equal expected (check src)
      , Test.test "mixing different bases" <|
      \_ ->
        let
          src =
            "a = nand 0x23 0b00100011"
          expected =
            Ok ()
        in
        Expect.equal expected (check src)
      ]
      , describe "Call"
      [ Test.test "simple call" <|
        \_ ->
          let
            src =
              "a = nand 0 0"
            expected =
              Ok ()
          in
          Expect.equal expected (check src)
      , Test.test "call arity too small" <|
        \_ ->
          let
            src =
              "a = nand 0"
            expected =
              Err [WrongCallArity { from = (1,5), to = (1,9), value = "nand" } [{ name = { from = (-1,-1), to = (-1,-1), value = "a" }, size = { from = (-1,-1), to = (-1,-1), value = VarSize "n" Nothing } },{ name = { from = (-1,-1), to = (-1,-1), value = "b" }, size = { from = (-1,-1), to = (-1,-1), value = VarSize "n" Nothing } }] [{ from = (1,10), to = (1,11), value = BusType (IntSize 1) }]]
          in
          Expect.equal expected (check src)
      , Test.test "call arity too big" <|
        \_ ->
          let
            src =
              "a = nand 0 0 0"
            expected =
              Err [WrongCallArity { from = (1,5), to = (1,9), value = "nand" } [{ name = { from = (-1,-1), to = (-1,-1), value = "a" }, size = { from = (-1,-1), to = (-1,-1), value = VarSize "n" Nothing } },{ name = { from = (-1,-1), to = (-1,-1), value = "b" }, size = { from = (-1,-1), to = (-1,-1), value = VarSize "n" Nothing } }] [{ from = (1,10), to = (1,11), value = BusType (IntSize 1) }, { from = (1,12), to = (1,13), value = BusType (IntSize 1) },{ from = (1,14), to = (1,15), value = BusType (IntSize 1) }]]
          in
          Expect.equal expected (check src)
      ]
      , describe "Indexing"
      [ test "Indexing a bus"
        "head bus[n] -> [1] = bus[0]" <|
        Ok ()
      
      , test "Slicing a bus"
        "first4 bus[16] -> [4] = bus[0..3]" <|
        Ok ()
      
      ,  test "Index out of bounds"
        "head bus[2] -> [1] = bus[2]" <|
        Err [IndexOutOfBounds 2 { from = (1,26), to = (1,27), value = 2 } { from = (1,26), to = (1,27), value = 2 }]
      
      , test "end index out of bounds"
        "first4 bus[3] -> [4] = bus[0..3]" <|
        Err [IndexOutOfBounds 3 { from = (1,28), to = (1,29), value = 0 } { from = (1,31), to = (1,32), value = 3 }]
      
      , test "start index out of bounds"
        "first4 bus[3] -> [4] = bus[3..4]" <|
        Err [IndexOutOfBounds 3 { from = (1,28), to = (1,29), value = 3 } { from = (1,31), to = (1,32), value = 4 }]
      
      , test "start index greater than end index"
        "first4 bus[3] -> [2] = bus[2..1]" <|
        Err [FromIndexBiggerThanToIndex { from = (1,28), to = (1,29), value = 2 } { from = (1,31), to = (1,32), value = 1 }]
      ]
    ]

test : String -> String -> Result (List HdlChecker.Problem) () -> Test
test name src expected =
  Test.test name <|
    \_ -> Expect.equal expected (check src)

check : String -> Result (List HdlChecker.Problem) ()
check src =
  case HdlParser.parse src of
    Err deadEnds -> -- parse error should never happen when testing the checker
      Err [ UndefinedName <| HdlParser.fakeLocated <| HdlParser.showDeadEnds src deadEnds]
    Ok program ->
      HdlChecker.check program