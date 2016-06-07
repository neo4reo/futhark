-- testing that variable shadowing and chunking 
-- doesn't allow for duplicate definitions
-- ==
-- error:
type foo = (int, f32)

structure M0 =
  struct
    type foo = foo -- the type is defined from l. 1
    type bar = f32
  end

structure M1 =
  struct
    type foo = f32
    type bar = M0.bar -- type is defined from l.6

    structure M0 =
      struct
        type foo = M0.foo -- is defined at l. 5
        type bar = (int, int, int)
      end

    type foo = f32 -- REDEFINITION OF foo IN Struct M1
    type baz = M0.bar -- defined at line 17
  end

type baz = M1.baz -- is defined at l. 13

fun baz main(int a, float b) = (1,2,3)
  