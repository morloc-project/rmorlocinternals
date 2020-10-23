context("serialization.R")

test_that(
  "can deserialize primitives",
  {
    # numbers can be cast as integers or numeric
    expect_identical(mlc_deserialize("42", .mlc_integer), 42L)
    expect_identical(mlc_deserialize("42", .mlc_numeric), 42)
    # but numbers cannot be cast as characters
    expect_error(mlc_deserialize("42", .mlc_character))
    expect_error(mlc_deserialize("42", .mlc_logical))

    expect_identical(mlc_deserialize("\"42\"", .mlc_character), "42")
    expect_identical(mlc_deserialize("true", .mlc_logical), T)
    expect_identical(mlc_deserialize("false", .mlc_logical), F)
    expect_identical(mlc_deserialize("\"false\"", .mlc_character), "false")
    expect_identical(mlc_deserialize("null", .mlc_null), NULL)
  }
)

test_that(
  "can serialize primitives",
  {
    # numeric
    expect_identical(mlc_serialize(42L, .mlc_integer), "42")
    expect_identical(mlc_serialize(42, .mlc_integer), "42")
    expect_identical(mlc_serialize(42, .mlc_numeric), "42.0")
    expect_error(mlc_serialize(42, .mlc_logical))
    expect_error(mlc_serialize(42, .mlc_character))
    # character
    expect_identical(mlc_serialize("", .mlc_character), '""')
    expect_identical(mlc_serialize("yolo", .mlc_character), '"yolo"')
    expect_error(mlc_serialize("", .mlc_numeric))
    # logical
    expect_identical(mlc_serialize(T, .mlc_logical), "true")
    expect_identical(mlc_serialize(F, .mlc_logical), "false")
    expect_error(mlc_serialize(F, .mlc_numeric))
    # null
    expect_identical(mlc_serialize(NULL, .mlc_null), "null")
  }
)

test_that(
  "can deserialize lists",
  {
    expect_identical( mlc_deserialize("[]", .mlc_list(.mlc_integer)),   integer(0))
    expect_identical( mlc_deserialize("[]", .mlc_list(.mlc_numeric)),   numeric(0))
    expect_identical( mlc_deserialize("[]", .mlc_list(.mlc_character)), character(0))
    expect_identical( mlc_deserialize("[]", .mlc_list(.mlc_logical)),   logical(0))
    expect_identical(
      mlc_deserialize("[1,2,3]", .mlc_list(.mlc_integer)),
      as.integer(c(1,2,3))
    )
    expect_identical(
      mlc_deserialize("[\"1\",\"2\",\"3\"]", .mlc_list(.mlc_character)),
      c("1","2","3")
    )
    expect_equal(
      mlc_deserialize("[[1,2],[3,4]]", .mlc_list(.mlc_tuple(.mlc_integer, .mlc_integer))),
      list(list(1,2), list(3,4))
    )
    expect_equal(
      mlc_deserialize("[[1,2],[3,4]]", .mlc_list(.mlc_list(.mlc_integer))),
      list(as.integer(c(1,2)), as.integer(c(3,4)))
    )
  }
)

test_that(
  "can serialize lists",
  {
    expect_identical( mlc_serialize(integer(0),   .mlc_list(.mlc_integer)),   "[]")
    expect_identical( mlc_serialize(numeric(0),   .mlc_list(.mlc_numeric)),   "[]")
    expect_identical( mlc_serialize(character(0), .mlc_list(.mlc_character)), "[]")
    expect_identical( mlc_serialize(logical(0),   .mlc_list(.mlc_logical)),   "[]")
    expect_identical(
      mlc_serialize(as.integer(1:3), .mlc_list(.mlc_integer)),
      "[1,2,3]"
    )
    expect_identical(
      mlc_serialize(as.character(1:3), .mlc_list(.mlc_character)),
      "[\"1\",\"2\",\"3\"]"
    )
    expect_identical(
      mlc_serialize(list(list(1,2), list(3,4)), .mlc_list(.mlc_tuple(.mlc_integer, .mlc_integer))),
      "[[1,2],[3,4]]"
    )
  }
)

test_that(
  "Can serialize records",
  {
    expect_equal(
      mlc_serialize(list(a=34), .mlc_record(a=.mlc_integer)),
      '{"a":34}'
    )
    expect_equal(
      mlc_serialize(list(a=34, b=c(1,2,3)), .mlc_record(a=.mlc_integer, b=.mlc_list(.mlc_integer))),
      '{"a":34,"b":[1,2,3]}'
    )
    # the schema order determines output order
    expect_equal(
      mlc_serialize(list(b=c(1,2,3), a=34), .mlc_record(a=.mlc_integer, b=.mlc_list(.mlc_integer))),
      '{"a":34,"b":[1,2,3]}'
    )
    expect_equal(
      mlc_serialize(list(a=34, b=c(1,2,3)), .mlc_record(b=.mlc_list(.mlc_integer), a=.mlc_integer)),
      '{"b":[1,2,3],"a":34}'
    )
  }
)

test_that(
  "Can deserialize records",
  {
    expect_equal(
      mlc_deserialize('{"a":34}', .mlc_record(a=.mlc_integer)),
      list(a=34)
    )
    expect_equal(
      mlc_deserialize('{"a":34,"b":[1,2,3]}', .mlc_record(a=.mlc_integer, b=.mlc_list(.mlc_integer))),
      list(a=34, b=c(1,2,3))
    )
    # the schema order determines output order
    expect_equal(
      mlc_deserialize('{"b":[1,2,3],"a":34}', .mlc_record(a=.mlc_integer, b=.mlc_list(.mlc_integer))),
      list(a=34, b=c(1,2,3))
    )
    expect_equal(
      mlc_deserialize('{"a":34,"b":[1,2,3]}', .mlc_record(b=.mlc_list(.mlc_integer), a=.mlc_integer)),
      list(b=c(1,2,3), a=34)
    )
  }
)

test_that(
  "Can deserialize lists of records"
    expect_equal(
      mlc_deserialize(
        '{"a":34,"b":[1,2,3]}',
        .mlc_list(.mlc_record(b=.mlc_integer, a=.mlc_integer)),
        list(b=c(1,2,3), a=34)
      )
    )
)
