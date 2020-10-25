context("type.R")

test_that(
  "test JSON type functions",
  {
    expect_equal(.mlc_list(.mlc_integer),
                 '{"list":["integer"]}')
    expect_equal(.mlc_tuple(.mlc_integer, .mlc_numeric),
                 '{"tuple":["integer","numeric"]}')
    expect_equal(.mlc_list(.mlc_tuple(.mlc_integer, .mlc_numeric)),
                 '{"list":[{"tuple":["integer","numeric"]}]}')
    expect_equal(.mlc_object(a=.mlc_integer),
                 '{"a":"integer"}')
    expect_equal(.mlc_object(a='"yolo"',b=.mlc_array("1", "34")),
                 '{"a":"yolo","b":[1,34]}')
    expect_equal(.mlc_record(a=.mlc_integer),
                 '{"record":{"a":"integer"}}')
    expect_equal(.mlc_record(a='"yolo"',b=.mlc_array("1", "34")),
                 '{"record":{"a":"yolo","b":[1,34]}}')
    expect_equal(.mlc_table(a=.mlc_integer),
                 '{"table":{"a":{"list":["integer"]}}}')
    expect_equal(.mlc_table(a='"yolo"',b=.mlc_array("1", "34")),
                 '{"table":{"a":{"list":["yolo"]},"b":{"list":[[1,34]]}}}')
    # table type is the same as a record of lists
    expect_equal(.mlc_table(a='"yolo"'),
                 sub("record", "table", .mlc_record(a=.mlc_list('"yolo"'))))
  }
)
