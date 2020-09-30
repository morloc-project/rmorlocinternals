context("type.R")

test_that(
  "test JSON type functions",
  {
    expect_equal(.mlc_list(.mlc_integer), '{"list":["integer"]}')
    expect_equal(.mlc_tuple(.mlc_integer, .mlc_numeric), '{"tuple":["integer","numeric"]}')
    expect_equal(.mlc_list(.mlc_tuple(.mlc_integer, .mlc_numeric)),
                 '{"list":[{"tuple":["integer","numeric"]}]}')
  }
)
