
context("xmlparsedata")

test_that("empty input", {
  xml <- xml_parse_data(parse(text = "", keep.source = TRUE))
  expect_true(is.character(xml))
  expect_true(length(xml) == 1)
  expect_match(xml, "<exprlist>\\s*</exprlist>")
  expect_silent(x <- xml2::read_xml(xml))
})

test_that("trivial input", {
  xml <- xml_parse_data(parse(text = "# comment\n", keep.source = TRUE))
  expect_true(is.character(xml))
  expect_true(length(xml) == 1)
  expect_match(xml, "<exprlist>\\s*<COMMENT [^<]*</COMMENT>\\s*</exprlist>")
  expect_silent(x <- xml2::read_xml(xml))

  xml <- xml_parse_data(parse(text = "1", keep.source = TRUE))
  expect_match(
    xml,
    paste0(
      "<exprlist>\\s*<expr [^<]*<NUM_CONST.*</NUM_CONST>\\s*",
      "</expr>\\s*</exprlist>"
    )
  )
  expect_silent(x <- xml2::read_xml(xml))
})

test_that("non-trivial input", {
  ip <- deparse(utils::install.packages)
  xml <- xml_parse_data(parse(text = ip, keep.source = TRUE))
  expect_silent(x <- xml2::read_xml(xml))
})
