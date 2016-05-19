
#' Parse Data of R Code as an 'XML' Tree
#'
#' Convert the output of 'utils::getParseData()' to an 'XML' tree, that is
#' searchable and easier to manipulate in general.
#'
#' @docType package
#' @name xmlparsedata
NULL

#' Convert R parse data to XML
#'
#' TODO
#'
#' @inheritParams utils::getParseData
#' @return An XML string representing the parse data. See details below
#'
#' @export
#' @importFrom utils getParseData
#' @examples
#' code <- "function(a = 1, b = 2) {\n  a + b\n}\n"
#' expr <- parse(text = code, keep.source = TRUE)
#' cat(xml_parse_data(expr))

xml_parse_data <- function(x, includeText = NA) {
  pd <- getParseData(x, includeText = includeText)
  pd <- fix_comments(pd)
  pd <- correct_order(pd)
  pd <- rewrite_ids(pd)
  forest <- make_forest(pd)

  ## Order, so that id corresponds to line number, simplifies indexing
  pd <- pd[ order(pd$id), ]

  ## It is easier to do it here, than individually
  pd$line1 <- as.character(pd$line1)
  pd$col1  <- as.character(pd$col1)
  pd$line2 <- as.character(pd$line2)
  pd$col2 <- as.character(pd$col2)

  outstream <- rawConnection(raw(0), "w")
  on.exit(close(outstream))

  writeChar(
    con = outstream, eos = NULL,
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n<exprlist>\n"
  )

  for (c in which(pd$parent == 0)) {
    to_xml_subtree(pd, forest, c, outstream)
    writeChar(con = outstream, "\n", eos = NULL)
  }

  writeChar(con = outstream, "</exprlist>\n", eos = NULL)

  rawToChar(rawConnectionValue(outstream))
}

fix_comments <- function(pd) {
  pd[ pd$parent >= 0, ]
}

## Rewrite the node IDs, so that they are consecutive, and
## we don't have to access the rows via names. Names are really
## slow for some reason

rewrite_ids <- function(pd) {
  levels <- sort(unique(c(pd$id, pd$parent)))
  pd$id <- as.integer(factor(pd$id, levels = levels)) - 1L
  pd$parent <- as.integer(factor(pd$parent, levels = levels)) -1L
  rownames(pd) <- NULL
  pd
}

## This is just the order within the file,
## we don't care about the tree structure here.
## The purpose is to order the children of a node reasonably well

correct_order <- function(pd) {
  pd[ order(pd$line1, pd$col1, -pd$line2, -pd$col2), ]
}

make_forest <- function(pd) {
  forest <- tapply(pd$id, pd$parent, c, simplify = FALSE)
  empty <- structure(
    vector(nrow(pd), mode = "list"),
    names = 1:nrow(pd)
  )
  empty[as.integer(names(forest[-1]))] <- forest[-1]
  empty
}

to_xml_subtree <- function(pd, forest, root, outstream, indent = 0) {

  token <- map_token(pd$token[root])
  text <- xml_encode(pd$text[root])
  if (text == "") text <- character()

  writeChar(
    con = outstream, eos = NULL,
    c(spaces(indent), "<", token,
      " line1=\"", pd$line1[root],
      "\" col1=\"", pd$col1[root],
      "\" line2=\"", pd$line2[root],
      "\" col2=\"", pd$col2[root],
      "\">",
      text,
      if (length(forest[[root]])) "\n")
  )

  for (c in forest[[root]]) {
    to_xml_subtree(pd, forest, c, outstream, indent + 2)
    writeChar(con = outstream, "\n", eos = NULL)
  }

  writeChar(
    con = outstream, eos = NULL,
    c(if (length(forest[[root]])) spaces(indent), "</", token, ">")
  )
}

map_token <- function(token) {
  map <- map[token]
  if (is.na(map)) token else map
}

map <- c(
  "'?'" = "OP-QUESTION",
  "'~'" = "OP-TILDE",
  "'+'" = "OP-PLUS",
  "'-'" = "OP-MINUS",
  "'*'" = "OP-STAR",
  "'/'" = "OP-SLASH",
  "':'" = "OP-COLON",
  "'^'" = "OP-CARET",
  "'$'" = "OP-DOLLAR",
  "'@'" = "OP-AT",
  "'('" = "OP-LEFT-PAREN",
  "'['" = "OP-LEFT-BRACKET",
  "';'" = "OP-SEMICOLON",
  "'{'" = "OP-LEFT-BRACE",
  "'}'" = "OP-RIGHT-BRACE",
  "')'" = "OP-RIGHT-PAREN",
  "'!'" = "OP-EXCLAMATION",
  "']'" = "OP-RIGHT-BRACE",
  "','" = "OP-COMMA"
)

xml_encode <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}
