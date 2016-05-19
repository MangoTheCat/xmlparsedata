
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
  forest <- make_forest(pd)
  to_xml_subtree(pd, forest, "0")
}

fix_comments <- function(pd) {
  pd$parent[ pd$parent < 0 & pd$token == "COMMENT" ] <- 0
  pd
}

## This is just the order within the file,
## we don't care about the tree structure here.
## The purpose is to order the children of a node reasonably well

correct_order <- function(pd) {
  pd[ order(pd$line1, pd$col1, -pd$line2, -pd$col2), ]
}

make_forest <- function(pd) {
  lapply(tapply(pd$id, pd$parent, c, simplify = FALSE), chr)
}

to_xml_subtree <- function(pd, forest, root, indent = 0) {
  root_xml <- to_xml_node(pd, root)

  num_children <- length(forest[[root]])
  children_xml <- vapply(
    FUN = to_xml_subtree, FUN.VALUE = "",
    forest[[root]],
    pd = pd, forest = forest, indent = indent + 2
  )

  paste0(
    spaces(indent), root_xml[[1]],
    if (num_children) "\n",
    if (num_children) paste(children_xml, collapse = "\n"),
    if (num_children) "\n",
    if (num_children) spaces(indent),
    root_xml[[2]]
  )
}

to_xml_node <- function(pd, node) {
  if (node == "0") {
    c("<exprlist>\n", "\n</exprlist>")

  } else {
    mypd <- pd[node, ]
    token <- map_token(mypd$token)
    pos <- paste0(mypd$line1, ":", mypd$col1, "-",
                  mypd$line2, ":", mypd$col2)
    c(paste0("<",  token, " pos=\"", pos, "\">", xml_encode(mypd$text)),
      paste0("</", token, ">"))
  }
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
  "'*'" = "OP-SLASH",
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
  "','" = "OP-COMMA"
)

xml_encode <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}
