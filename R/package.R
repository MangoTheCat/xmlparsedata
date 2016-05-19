
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

  ## The root is not in the tree, so need to handle separately here
  children_xml <- vapply(
    FUN = to_xml_subtree, FUN.VALUE = "",
    which(pd$parent == 0),
    pd = pd, forest = forest
  )
  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n",
    "<exprlist>\n",
    paste(children_xml, collapse = "\n"),
    "\n</exprlist>\n"
  )
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

to_xml_subtree <- function(pd, forest, root, indent = 0) {
  root_xml <- to_xml_node(pd, root)
  ss <- spaces(indent)

  num_children <- length(forest[[root]])
  children_xml <- vapply(
    FUN = to_xml_subtree, FUN.VALUE = "",
    forest[[root]],
    pd = pd, forest = forest, indent = indent + 2
  )

  paste0(
    ss, root_xml[[1]],
    if (num_children) "\n",
    if (num_children) paste(children_xml, collapse = "\n"),
    if (num_children) "\n",
    if (num_children) ss,
    root_xml[[2]]
  )
}

to_xml_node <- function(pd, node) {
  token <- map_token(pd$token[node])
  c(paste0("<",  token,
           " line1=\"", pd$line1[node], "\" col1=\"", pd$col1[node],
           "\" line2=\"", pd$line2[node], "\" col2=\"", pd$col2[node],
           "\">", xml_encode(pd$text[node])),
    paste0("</", token, ">"))
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
