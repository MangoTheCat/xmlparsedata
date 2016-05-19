
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
#' @param pretty Whether to pretty-indent the XML output.
#' @inheritParams utils::getParseData
#' @return An XML string representing the parse data. See details below
#'
#' @export
#' @importFrom utils getParseData
#' @examples
#' code <- "function(a = 1, b = 2) {\n  a + b\n}\n"
#' expr <- parse(text = code, keep.source = TRUE)
#' cat(xml_parse_data(expr))

xml_parse_data <- function(x, includeText = NA, pretty = FALSE) {
  pd <- getParseData(x, includeText = includeText)
  pd <- fix_comments(pd)

  ## Tags for all nodes, teminal nodes have end tags as well
  pd$token <- map_token(pd$token)
  pd$tag <- paste0(
    "<", pd$token,
    " line1=\"", pd$line1,
    "\" col1=\"", pd$col1,
    "\" line2=\"", pd$line2,
    "\" col2=\"", pd$col2,
    "\">",
    xml_encode(pd$text),
    ifelse(pd$terminal, paste0("</", pd$token, ">"), "")
  )

  ## Add an extra terminal tag for each non-terminal one
  pd2 <- pd[! pd$terminal, ]
  pd2$terminal <- TRUE
  pd2$parent <- -1
  pd2$line1 <- pd2$line2
  pd2$col1 <- pd2$col2
  pd2$line2 <- pd2$line2 - 1L
  pd2$col2 <- pd2$col2 - 1L
  pd2$tag <- paste0("</", pd2$token, ">")

  pd <- rbind(pd, pd2)

  ## Order the nodes properly
  pd <- pd[ order(pd$line1, pd$col1, -pd$line2, -pd$col2, pd$terminal), ]

  if (pretty) {
    str <- ! pd$terminal
    end <- pd$parent == -1
    ind <- 2L + cumsum(str * 2L + end * (-2L)) - str * 2L
    xml <- paste0(spaces(ind), pd$tag, collapse = "\n")

  } else {
    xml <- paste(pd$tag, collapse = "\n")
  }

  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n<exprlist>\n",
    xml,
    "\n</exprlist>\n"
  )
}

fix_comments <- function(pd) {
  pd[ pd$parent >= 0, ]
}

map_token <- function(token) {
  map <- map[token]
  ifelse(is.na(map), token, map)
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
