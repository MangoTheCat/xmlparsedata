
buf <- new.env(parent = emptyenv())

chunk_size <- 1000

buf_clear <- function() {
  rm(list = ls(buf), envir = buf)
  assign("1", vector(chunk_size, mode = "character"), envir = buf)
  assign("last", "1", envir = buf)
  assign("pos", 1, envir = buf)
}

buf_append <- function(str) {
  len <- length(str)
  if (!len) return()

  last <- get("last", envir = buf)
  pos <- get("pos", envir = buf)

  ## Do we have enough space in the last vector
  if (chunk_size - pos + 1 >= len) {
    vec <- get(last, envir = buf)
    vec[pos:(pos + len - 1)] <- str
    assign(last, vec, envir = buf)
    assign("pos", pos + len, envir = buf)

  } else {
    last <- as.character(as.integer(last) + 1)
    vec <- c(str, vector(chunk_size - len, mode = "character"))
    assign(last, vec, envir = buf)
    assign("last", last, envir = buf)
    assign("pos", len + 1, envir = buf)
  }
}

buf_get <- function() {
  vecs <- setdiff(ls(buf), c("last", "pos"))
  vecs <- as.character(sort(as.numeric(vecs)))
  strs <- character(length(vecs))
  for (i in seq_along(vecs)) {
    strs[i] <- paste(get(vecs[i], envir = buf), collapse = "")
  }
  paste(strs, collapse = "")
}
