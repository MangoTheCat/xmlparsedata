
spaces_const <- sapply(1:20, function(x) paste(rep(" ", x), collapse = ""))

spaces <- function(x) {
  if (x > 20) paste(rep(" ", x), collapse = "") else spaces_const[x]
}
