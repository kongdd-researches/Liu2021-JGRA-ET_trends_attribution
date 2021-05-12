#' @export
sign_agree <- function(x, y) {
  tbl <- table(sign(x), sign(y))
  {tbl[c(1, 4)]/sum(tbl)} %>% sum()
}
