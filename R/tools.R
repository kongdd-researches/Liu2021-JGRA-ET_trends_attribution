#' @export
sign_agree <- function(x, y) {
  tbl <- table(sign(x), sign(y))
  {tbl[c(1, 4)]/sum(tbl)} %>% sum()
}


# function for parameter of Ellipse
Ellipse <- function(x) {
  x0 =x[2,2]; y0 = x[1,2]
  a1 = x[2,2] - x[2,1]
  b1 = x[1,2] - x[1,1]
  a <- (a1^2 + b1^2)^0.5
  b <- b1*(2^0.5)
  angle = atan(b1/a1)
  data.frame(x0, y0, a, b, angle)
}
