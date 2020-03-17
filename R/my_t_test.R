#' my_t_test
#'
#' Performs a one sample t-test on data.
#'
#' @param x A numeric vector of data.
#' @param alternative A character string specifying the alternative
#'   hypothesis (only accept "two.sided", "less", or "greater").
#' @param mu A number indicating the null hypothesis value of the
#'   mean.
#' @keywords p_value
#'
#' @return A list with elements:\cr
#' \itemize{
#' \item \code{test_stat} The numeric test statistic\cr
#' \item \code{df} The degrees of freedom\cr
#' \item \code{alternative} The value of the parameter \code{alternative}\cr
#' \item \code{p_val} The numeric p-value
#' }
#'
#' @examples
#' x <- rnorm(10, mean = 0, sd = 1)
#' my_t_test(x, alternative="two.sided", mu=0)
#'
#'@export
my_t_test <- function(x, alternative, mu) {
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop("alternative is not valid!")
  }
  ssize <- length(x)
  se <- sd(x) / sqrt(ssize)
  df <- ssize - 1
  t <- (mean(x) - mu) / se
  if (alternative == "two.sided") {
    p_value <- 2 * pt(abs(t), df = df, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_value <- pt(t, df = df, lower.tail = TRUE)
  } else {
    p_value <- pt(t, df = df, lower.tail = FALSE)
  }
  mylist <- list(t, df, alternative, p_value)
  names(mylist) <- c("t", "df", "alternative", "p_value")
  return(mylist)
}
