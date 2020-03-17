#' my_lm
#'
#' Fits a linear model in R.
#'
#' @param formula A formula class object.
#' @param data Input data frame.x A numeric vector of data.
#' @keywords linear model
#'
#' @return a table with rows for each coefficient and columns
#' for "Estimate", "Std. Error", "t value", and "p value"
#'
#' @examples
#' data("mtcars")
#' my_lm(formula = mpg ~ hp + wt, data = mtcars)
#'
#'@export
my_lm <- function(formula, data){
  X <- model.matrix(formula, data)
  frame <- model.frame(formula, data)
  Y <- model.response(frame)
  beta_hat <- (solve((t(X) %*% X ))) %*% t(X) %*% Y
  df <- nrow(data) - length(beta_hat)
  sigma_square <- sum((Y - X %*% beta_hat)^2 / df)
  s_e <- diag(sqrt(sigma_square * solve(t(X) %*% X)))
  t <- beta_hat /s_e
  P_val <- 2 * pt(abs(t), df,  lower.tail = FALSE)
  table <- data.frame("Estimate" = beta_hat,
                      "Std.error" = s_e,
                      "t value" = t,
                      "Pr(>|t|)" = P_val)
  return(table)
}
