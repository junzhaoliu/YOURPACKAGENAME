#' my_rf_cv
#'
#' Uses Random Forest Cross-Validation method to predict
#' a output class using covariates.
#'
#' @param k number of folds.
#' @keywords Random Forest
#'
#' @return A numeric with the cross-validation error
#'
#' @examples
#' my_rf_cv(5)
#'
#'@export
my_rf_cv <- function(k) {
  folds <- sample(rep(1:k, length = length(lifeExp)))
  y <- my_gapminder$lifeExp
  x1 <- my_gapminder$gdpPercap
  data <- data.frame("x1" = x1, "y" = y, "split" = folds)
  misrates <- numeric(k)
  for (i in 1:k) {
    data_train <- data %>% filter(split != i)
    data_test <- data %>% filter(split == i)

    model <- randomForest(y ~ x1, data = data_train, ntree = 100)
    y_test_hat <- as.data.frame(predict(model, data_test))

    misrates[i] <- colMeans((y_test_hat - data_test$y)^2)
  }
  MSE <- mean(misrates)
  return(MSE)
}
