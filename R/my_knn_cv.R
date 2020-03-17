#' my_knn_cv
#'
#' Uses k-Nearest Neighbors Cross-Validation method to predict
#' a output class using covariates.
#'
#' @param train Input data frame.
#' @param cl True class value of your training data.
#' @param k_nn Integer representing the number of neighbors
#' @param k_cv Integer representing the number of folds
#' @keywords k-Nearest Neighbors
#'
#' @return A list with elements:\cr
#' \itemize{
#' \item \code{class} A vector of the predicted class Y^i for
#' all observations\cr
#' \item \code{cv_err} A numeric with the cross-validation
#' misclassification error
#' }
#'
#' @examples
#' train <- iris[,1:4]
#' cl <- iris$Species
#' my_knn_cv(train, cl, 1, 5)
#'
#'@export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  set.seed(302)
  folds <- sample(rep(1:k_cv, length = nrow(train)))
  data <- data.frame("x" = train, "y" = cl, "split" = folds)
  class <- list()
  misclas_rates <- numeric(k_cv)
  for (i in 1:k_cv) {
    data_train <- data %>% filter(split != i)
    data_test <- data %>% filter(split == i)

    #x_i
    x_train <- data_train[, 1:ncol(train)]
    #x_i*
    x_test <- data_test[, 1:ncol(train)]
    #y_i
    y_train <- data_train[, ncol(data)-1]
    #y_i*
    y_test <- data_test[, ncol(data)-1]

    y_test_hat <- knn(train = x_train, cl = y_train, test = x_test, k = k_nn)

    counts <- 0
    y_true_v <- as.vector(y_test)
    y_test_v <- as.vector(y_test_hat)
    for (j in 1:length(y_true_v)) {
      if (y_true_v[j] != y_test_v[j]){
        counts <- counts + 1
      }
    }
    misclas_rate <- counts / length(y_true_v)

    misclas_rates[i] <- misclas_rate
  }
  class <- knn(train = train, cl = cl, test = train, k = k_nn)
  cv_Err <- mean(misclas_rates)
  mylist <- list(class, cv_Err)
  names(mylist) <- c("class", "cv_Err")
  return(mylist)
}
