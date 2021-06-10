#' random forest cross validation
#'
#' This function applies random forest cross validation to penguins
#' data frame given number of folds
#'
#' @param k Numeric indicating number of folds
#' @keywords prediction
#'
#' @return Numeric of mean squared error
#'
#' @examples
#' k <- 5
#' my_rf_cv(k)
#'
#' @export
my_rf_cv <- function(k) {
  # Load in penguin data
  penguins <- stats::na.omit(read.csv("../Data/my_penguins.csv"))[, -1]

  # Generate random folds
  fold <- sample(rep(1:k, length = nrow(penguins)))

  # Assign random folds to each row
  data <- cbind(penguins[, 3:6], "fold" = fold)

  # To store mse of each fold
  mse_list <- c()

  for (i in 1:k) {
    data_train <- data[data$fold != i, ] # Select all other folds
    data_test <- data[data$fold == i, ] # Select single fold

    data_train_mass <- data_train$body_mass_g # Select the target output
    data_test_mass <- data_test$body_mass_g # Select the target output

    # Random forest model
    tree <- randomForest::randomForest(
      body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = data_train, ntree = 100)

    # Record predictions of model
    pred <- stats::predict(tree, data_test[1:3])

    # Compute mean squared error of predictions
    mse <- (pred - data_test_mass)^2

    # Store error
    mse_list[i] <- mean(mse)
  }
  
  # Return the mean of result list
  return(mean(mse_list))
}
