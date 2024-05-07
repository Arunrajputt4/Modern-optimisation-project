  ## Header: Load required libraries
  library(rpart)  # For decision trees
  library(caret)  # For model evaluation
  
  ## Header: Function to fit decision tree model
  getBenchmark <- function(X, y) {
    # Fit decision tree model
    mod <- rpart(y ~ ., data = data.frame(X), method = "class")
    
    return(mod)
  }
  
  ## Header: Function to get data
  getData <- function() {
    # Load your cleaned dataset
    clean_data <- read.csv("D:/University Learning Support/Arun/Modern Optimization/Project/clean_data.csv")
    
    # Assuming the target variable is the last column
    target_var <- "Approved_Conversion"
    
    # Split data into features and target
    X <- clean_data[, !(names(clean_data) %in% target_var)]  # All columns except target
    y <- clean_data[, target_var]
    
    return(list(X = X, y = y))
  }
    
    ## Header: Feature Fitness Function for Decision Tree
    featureFitness <- function(string, X, y) {
      # print(string)  # Uncomment this line if you want to print every single solution
      inc <- which(string == 1)  # 'inc' includes those features/variables for which 'string' contains 1
      if (length(inc) == 0) return(-10E20)  # If no feature is selected, give a terrible fitness to this solution
      
      # Create a matrix of values for all the variables contained in 'inc'
      X_subset <- X[, inc, drop = FALSE]  # Ensure X_subset is a matrix
      
      # Fit decision tree model on the subset of features
      mod <- rpart(y ~ ., data = data.frame(X_subset, y), method = "class")
      
      # Calculate the accuracy or AUC as the fitness metric
      y_pred <- predict(mod, X_subset, type = "class")
      accuracy <- sum(y_pred == y) / length(y)
      # or
      # auc <- caret::twoClassSummary(y, as.numeric(y_pred), lev = c(0, 1))$ROC
      
      # Maximize accuracy or AUC
      accuracy
      # or
      # auc
    }
