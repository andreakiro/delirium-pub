# Load required libraries
library(readxl)
library(pROC)
library(ggplot2)

# Function to run the model and return predictions and true labels for one iteration
calculate_predictions <- function(data) {
  # Extract the response variable (first column) and predictors (rest)
  Y <- as.factor(data[[1]])  # Assuming the first column contains the response variable
  X <- as.matrix(data[, -1])  # Assuming columns 2 to ncol(data) contain the predictor variables
  
  # Splitting the data
  train_indices <- sample(1:nrow(data), size = floor(0.8 * nrow(data)))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Extract training and test sets
  X_train <- as.matrix(train_data[, -1])
  Y_train <- as.factor(train_data[[1]])
  X_test <- as.matrix(test_data[, -1])
  Y_test <- as.factor(test_data[[1]])
  
  # Fit model using training data
  cv_fit_train <- cv.glmnet(X_train, Y_train,
                            family = "binomial",
                            alpha = 0, # (lasso = 1; ridge = 0)
                            standardize = FALSE)
  
  # Predict on test set
  predicted_probabilities_test <- predict(cv_fit_train,
                                          newx = X_test,
                                          s = "lambda.min",
                                          type = "response")
  
  # Return the true labels and predicted probabilities as a data frame
  return(data.frame(TrueLabel = Y_test, PredictedProb = as.vector(predicted_probabilities_test)))
}

# Function to perform Monte Carlo Cross-Validation and collect predictions across iterations
monte_carlo_cv_predictions <- function(data, n_repeats) {
  all_predictions <- data.frame(TrueLabel = factor(), PredictedProb = numeric())
  
  for (i in 1:n_repeats) {
    iteration_predictions <- calculate_predictions(data)
    all_predictions <- rbind(all_predictions, iteration_predictions)
  }
  
  return(all_predictions)
}



# Function to calculate performance metrics at the optimal threshold
calculate_metrics <- function(roc_obj) {
  # Find optimal threshold using Youden's J statistic (maximizing Sensitivity + Specificity)
  opt_threshold <- coords(roc_obj, "best", ret="threshold", best.method="youden")
  
  # Get sensitivity, specificity, PPV (positive predictive value), and NPV (negative predictive value)
  sensitivity <- coords(roc_obj, "best", ret="sensitivity", best.method="youden")
  specificity <- coords(roc_obj, "best", ret="specificity", best.method="youden")
  ppv <- coords(roc_obj, "best", ret="ppv", best.method="youden")
  npv <- coords(roc_obj, "best", ret="npv", best.method="youden")
  
  auc_value <- auc(roc_obj)
  
  return(list(AUC = auc_value, Sensitivity = sensitivity, Specificity = specificity, PPV = ppv, NPV = npv, Threshold = opt_threshold))
}

# Load the data for the four datasets
data_all <- read_excel("Delirium_figures.xlsx", sheet = "Classification_all_")
data_cytokine <- read_excel("Delirium_figures.xlsx", sheet = "Classification_cytokines_only")
data_cells <- read_excel("Delirium_figures.xlsx", sheet = "Classification_cells_only")
data_top12 <- read_excel("Delirium_figures.xlsx", sheet = "Classification_small_v3")


# Perform Monte Carlo Cross-Validation and collect predictions for all datasets
n_repeats <- 1000

# All data
pooled_predictions_all <- monte_carlo_cv_predictions(data_all, n_repeats)
roc_pooled_all <- roc(pooled_predictions_all$TrueLabel, pooled_predictions_all$PredictedProb)
metrics_all <- calculate_metrics(roc_pooled_all)

# All cytokines
pooled_predictions_cytokines <- monte_carlo_cv_predictions(data_cytokine, n_repeats)
roc_pooled_cytokines <- roc(pooled_predictions_cytokines$TrueLabel, pooled_predictions_cytokines$PredictedProb)
metrics_cytokines <- calculate_metrics(roc_pooled_cytokines)

# All cells
pooled_predictions_cells <- monte_carlo_cv_predictions(data_cells, n_repeats)
roc_pooled_cells <- roc(pooled_predictions_cells$TrueLabel, pooled_predictions_cells$PredictedProb)
metrics_cells <- calculate_metrics(roc_pooled_cells)

# Top 12
pooled_predictions_top12 <- monte_carlo_cv_predictions(data_top12, n_repeats)
roc_pooled_top12 <- roc(pooled_predictions_top12$TrueLabel, pooled_predictions_top12$PredictedProb)
metrics_top12 <- calculate_metrics(roc_pooled_top12)


# Prepare data for ggplot2
roc_df_all <- data.frame(TPR = roc_pooled_all$sensitivities, FPR = 1 - roc_pooled_all$specificities, Group = "All Data")
roc_df_cytokines <- data.frame(TPR = roc_pooled_cytokines$sensitivities, FPR = 1 - roc_pooled_cytokines$specificities, Group = "Cytokines")
roc_df_cells <- data.frame(TPR = roc_pooled_cells$sensitivities, FPR = 1 - roc_pooled_cells$specificities, Group = "Cells")
roc_df_top12 <- data.frame(TPR = roc_pooled_top12$sensitivities, FPR = 1 - roc_pooled_top12$specificities, Group = "Top 12")

# Combine all dataframes
roc_df <- rbind(roc_df_all, roc_df_cytokines, roc_df_cells, roc_df_top12)

# Plot all ROC curves using ggplot2
ggplot(roc_df, aes(x = FPR, y = TPR, color = Group, linetype = Group)) +
  geom_line(size = 1.2) +  # Adjust the size (thickness) of the lines
  geom_abline(linetype = "dashed", color = "black") +
  labs(title = "Pooled ROC Curves", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal() +
  
  scale_color_manual(values = c("gray", "plum2", "steelblue3", "salmon")) +  # Customize colors
  theme(legend.title = element_blank())+
  theme(panel.grid.minor = element_blank()) # Remove minor grid lines

