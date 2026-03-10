# 05 - PERFORMANCE AUDIT & EVALUATION

library(ggplot2)
library(dplyr)

#Mean Absolute Error
# MAE provides the average absolute dollar amount the model is off by.
calc_mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

mae_lm <- calc_mae(y_test, lm_pred)
mae_subset <- calc_mae(y_test, subset_pred)
mae_ridge <- calc_mae(y_test, ridge_pred)
mae_lasso <- calc_mae(y_test, lasso_pred)
mae_pcr <- calc_mae(y_test, as.vector(pcr_pred))
mae_pls <- calc_mae(y_test, as.vector(pls_pred))
mae_tree <- calc_mae(y_test, tree_pred)
mae_rf <- calc_mae(y_test, rf_pred)

audit_results <- final_results %>%
  mutate(MAE = c(mae_lm, mae_subset, mae_ridge, mae_lasso, mae_pcr, mae_pls, mae_tree, mae_rf))

# Sort by lowest RMSE to identify the winner
audit_results <- audit_results[order(audit_results$RMSE), ]
print("--- FINAL AUDITED SUPERVISED MODEL PERFORMANCE ---")
print(audit_results)


# Plot1: Bar chart comparing RMSE
plot_rmse <- ggplot(audit_results, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_viridis_d(option = "mako") +
  theme_minimal() +
  labs(title = "Model Performance Comparison (Lower is Better)", 
       x = "Model", y = "Root Mean Squared Error (RMSE)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_rmse)

# Plot2: Actual vs Predicted (Using Forward Subset)
# Assuming Forward Subset remains the best or near-best model
plot_data <- data.frame(Actual = y_test, Predicted = as.vector(subset_pred))

plot_actual_pred <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "#2C7BB6") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  theme_minimal() +
  labs(title = "Actual vs. Predicted Revenue (Best Model: Forward Subset)", 
       subtitle = "Dots closer to the red dashed line indicate higher accuracy",
       x = "Actual Revenue ($)", y = "Predicted Revenue ($)")

print(plot_actual_pred)

# Plot3: Residuals Plot
plot_data$Residuals <- plot_data$Actual - plot_data$Predicted

plot_residuals <- ggplot(plot_data, aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  theme_minimal() +
  labs(title = "Residual Plot (Best Model: Forward Subset)", 
       subtitle = "Checking for homoscedasticity and uncaptured patterns",
       x = "Predicted Revenue", y = "Residual Error")

print(plot_residuals)