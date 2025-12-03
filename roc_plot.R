plot_roc_curve <- function(qrisk3, ars, threshold) {
  # Convert risk scores to binary using the threshold
  qrisk3_predictions <- ifelse(qrisk3 > threshold, 1, 0)
  ars_predictions <- ifelse(ars > threshold, 1, 0)
  
  # Calculate sensitivity and specificity based on the predictions
  sensitivity <- sum(qrisk3_predictions == 1 & ars_predictions == 1) / sum(ars_predictions == 1)
  specificity <- sum(qrisk3_predictions == 0 & ars_predictions == 0) / sum(ars_predictions == 0)
  
  # Create ROC curve data frame
  roc_data <- data.frame(Specificity = 1 - specificity, Sensitivity = sensitivity)
  
  # Create ROC curve plot using ggplot2
  roc_plot <- ggplot(roc_data, aes(x = Specificity, y = Sensitivity)) +
    geom_line(color = "blue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    labs(x = "False Positive Rate (1 - Specificity)",
         y = "True Positive Rate (Sensitivity)",
         title = "Receiver Operating Characteristic (ROC) Curve") +
    theme_pubr() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
  
  # Print the plot
  print(roc_plot)
}

# Example usage:
# Assuming qrisk3, ars, and threshold are scalar values
plot_roc_curve(qrisk3, ars, threshold)
