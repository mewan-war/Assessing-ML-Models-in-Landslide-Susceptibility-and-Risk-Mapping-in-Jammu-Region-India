## R Code for:
## Combined ROC Curve Construction for LSI using AHP, LogR, RF and ANN

## Author:
## Mewan Banshan War (mewan.banshan@research.iiit.ac.in)
################################################################################

# Load necessary libraries
library(pROC)
library(readxl)
library(dplyr)

# Set working directory
setwd("D:/IGARSS 2026/Datasets")

############################### Evaluation of 1st Model: AHP ###################
# Load the dataset
data_ahp <- read_xlsx("AHP-LR-ANN-RF.xlsx", sheet = 1)

# Randomly sample 500 records from each class
set.seed(123)  # for reproducibility

data_ahp_balanced <- data_ahp %>%
  group_by(Label) %>%
  sample_n(size = 500, replace = FALSE) %>%
  ungroup()

# Generate ROC curve
roc_curve_ahp <- roc(data_ahp_balanced$Label, data_ahp_balanced$RASTERVALU, direction = "<")

# Compute AUC
auc_value_ahp <- auc(roc_curve_ahp)

# Identify the best threshold
best_coords <- coords(
  roc_curve_ahp, 
  "best",
  ret = c("threshold", "sensitivity", "specificity")
)

# Convert specificity → FPR
best_coords$false_positive_rate <- 1 - best_coords$specificity
best_coords$true_positive_rate  <- best_coords$sensitivity
best_coords <- best_coords[, c("threshold", "true_positive_rate", "false_positive_rate")]
print(best_coords)

############################### Evaluation of 2nd Model: LogR ##################
# Load the dataset
data_logR <- read_xlsx("AHP-LR-ANN-RF.xlsx", sheet = 2)

roc_curve_logR <- data_logR

# Compute AUC
auc_value_logR <- sum(
  diff(roc_curve_logR$False_Positive_Rate) *
    (head(roc_curve_logR$True_Positive_Rate, -1) +
       tail(roc_curve_logR$True_Positive_Rate, -1)) / 2
)

print(auc_value_logR)

############################### Evaluation of 3rd Model: ANN ###################
# Load the dataset
data_ANN <- read_xlsx("AHP-LR-ANN-RF.xlsx", sheet = 3)

roc_curve_ANN <- data_ANN

# Compute AUC
auc_value_ANN <- sum(
  diff(roc_curve_ANN$False_Positive_Rate) *
    (head(roc_curve_ANN$True_Positive_Rate, -1) +
       tail(roc_curve_ANN$True_Positive_Rate, -1)) / 2
)

print(auc_value_ANN)

############################### Evaluation of 4th Model: RF ####################
# Load the dataset
data_RF <- read_xlsx("AHP-LR-ANN-RF.xlsx", sheet = 4)

roc_curve_RF <- data_RF

# Compute AUC
auc_value_RF <- sum(
  diff(roc_curve_RF$False_Positive_Rate) *
    (head(roc_curve_RF$True_Positive_Rate, -1) +
       tail(roc_curve_RF$True_Positive_Rate, -1)) / 2
)

print(auc_value_RF)

# Combine all ROCs in one layout

roc_curve_logR <- roc_curve_logR[order(roc_curve_logR$False_Positive_Rate), ]
roc_curve_ANN  <- roc_curve_ANN[order(roc_curve_ANN$False_Positive_Rate), ]
roc_curve_RF   <- roc_curve_RF[order(roc_curve_RF$False_Positive_Rate), ]

auc_value_logR <- sum(
  diff(roc_curve_logR$False_Positive_Rate) *
    (head(roc_curve_logR$True_Positive_Rate, -1) +
       tail(roc_curve_logR$True_Positive_Rate, -1)) / 2
)

auc_value_ANN <- sum(
  diff(roc_curve_ANN$False_Positive_Rate) *
    (head(roc_curve_ANN$True_Positive_Rate, -1) +
       tail(roc_curve_ANN$True_Positive_Rate, -1)) / 2
)

auc_value_RF <- sum(
  diff(roc_curve_RF$False_Positive_Rate) *
    (head(roc_curve_RF$True_Positive_Rate, -1) +
       tail(roc_curve_RF$True_Positive_Rate, -1)) / 2
)

# Open blank plotting space using first curve (AHP)
plot(
  1 - roc_curve_ahp$specificities,
  roc_curve_ahp$sensitivities,
  type = "l",
  lwd = 3,
  col = "blue",
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "False Positive Rate (1 - Specificity)",
  ylab = "True Positive Rate (Sensitivity)",
  main = "Model Comparison through ROC Curves"
)

# Add remaining curves
lines(roc_curve_logR$False_Positive_Rate,
      roc_curve_logR$True_Positive_Rate,
      col = "orange", lwd = 3)

lines(roc_curve_ANN$False_Positive_Rate,
      roc_curve_ANN$True_Positive_Rate,
      col = "red", lwd = 3)

lines(roc_curve_RF$False_Positive_Rate,
      roc_curve_RF$True_Positive_Rate,
      col = "darkgreen", lwd = 3)

# Add diagonal reference
abline(a = 0, b = 1, lty = 2, col = "gray")

legend(
  "bottomright",
  legend = c(
    paste("AHP (AUC =", round(auc_value_ahp, 3),")"),
    paste("LogR (AUC =", round(auc_value_logR, 3),")"),
    paste("ANN (AUC =", round(auc_value_ANN, 3),")"),
    paste("RF (AUC =", round(auc_value_RF, 3),")")
  ),
  col = c("blue", "orange", "red", "darkgreen"),
  lwd = 3,
  cex = 0.9,
  bty = "n"
)

print(plot)