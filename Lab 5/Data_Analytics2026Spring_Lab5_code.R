##############################################
### Support Vector Machine Classification  ###
##############################################

## load libraries
library(e1071)
library(randomForest)
library(readr)
library(ggplot2)

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash",
                 "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid Phenols",
                 "Proanthocyanins", "Color Intensity", "Hue",
                 "Od280/od315 of diluted wines", "Proline")

wine$Type <- as.factor(wine$Type)

## ── Feature subset (top PC1 contributors from Lab 4 PCA) ──────────────────────
# Flavanoids, Total phenols, Od280/od315 of diluted wines, Proline
subset_vars <- c("Flavanoids", "Total phenols", "Od280/od315 of diluted wines", "Proline")

## ── Train / test split ────────────────────────────────────────────────────────
set.seed(42)
n         <- nrow(wine)
train_idx <- sample(seq_len(n), size = floor(0.7 * n))

wine_sub       <- wine[, c("Type", subset_vars)]
wine_sub_train <- wine_sub[train_idx, ]
wine_sub_test  <- wine_sub[-train_idx, ]

## ── 1. Tune & train SVM – Linear kernel ───────────────────────────────────────

tune_linear <- tune.svm(
  Type ~ .,
  data   = wine_sub_train,
  kernel = "linear",
  cost   = c(0.01, 0.1, 1, 10, 100)
)

cat("\n=== Linear SVM Tuning Results ===\n")
print(tune_linear)
cat(sprintf("Best cost: %s\n", tune_linear$best.parameters$cost))

svm_linear <- tune_linear$best.model

## ── 2. Tune & train SVM – Radial (RBF) kernel ─────────────────────────────────

tune_radial <- tune.svm(
  Type ~ .,
  data   = wine_sub_train,
  kernel = "radial",
  cost   = c(0.1, 1, 10, 100),
  gamma  = c(0.01, 0.1, 1, 10)
)

cat("\n=== Radial SVM Tuning Results ===\n")
print(tune_radial)
cat(sprintf("Best cost: %s  |  Best gamma: %s\n",
            tune_radial$best.parameters$cost,
            tune_radial$best.parameters$gamma))

svm_radial <- tune_radial$best.model

## ── 3. Random Forest classifier ────────────────────────────────────────────────

set.seed(42)
rf_model <- randomForest(
  Type ~ .,
  data     = wine_sub_train,
  ntree    = 500,
  mtry     = 2,
  importance = TRUE
)

cat("\n=== Random Forest Model ===\n")
print(rf_model)

## ── 4. Predictions on test set ────────────────────────────────────────────────

pred_linear <- predict(svm_linear, wine_sub_test)
pred_radial <- predict(svm_radial, wine_sub_test)
pred_rf     <- predict(rf_model,   wine_sub_test)

## ── 5. Performance metrics helper ─────────────────────────────────────────────

prf_metrics <- function(cm) {
  classes <- rownames(cm)
  metrics <- sapply(classes, function(cls) {
    tp   <- cm[cls, cls]
    fp   <- sum(cm[, cls]) - tp
    fn   <- sum(cm[cls, ]) - tp
    prec <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
    rec  <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    f1   <- ifelse((prec + rec) == 0, 0, 2 * prec * rec / (prec + rec))
    c(Precision = prec, Recall = rec, F1 = f1)
  })
  df <- as.data.frame(t(metrics))
  df$Macro_avg <- colMeans(df)   # add macro-average row
  df
}

Y_test <- wine_sub_test$Type

## ── 6. Confusion matrices & metrics ──────────────────────────────────────────

cm_linear <- table(Actual = Y_test, Predicted = pred_linear)
cat("\n=== Confusion Matrix: SVM Linear ===\n")
print(cm_linear)
cat("\nPer-class metrics:\n")
metrics_linear <- prf_metrics(cm_linear)
print(round(metrics_linear, 4))
cat(sprintf("Accuracy: %.4f\n", sum(diag(cm_linear)) / sum(cm_linear)))

cm_radial <- table(Actual = Y_test, Predicted = pred_radial)
cat("\n=== Confusion Matrix: SVM Radial ===\n")
print(cm_radial)
cat("\nPer-class metrics:\n")
metrics_radial <- prf_metrics(cm_radial)
print(round(metrics_radial, 4))
cat(sprintf("Accuracy: %.4f\n", sum(diag(cm_radial)) / sum(cm_radial)))

cm_rf <- table(Actual = Y_test, Predicted = pred_rf)
cat("\n=== Confusion Matrix: Random Forest ===\n")
print(cm_rf)
cat("\nPer-class metrics:\n")
metrics_rf <- prf_metrics(cm_rf)
print(round(metrics_rf, 4))
cat(sprintf("Accuracy: %.4f\n", sum(diag(cm_rf)) / sum(cm_rf)))

## ── 7. Side-by-side comparison plot ──────────────────────────────────────────

comparison <- data.frame(
  Model   = rep(c("SVM Linear", "SVM Radial", "Random Forest"), each = 3),
  Metric  = rep(c("Precision", "Recall", "F1"), times = 3),
  Value   = c(
    unlist(metrics_linear["Macro_avg", c("Precision", "Recall", "F1")]),
    unlist(metrics_radial["Macro_avg", c("Precision", "Recall", "F1")]),
    unlist(metrics_rf["Macro_avg",     c("Precision", "Recall", "F1")])
  )
)

ggplot(comparison, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Model Comparison – Macro-Averaged Precision, Recall & F1",
       x = NULL, y = "Score") +
  theme_minimal()
