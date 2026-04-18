################################################
#### Evaluating Classification & CLustering ####
################################################

library("caret")
library(GGally)
library(psych)



## read data
abalone <- read.csv("abalone/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]

## separate x (features) & y (class labels)
X <- train[,1:7] 
Y <- train[,8]

## features subset
# train <- train[,5:8]
# test <- test[,5:8]

## feature boxplots
boxplot(X, main="abalone features")

## class label distributions
plot(Y)


## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## psych scatterplot matrix
pairs.panels(X,gap = 0,bg = c("pink", "green", "blue")[Y],pch=21)

## GGally 
ggpairs(train, ggplot2::aes(colour = Y))


## Model 1: Everything

# 7. Train KNN model with cross-validation
control <- trainControl(method = "cv", number = 5)

knn_model <- train(
  x = X,
  y = Y,
  method = "knn",
  tuneLength = 10,
  trControl = control,
  preProcess = c("center", "scale")
)

# 8. Print model summary
print(knn_model)

# 9. Plot tuning results
plot(knn_model)

# 10. Make predictions
predictions <- predict(knn_model, newdata = test)

# 11. Evaluate performance
conf_matrix <- confusionMatrix(predictions, test$age.group)
print(conf_matrix)

# 12. Show best k
cat("\nBest k selected:", knn_model$bestTune$k, "\n")



## Model 2: Backwards Feature Elimination

remaining <- colnames(X)
current_acc <- max(knn_model$results$Accuracy)
cat("Starting accuracy (all features):", round(current_acc, 4), "\n")

while(length(remaining) > 1) {
  accs <- sapply(remaining, function(feat) {
    m <- train(x = X[, setdiff(remaining, feat), drop=FALSE], y = Y,
               method = "knn", tuneLength = 10, trControl = control,
               preProcess = c("center", "scale"))
    max(m$results$Accuracy)
  })
  to_remove <- names(which.max(accs))
  if(max(accs) >= current_acc) {
    current_acc <- max(accs)
    remaining <- setdiff(remaining, to_remove)
    cat("Removed:", to_remove, "| CV Accuracy:", round(current_acc, 4), "\n")
  } else {
    break
  }
}
cat("Selected features:", paste(remaining, collapse = ", "), "\n")

X2 <- X[, remaining, drop=FALSE]
knn_model2 <- train(x = X2, y = Y, method = "knn",
                    tuneLength = 10, trControl = control,
                    preProcess = c("center", "scale"))
print(knn_model2)
plot(knn_model2)

predictions2 <- predict(knn_model2, newdata = test[, remaining, drop=FALSE])
conf_matrix2 <- confusionMatrix(predictions2, test$age.group)
print(conf_matrix2)


## Compare Models
cat("\n--- Model Comparison ---\n")
cat("Model 1 (all 7 features) test accuracy:   ", round(conf_matrix$overall["Accuracy"], 4), "\n")
cat("Model 2 (reduced features) test accuracy: ", round(conf_matrix2$overall["Accuracy"], 4), "\n")

cat("\nModel 1 contingency table:\n")
print(conf_matrix$table)
cat("\nModel 2 contingency table:\n")
print(conf_matrix2$table)


## Optimal k for better performing model
better_X <- if(conf_matrix2$overall["Accuracy"] >= conf_matrix$overall["Accuracy"]) X2 else X

k_range <- seq(1, 40, by = 2)
k_accuracies <- sapply(k_range, function(k) {
  m <- train(x = better_X, y = Y, method = "knn",
             tuneGrid = data.frame(k = k), trControl = control,
             preProcess = c("center", "scale"))
  max(m$results$Accuracy)
})

best_k <- k_range[which.max(k_accuracies)]
cat("\nOptimal k:", best_k, "| Accuracy:", round(max(k_accuracies), 4), "\n")

plot(k_range, k_accuracies, type = "b", pch = 19,
     xlab = "k", ylab = "CV Accuracy",
     main = paste("Accuracy vs k — best k =", best_k))
abline(v = best_k, col = "red", lty = 2)


## EOF ##
