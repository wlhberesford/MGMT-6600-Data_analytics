##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)


## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

X <- wine[,-1]
Y <- wine$Type

## ── 1. Compute PCA & plot PC1 vs PC2 ──────────────────────────────────────────

# Scale features (required: variables have very different units/ranges)
pca <- princomp(scale(X))

# PC scores (n × p matrix; each row is one wine observation in PC space)
scores <- as.data.frame(pca$scores)
scores$Type <- Y

ggplot(scores, aes(x = Comp.1, y = Comp.2, color = Type)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Wine Dataset – PC1 vs PC2",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

## ── 2. Variables contributing most to PC1 ─────────────────────────────────────

# Loadings: each column is a PC; rows are original variables
pc1_loadings <- sort(abs(pca$loadings[, 1]), decreasing = TRUE)
print("PC1 loadings (sorted by absolute value):")
print(round(pc1_loadings, 4))

# Bar chart of PC1 loadings
loadings_df <- data.frame(
  Variable = names(pc1_loadings),
  Loading  = pc1_loadings
)
ggplot(loadings_df, aes(x = reorder(Variable, Loading), y = Loading)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Absolute Loadings on PC1",
       x = NULL, y = "|Loading|") +
  theme_minimal()

## ── 3. kNN on a subset of original variables ──────────────────────────────────
# Top-4 contributors to PC1: Flavanoids, Total phenols, Od280/od315, Proline
subset_vars <- c("Flavanoids", "Total phenols", "Od280/od315 of diluted wines", "Proline")

set.seed(42)
n        <- nrow(wine)
train_idx <- sample(seq_len(n), size = floor(0.7 * n))

X_sub        <- scale(X[, subset_vars])
X_sub_train  <- X_sub[train_idx, ]
X_sub_test   <- X_sub[-train_idx, ]
Y_train      <- Y[train_idx]
Y_test       <- Y[-train_idx]

knn_sub_pred <- knn(train = X_sub_train,
                    test  = X_sub_test,
                    cl    = Y_train,
                    k     = 5)

## ── 4. kNN on first 2 PCs ─────────────────────────────────────────────────────

X_pc        <- scores[, c("Comp.1", "Comp.2")]
X_pc_train  <- X_pc[train_idx, ]
X_pc_test   <- X_pc[-train_idx, ]

knn_pc_pred <- knn(train = X_pc_train,
                   test  = X_pc_test,
                   cl    = Y_train,
                   k     = 5)

## ── 5. Compare models: contingency tables + precision / recall / F1 ───────────

# Helper: compute per-class precision, recall, F1 from a confusion matrix
prf_metrics <- function(cm) {
  classes <- rownames(cm)
  metrics <- sapply(classes, function(cls) {
    tp <- cm[cls, cls]
    fp <- sum(cm[, cls]) - tp
    fn <- sum(cm[cls, ]) - tp
    prec   <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
    rec    <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    f1     <- ifelse((prec + rec) == 0, 0, 2 * prec * rec / (prec + rec))
    c(Precision = prec, Recall = rec, F1 = f1)
  })
  as.data.frame(t(metrics))
}

# --- Subset-variable model ---
cm_sub <- table(Actual = Y_test, Predicted = knn_sub_pred)
cat("\n=== Contingency Table: kNN on Subset Variables ===\n")
print(cm_sub)
cat("\nPer-class metrics:\n")
print(round(prf_metrics(cm_sub), 4))
cat(sprintf("Accuracy: %.4f\n", sum(diag(cm_sub)) / sum(cm_sub)))

# --- PC model ---
cm_pc <- table(Actual = Y_test, Predicted = knn_pc_pred)
cat("\n=== Contingency Table: kNN on First 2 PCs ===\n")
print(cm_pc)
cat("\nPer-class metrics:\n")
print(round(prf_metrics(cm_pc), 4))
cat(sprintf("Accuracy: %.4f\n", sum(diag(cm_pc)) / sum(cm_pc)))