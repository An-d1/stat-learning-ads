ads_data <- readRDS("../data/cleaned/global_ads_performance_dataset_cleaned.rds")
target <- "revenue"

# Remove leakage variables
ads_data$date <- NULL
ads_data$ROAS <- NULL

ads_data <- na.omit(ads_data)

full_formula <- as.formula(paste(target, "~ ."))

set.seed(123)

n <- nrow(ads_data)
train_idx <- sample(1:n, size = floor(0.75*n))

train_data <- ads_data[train_idx, ]
test_data  <- ads_data[-train_idx, ]

predictors <- setdiff(names(train_data), target)
p <- length(predictors)
p

library(utils)  # for combn

best_models <- list()
results <- data.frame()

for (k in 1:p) {
  
  combos <- combn(predictors, k, simplify = FALSE)
  
  best_rss_k <- Inf
  best_model_k <- NULL
  
  for (vars in combos) {
    
    formula_k <- as.formula(
      paste(target, "~", paste(vars, collapse = "+"))
    )
    
    fit_k <- lm(formula_k, data = train_data)
    
    rss_k <- sum(residuals(fit_k)^2)
    
    if (rss_k < best_rss_k) {
      best_rss_k <- rss_k
      best_model_k <- fit_k
    }
  }
  
  best_models[[k]] <- best_model_k
  
  results <- rbind(results,
                   data.frame(
                     k = k,
                     RSS = best_rss_k,
                     R2 = summary(best_model_k)$r.squared,
                     AdjR2 = summary(best_model_k)$adj.r.squared,
                     AIC = AIC(best_model_k),
                     BIC = BIC(best_model_k)
                   ))
  
  cat("Finished k =", k, "\n")
}

results


par(mfrow = c(1,2))

# RSS plot
plot(results$k, results$RSS,
     type = "b",
     pch = 19,
     xlab = "Number of Predictors",
     ylab = "Residual Sum of Squares")

# R² plot
plot(results$k, results$R2,
     type = "b",
     pch = 19,
     xlab = "Number of Predictors",
     ylab = "R²")



full_model <- best_models[[p]]
sigma2_hat <- sum(residuals(full_model)^2) / df.residual(full_model)
results$Cp <- (results$RSS + 2*results$k*sigma2_hat) / nrow(train_data)

results
par(mfrow = c(1,3))

# Cp
plot(results$k, results$Cp,
     type="b",
     pch=19,
     xlab="Number of Predictors",
     ylab="Cp")

# BIC
plot(results$k, results$BIC,
     type="b",
     pch=19,
     xlab="Number of Predictors",
     ylab="BIC")

# Adjusted R²
plot(results$k, results$AdjR2,
     type="b",
     pch=19,
     xlab="Number of Predictors",
     ylab="Adjusted R²")


validation_errors <- c()

for (k in 1:p) {
  
  model_k <- best_models[[k]]
  
  preds <- predict(model_k, newdata = test_data)
  
  mse <- mean((test_data[[target]] - preds)^2)
  
  validation_errors[k] <- mse
}

plot(1:p, validation_errors,
     type="b",
     pch=19,
     xlab="Number of Predictors",
     ylab="Validation Set MSE")



set.seed(123)

K <- 10
folds <- sample(rep(1:K, length.out = nrow(train_data)))

cv_errors <- matrix(NA, K, p)

for (j in 1:K) {
  
  train_fold <- train_data[folds != j, ]
  valid_fold <- train_data[folds == j, ]
  
  # Recompute best subset inside fold
  predictors_fold <- setdiff(names(train_fold), target)
  
  for (k in 1:p) {
    
    combos <- combn(predictors_fold, k, simplify = FALSE)
    
    best_rss_k <- Inf
    best_model_k <- NULL
    
    for (vars in combos) {
      formula_k <- as.formula(
        paste(target, "~", paste(vars, collapse="+"))
      )
      
      fit_k <- lm(formula_k, data=train_fold)
      rss_k <- sum(residuals(fit_k)^2)
      
      if (rss_k < best_rss_k) {
        best_rss_k <- rss_k
        best_model_k <- fit_k
      }
    }
    
    preds <- predict(best_model_k, newdata=valid_fold)
    mse <- mean((valid_fold[[target]] - preds)^2)
    
    cv_errors[j,k] <- mse
  }
}

cv_mean <- colMeans(cv_errors)

plot(1:p, cv_mean,
     type="b",
     pch=19,
     xlab="Number of Predictors",
     ylab="Cross-Validation MSE")

cv_sd <- apply(cv_errors, 2, sd)
cv_se <- cv_sd / sqrt(K)

min_cv <- min(cv_mean)
min_k <- which.min(cv_mean)

threshold <- min_cv + cv_se[min_k]

which(cv_mean <= threshold)

preds_test <- predict(final_model, newdata = test_data)

rmse <- sqrt(mean((test_data$revenue - preds_test)^2))
r2_test <- 1 - sum((test_data$revenue - preds_test)^2) /
  sum((test_data$revenue - mean(test_data$revenue))^2)

rmse
r2_test

formula(best_models[[1]])


final_model <- best_models[[1]]

summary(final_model)




############################################################
# DATA PREPARATION
############################################################

# Load dataset
ads_data <- readRDS("../data/cleaned/global_ads_performance_dataset_cleaned.rds")

# Target variable
target <- "revenue"

# Remove leakage variables
# - date: not meaningful for regression
# - ROAS: directly derived from revenue (data leakage)
ads_data$date <- NULL
ads_data$ROAS <- NULL

# Remove missing values
ads_data <- na.omit(ads_data)

# Define full regression formula
full_formula <- as.formula(paste(target, "~ ."))

# Train/Test split (75% / 25%)
set.seed(123)

n <- nrow(ads_data)
train_idx <- sample(1:n, size = floor(0.75*n))

train_data <- ads_data[train_idx, ]
test_data  <- ads_data[-train_idx, ]

# Extract predictor names
predictors <- setdiff(names(train_data), target)
p <- length(predictors)

p

############################################################
# BEST SUBSET SELECTION
############################################################

library(utils)

best_models <- list()
results <- data.frame()

for (k in 1:p) {
  
  combos <- combn(predictors, k, simplify = FALSE)
  
  best_rss_k <- Inf
  best_model_k <- NULL
  
  for (vars in combos) {
    
    formula_k <- as.formula(
      paste(target, "~", paste(vars, collapse = "+"))
    )
    
    fit_k <- lm(formula_k, data = train_data)
    rss_k <- sum(residuals(fit_k)^2)
    
    if (rss_k < best_rss_k) {
      best_rss_k <- rss_k
      best_model_k <- fit_k
    }
  }
  
  best_models[[k]] <- best_model_k
  
  results <- rbind(results,
                   data.frame(
                     k = k,
                     RSS = best_rss_k,
                     R2 = summary(best_model_k)$r.squared,
                     AdjR2 = summary(best_model_k)$adj.r.squared,
                     AIC = AIC(best_model_k),
                     BIC = BIC(best_model_k)
                   ))
  
  cat("Finished k =", k, "\n")
}

results

############################################################
# TRAINING ERROR PLOTS
############################################################

par(mfrow = c(1,2))

plot(results$k, results$RSS,
     type="b", pch=19,
     xlab="Number of Predictors",
     ylab="Residual Sum of Squares")

plot(results$k, results$R2,
     type="b", pch=19,
     xlab="Number of Predictors",
     ylab="R²")

############################################################
# MODEL SELECTION CRITERIA
############################################################

# Estimate sigma^2 from full model
full_model <- best_models[[p]]
sigma2_hat <- sum(residuals(full_model)^2) / df.residual(full_model)

# Compute Cp
results$Cp <- (results$RSS + 2*results$k*sigma2_hat) / nrow(train_data)

par(mfrow = c(1,3))

plot(results$k, results$Cp,
     type="b", pch=19,
     xlab="Number of Predictors",
     ylab="Cp")

plot(results$k, results$BIC,
     type="b", pch=19,
     xlab="Number of Predictors",
     ylab="BIC")

plot(results$k, results$AdjR2,
     type="b", pch=19,
     xlab="Number of Predictors",
     ylab="Adjusted R²")


############################################################
# VALIDATION SET APPROACH
############################################################

validation_errors <- c()

for (k in 1:p) {
  preds <- predict(best_models[[k]], newdata=test_data)
  validation_errors[k] <- mean((test_data[[target]] - preds)^2)
}

plot(1:p, validation_errors,
     type="b", pch=19,
     xlab="Number of Predictors",
     ylab="Validation Set MSE")

############################################################
# 10-FOLD CROSS VALIDATION
############################################################

set.seed(123)

K <- 10
folds <- sample(rep(1:K, length.out = nrow(train_data)))

cv_errors <- matrix(NA, K, p)

for (j in 1:K) {
  
  train_fold <- train_data[folds != j, ]
  valid_fold <- train_data[folds == j, ]
  
  predictors_fold <- setdiff(names(train_fold), target)
  
  for (k in 1:p) {
    
    combos <- combn(predictors_fold, k, simplify=FALSE)
    
    best_rss_k <- Inf
    best_model_k <- NULL
    
    for (vars in combos) {
      formula_k <- as.formula(
        paste(target, "~", paste(vars, collapse="+"))
      )
      
      fit_k <- lm(formula_k, data=train_fold)
      rss_k <- sum(residuals(fit_k)^2)
      
      if (rss_k < best_rss_k) {
        best_rss_k <- rss_k
        best_model_k <- fit_k
      }
    }
    
    preds <- predict(best_model_k, newdata=valid_fold)
    cv_errors[j,k] <- mean((valid_fold[[target]] - preds)^2)
  }
}

cv_mean <- colMeans(cv_errors)

plot(1:p, cv_mean,
     type="b", pch=19,
     xlab="Number of Predictors",
     ylab="Cross-Validation MSE")

############################################################
# ONE-STANDARD-ERROR RULE
############################################################

cv_sd <- apply(cv_errors, 2, sd)
cv_se <- cv_sd / sqrt(K)

min_k <- which.min(cv_mean)
threshold <- min(cv_mean) + cv_se[min_k]

candidate_k <- which(cv_mean <= threshold)
final_k_subset <- min(candidate_k)

final_subset_model <- best_models[[final_k_subset]]

formula(final_subset_model)

############################################################
# FINAL SUBSET MODEL PERFORMANCE
############################################################

subset_preds_test <- predict(final_subset_model, newdata=test_data)

rmse_subset <- sqrt(mean((test_data[[target]] - subset_preds_test)^2))

r2_subset <- 1 - sum((test_data[[target]] - subset_preds_test)^2) /
  sum((test_data[[target]] - mean(test_data[[target]]))^2)

rmse_subset
r2_subset

############################################################
# 2. SHRINKAGE
############################################################

# Create model matrix (dummy variables handled automatically)
X_train <- model.matrix(full_formula, train_data)[,-1]
X_test  <- model.matrix(full_formula, test_data)[,-1]

y_train <- train_data[[target]]
y_test  <- test_data[[target]]

# Standardize predictors (important!)
x_means <- colMeans(X_train)
x_sds   <- apply(X_train, 2, sd)
x_sds[x_sds == 0] <- 1

X_train_s <- sweep(X_train, 2, x_means, "-")
X_train_s <- sweep(X_train_s, 2, x_sds, "/")

X_test_s <- sweep(X_test, 2, x_means, "-")
X_test_s <- sweep(X_test_s, 2, x_sds, "/")

# Center response
y_mean <- mean(y_train)
y_center <- y_train - y_mean

############################################################
# RIDGE REGRESSION
############################################################

lambda_grid <- 10^seq(-4,6,length.out=60)

ridge_cv <- c()

for (lam in lambda_grid) {
  
  beta <- solve(t(X_train_s)%*%X_train_s + 
                  lam*diag(ncol(X_train_s))) %*% 
    t(X_train_s)%*%y_center
  
  preds <- X_train_s %*% beta + y_mean
  
  ridge_cv <- c(ridge_cv,
                mean((y_train - preds)^2))
}

best_lambda_ridge <- lambda_grid[which.min(ridge_cv)]

plot(lambda_grid, ridge_cv, type="l",
     log="x",
     xlab="Lambda",
     ylab="Training MSE",
     main="Ridge CV Curve")

beta_ridge <- solve(t(X_train_s)%*%X_train_s + 
                      best_lambda_ridge*diag(ncol(X_train_s))) %*% 
  t(X_train_s)%*%y_center

ridge_preds_test <- X_test_s %*% beta_ridge + y_mean

rmse_ridge <- sqrt(mean((y_test - ridge_preds_test)^2))
r2_ridge <- 1 - sum((y_test - ridge_preds_test)^2) /
  sum((y_test - mean(y_test))^2)

rmse_ridge
r2_ridge

############################################################
# LASSO REGRESSION
############################################################

soft_threshold <- function(z, gamma) {
  sign(z) * pmax(abs(z) - gamma, 0)
}

lasso_cd <- function(X, y, lambda, max_iter=1000) {
  
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)
  
  for (iter in 1:max_iter) {
    for (j in 1:p) {
      
      r_j <- y - X %*% beta + X[,j]*beta[j]
      rho <- sum(X[,j]*r_j)
      
      beta[j] <- soft_threshold(rho/n, lambda) /
        (sum(X[,j]^2)/n)
    }
  }
  
  return(beta)
}

lasso_cv <- c()

for (lam in lambda_grid) {
  
  beta <- lasso_cd(X_train_s, y_center, lam)
  preds <- X_train_s %*% beta + y_mean
  
  lasso_cv <- c(lasso_cv,
                mean((y_train - preds)^2))
}

best_lambda_lasso <- lambda_grid[which.min(lasso_cv)]

plot(lambda_grid, lasso_cv, type="l",
     log="x",
     xlab="Lambda",
     ylab="Training MSE",
     main="Lasso CV Curve")

beta_lasso <- lasso_cd(X_train_s, y_center, best_lambda_lasso)

lasso_preds_test <- X_test_s %*% beta_lasso + y_mean

rmse_lasso <- sqrt(mean((y_test - lasso_preds_test)^2))
r2_lasso <- 1 - sum((y_test - lasso_preds_test)^2) /
  sum((y_test - mean(y_test))^2)

rmse_lasso
r2_lasso

############################################################
# ELASTIC NET
############################################################

elastic_cd <- function(X, y, lambda, alpha, max_iter=1000) {
  
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)
  
  for (iter in 1:max_iter) {
    for (j in 1:p) {
      
      r_j <- y - X %*% beta + X[,j]*beta[j]
      rho <- sum(X[,j]*r_j)
      
      beta[j] <- soft_threshold(rho/n, lambda*alpha) /
        ((sum(X[,j]^2)/n) + lambda*(1-alpha))
    }
  }
  
  return(beta)
}

alpha_val <- 0.5

enet_cv <- c()

for (lam in lambda_grid) {
  
  beta <- elastic_cd(X_train_s, y_center,
                     lam, alpha_val)
  
  preds <- X_train_s %*% beta + y_mean
  
  enet_cv <- c(enet_cv,
               mean((y_train - preds)^2))
}

best_lambda_enet <- lambda_grid[which.min(enet_cv)]

beta_enet <- elastic_cd(X_train_s, y_center,
                        best_lambda_enet, alpha_val)

enet_preds_test <- X_test_s %*% beta_enet + y_mean

rmse_enet <- sqrt(mean((y_test - enet_preds_test)^2))
r2_enet <- 1 - sum((y_test - enet_preds_test)^2) /
  sum((y_test - mean(y_test))^2)

rmse_enet
r2_enet

shrinkage_results <- data.frame(
  Model = c("Ridge","Lasso","Elastic Net"),
  RMSE = c(rmse_ridge,
           rmse_lasso,
           rmse_enet),
  R2 = c(r2_ridge,
         r2_lasso,
         r2_enet)
)

shrinkage_results

############################################################
# PRETTIER RIDGE PATH PLOTS (base R only)
# Requires: B_ridge (p x L), lambda_grid, norm_ratio
############################################################

# Choose which variables to highlight (top 4 by abs coef at smallest lambda)
top_k <- 4
idx_small_lam <- 1
top_vars <- order(abs(B_ridge[, idx_small_lam]), decreasing = TRUE)[1:top_k]
highlight_names <- rownames(B_ridge)[top_vars]

is_highlight <- rownames(B_ridge) %in% highlight_names
gray_idx <- which(!is_highlight)
hi_idx <- which(is_highlight)

# Give highlight lines distinct linetypes (works in B/W prints too)
hi_lty <- c(1, 2, 3, 4)[seq_along(hi_idx)]
hi_lwd <- 2.5
gray_lwd <- 1

# Put legend outside the plot area
op <- par(no.readonly = TRUE)
par(mfrow = c(1,2),
    mar = c(5, 5, 4, 9),   # extra right margin for legend
    xaxs = "i", yaxs = "i")

## ---- LEFT: coefficients vs lambda (log scale) ----
plot(NA, NA,
     xlim = range(lambda_grid),
     ylim = range(B_ridge),
     log = "x",
     xlab = expression(lambda),
     ylab = "Standardized Coefficients",
     main = "Ridge: Coefficient Paths")

abline(h = 0, lty = 3, lwd = 1)

# All other coefficients (thin gray)
matlines(lambda_grid, t(B_ridge[gray_idx, , drop = FALSE]),
         lty = 1, lwd = gray_lwd, col = "grey75")

# Highlighted coefficients (thicker, colored)
cols_hi <- c("black", "blue3", "red3", "darkgreen")[seq_along(hi_idx)]
for (i in seq_along(hi_idx)) {
  lines(lambda_grid, B_ridge[hi_idx[i], ],
        lwd = hi_lwd, lty = hi_lty[i], col = cols_hi[i])
}

legend("right",
       inset = c(-0.25, 0),
       legend = highlight_names,
       lty = hi_lty, lwd = hi_lwd, col = cols_hi,
       xpd = TRUE, bty = "n", cex = 1)

## ---- RIGHT: coefficients vs L2 norm ratio ----
o <- order(norm_ratio)

plot(NA, NA,
     xlim = range(norm_ratio),
     ylim = range(B_ridge),
     xlab = expression(frac(norm(hat(beta)[lambda], 2), norm(hat(beta)[0], 2))),
     ylab = "Standardized Coefficients",
     main = "Ridge: Paths vs L2 Norm Ratio")

abline(h = 0, lty = 3, lwd = 1)

# All other coefficients (thin gray)
matlines(norm_ratio[o], t(B_ridge[gray_idx, o, drop = FALSE]),
         lty = 1, lwd = gray_lwd, col = "grey75")

# Highlighted coefficients (thicker)
for (i in seq_along(hi_idx)) {
  lines(norm_ratio[o], B_ridge[hi_idx[i], o],
        lwd = hi_lwd, lty = hi_lty[i], col = cols_hi[i])
}

legend("right",
       inset = c(-0.25, 0),
       legend = highlight_names,
       lty = hi_lty, lwd = hi_lwd, col = cols_hi,
       xpd = TRUE, bty = "n", cex = 1)

par(op)


############################################################
# PCA
############################################################

X_train <- model.matrix(full_formula, train_data)[,-1]
X_test  <- model.matrix(full_formula, test_data)[,-1]

# Standardize
x_means <- colMeans(X_train)
x_sds   <- apply(X_train,2,sd)
x_sds[x_sds==0] <- 1

X_train_s <- sweep(sweep(X_train,2,x_means,"-"),2,x_sds,"/")
X_test_s  <- sweep(sweep(X_test,2,x_means,"-"),2,x_sds,"/")

pca <- prcomp(X_train_s, center=FALSE, scale.=FALSE)

# Proportion of variance explained
var_explained <- pca$sdev^2 / sum(pca$sdev^2)

plot(cumsum(var_explained),
     type="b", pch=19,
     xlab="Number of Components",
     ylab="Cumulative Variance Explained")

############################################################
# CORRECT PCR IMPLEMENTATION
############################################################

# Build training data frame for PCR
Z_train_df <- as.data.frame(Z_train[,1:best_m])
Z_train_df$y <- y_train

fit_pcr <- lm(y ~ ., data = Z_train_df)

# Build test data frame
Z_test_df <- as.data.frame(Z_test[,1:best_m])

pcr_preds_test <- predict(fit_pcr, newdata = Z_test_df)

rmse_pcr <- sqrt(mean((y_test - pcr_preds_test)^2))

r2_pcr <- 1 - sum((y_test - pcr_preds_test)^2) /
  sum((y_test - mean(y_test))^2)

rmse_pcr
r2_pcr


############################################################
# PCA PICTURES (2-variable PCA like the slides)
############################################################

# Choose 2 numeric predictors (EDIT THESE if you want)
x_var <- "conversions"
y_var <- "ad_spend"

stopifnot(x_var %in% names(train_data), y_var %in% names(train_data))

# Keep complete cases for these two vars
df2 <- train_data[, c(x_var, y_var)]
df2 <- df2[complete.cases(df2), ]

# Standardize (PCA is sensitive to scale)
X2 <- scale(df2)

# PCA on 2 variables
pca2 <- prcomp(X2, center = FALSE, scale. = FALSE)

# Scores
Z <- pca2$x
z1 <- Z[,1]
z2 <- Z[,2]

# Loadings (directions in standardized space)
v1 <- pca2$rotation[,1]   # PC1 direction (unit vector)
v2 <- pca2$rotation[,2]   # PC2 direction (unit vector)

# Helper: draw a line through origin with direction v, scaled to match data spread
draw_pc_line <- function(v, col, lty, lwd) {
  tvals <- seq(-3, 3, length.out = 200)
  lines(tvals * v[1], tvals * v[2], col = col, lty = lty, lwd = lwd)
}

############################################################
# PLOT 1: Scatter + PC1 (green) + PC2 (blue dashed)
############################################################
par(mfrow = c(1,1), mar=c(5,5,4,2))

plot(X2[,1], X2[,2],
     pch = 19, cex = 0.7,
     xlab = x_var, ylab = y_var,
     main = "PCA: PC1 and PC2 directions (standardized space)")

# PC1 and PC2 lines through origin
draw_pc_line(v1, col="darkgreen", lty=1, lwd=2)
draw_pc_line(v2, col="blue3",     lty=2, lwd=2)

legend("topleft",
       legend = c("PC1", "PC2"),
       col = c("darkgreen", "blue3"),
       lty = c(1,2), lwd = 2, bty="n")


############################################################
# PLOT 2: Perpendicular projections onto PC1
############################################################
plot(X2[,1], X2[,2],
     pch = 19, cex = 0.7,
     xlab = x_var, ylab = y_var,
     main = "PCA: Projections onto PC1")

draw_pc_line(v1, col="darkgreen", lty=1, lwd=2)

# Project each point onto PC1: proj = (x·v1) v1
proj <- (as.vector(X2 %*% v1)) %*% t(v1)   # n x 2

# Draw perpendicular segments
for (i in 1:nrow(X2)) {
  segments(X2[i,1], X2[i,2], proj[i,1], proj[i,2],
           col="grey40", lty=2)
}

# Mark mean/projection point (origin in standardized space)
points(0, 0, pch=19, cex=1.2)


############################################################
# PLOT 3: Rotated view (PC1 scores vs PC2 scores)
############################################################
plot(z1, z2,
     pch=19, cex=0.7,
     xlab = "1st Principal Component",
     ylab = "2nd Principal Component",
     main = "Rotated coordinates: PC1 vs PC2")

abline(h=0, v=0, lty=3, col="grey40")


############################################################
# PLOT 4: PC1 scores vs original variables
############################################################
par(mfrow=c(1,2), mar=c(5,5,4,2))

plot(z1, X2[,1], pch=19, cex=0.7,
     xlab="1st Principal Component", ylab=x_var,
     main=paste("PC1 scores vs", x_var))

plot(z1, X2[,2], pch=19, cex=0.7,
     xlab="1st Principal Component", ylab=y_var,
     main=paste("PC1 scores vs", y_var))


############################################################
# PLOT 5: PC2 scores vs original variables
############################################################
par(mfrow=c(1,2), mar=c(5,5,4,2))

plot(z2, X2[,1], pch=19, cex=0.7,
     xlab="2nd Principal Component", ylab=x_var,
     main=paste("PC2 scores vs", x_var))

plot(z2, X2[,2], pch=19, cex=0.7,
     xlab="2nd Principal Component", ylab=y_var,
     main=paste("PC2 scores vs", y_var))

par(mfrow=c(1,1))