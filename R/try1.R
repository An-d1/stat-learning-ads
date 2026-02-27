############################################################
# DATA PREPARATION (LOG-TRANSFORMED VERSION)
############################################################

ads_data <- readRDS("../data/cleaned/global_ads_performance_dataset_cleaned.rds")

# Remove leakage variables
ads_data$date <- NULL
ads_data$ROAS <- NULL

ads_data <- na.omit(ads_data)

# Save original revenue for final evaluation
ads_data$revenue_original <- ads_data$revenue

############################################################
# LOG TRANSFORM TARGET
############################################################

ads_data$log_revenue <- log1p(ads_data$revenue)
ads_data$revenue <- NULL
target <- "log_revenue"

############################################################
# LOG TRANSFORM SKEWED NUMERIC VARIABLES
############################################################

numeric_vars <- c("ad_spend","CPC","impressions","clicks","conversions")

for (var in numeric_vars) {
  if (var %in% names(ads_data)) {
    ads_data[[paste0("log_",var)]] <- log1p(ads_data[[var]])
    ads_data[[var]] <- NULL
  }
}

############################################################
# TRAIN / TEST SPLIT
############################################################

set.seed(123)

n <- nrow(ads_data)
train_idx <- sample(1:n, floor(0.75*n))

train_data <- ads_data[train_idx, ]
test_data  <- ads_data[-train_idx, ]

############################################################
# BEST SUBSET SELECTION
############################################################

predictors <- setdiff(names(train_data), c(target,"revenue_original"))
p <- length(predictors)

library(utils)

best_models <- list()
validation_errors <- c()

for (k in 1:p) {
  
  combos <- combn(predictors, k, simplify=FALSE)
  
  best_rss <- Inf
  best_model <- NULL
  
  for (vars in combos) {
    
    formula_k <- as.formula(
      paste(target,"~",paste(vars,collapse="+"))
    )
    
    fit <- lm(formula_k, data=train_data)
    rss <- sum(residuals(fit)^2)
    
    if (rss < best_rss) {
      best_rss <- rss
      best_model <- fit
    }
  }
  
  best_models[[k]] <- best_model
  
  preds <- predict(best_model, newdata=test_data)
  validation_errors[k] <- mean((test_data[[target]] - preds)^2)
  
  cat("Finished k =",k,"\n")
}

plot(1:p, validation_errors,
     type="b", pch=19,
     xlab="Number of Predictors",
     ylab="Validation MSE (log-scale)")

best_k <- which.min(validation_errors)
final_subset_model <- best_models[[best_k]]

############################################################
# SUBSET TEST PERFORMANCE (BACK-TRANSFORMED)
############################################################

subset_preds_log <- predict(final_subset_model, newdata=test_data)

subset_preds_real <- expm1(subset_preds_log)

rmse_subset <- sqrt(mean((test_data$revenue_original -
                            subset_preds_real)^2))

r2_subset <- 1 - sum((test_data$revenue_original -
                        subset_preds_real)^2) /
  sum((test_data$revenue_original -
         mean(test_data$revenue_original))^2)

rmse_subset
r2_subset

############################################################
# SHRINKAGE METHODS
############################################################

full_formula <- as.formula(paste(target,"~ . - revenue_original"))

X_train <- model.matrix(full_formula, train_data)[,-1]
X_test  <- model.matrix(full_formula, test_data)[,-1]

y_train <- train_data[[target]]
y_test  <- test_data[[target]]

############################################################
# STANDARDIZATION (VERY IMPORTANT)
############################################################

x_means <- colMeans(X_train)
x_sds   <- apply(X_train,2,sd)
x_sds[x_sds==0] <- 1

X_train_s <- sweep(sweep(X_train,2,x_means,"-"),2,x_sds,"/")
X_test_s  <- sweep(sweep(X_test,2,x_means,"-"),2,x_sds,"/")

y_mean <- mean(y_train)
y_center <- y_train - y_mean

############################################################
# RIDGE
############################################################

lambda_grid <- 10^seq(-4,6,length.out=60)
ridge_mse <- c()

for (lam in lambda_grid) {
  
  beta <- solve(t(X_train_s)%*%X_train_s +
                  lam*diag(ncol(X_train_s))) %*%
    t(X_train_s)%*%y_center
  
  preds <- X_train_s %*% beta + y_mean
  
  ridge_mse <- c(ridge_mse,
                 mean((y_train - preds)^2))
}

best_lambda_ridge <- lambda_grid[which.min(ridge_mse)]

beta_ridge <- solve(t(X_train_s)%*%X_train_s +
                      best_lambda_ridge*diag(ncol(X_train_s))) %*%
  t(X_train_s)%*%y_center

ridge_preds_log <- X_test_s %*% beta_ridge + y_mean
ridge_preds_real <- expm1(ridge_preds_log)

rmse_ridge <- sqrt(mean((test_data$revenue_original -
                           ridge_preds_real)^2))

r2_ridge <- 1 - sum((test_data$revenue_original -
                       ridge_preds_real)^2) /
  sum((test_data$revenue_original -
         mean(test_data$revenue_original))^2)

############################################################
# LASSO
############################################################

soft_threshold <- function(z,gamma) {
  sign(z)*pmax(abs(z)-gamma,0)
}

lasso_cd <- function(X,y,lambda,max_iter=1000) {
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0,p)
  
  for (iter in 1:max_iter) {
    for (j in 1:p) {
      r_j <- y - X%*%beta + X[,j]*beta[j]
      rho <- sum(X[,j]*r_j)
      beta[j] <- soft_threshold(rho/n,lambda) /
        (sum(X[,j]^2)/n)
    }
  }
  beta
}

lasso_mse <- c()

for (lam in lambda_grid) {
  beta <- lasso_cd(X_train_s,y_center,lam)
  preds <- X_train_s%*%beta + y_mean
  lasso_mse <- c(lasso_mse,
                 mean((y_train-preds)^2))
}

best_lambda_lasso <- lambda_grid[which.min(lasso_mse)]

beta_lasso <- lasso_cd(X_train_s,y_center,best_lambda_lasso)

lasso_preds_log <- X_test_s%*%beta_lasso + y_mean
lasso_preds_real <- expm1(lasso_preds_log)

rmse_lasso <- sqrt(mean((test_data$revenue_original -
                           lasso_preds_real)^2))

r2_lasso <- 1 - sum((test_data$revenue_original -
                       lasso_preds_real)^2) /
  sum((test_data$revenue_original -
         mean(test_data$revenue_original))^2)

############################################################
# ELASTIC NET
############################################################

elastic_cd <- function(X,y,lambda,alpha,max_iter=1000){
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0,p)
  
  for (iter in 1:max_iter){
    for (j in 1:p){
      r_j <- y - X%*%beta + X[,j]*beta[j]
      rho <- sum(X[,j]*r_j)
      beta[j] <- soft_threshold(rho/n,lambda*alpha) /
        ((sum(X[,j]^2)/n)+lambda*(1-alpha))
    }
  }
  beta
}

alpha_val <- 0.5
enet_mse <- c()

for (lam in lambda_grid){
  beta <- elastic_cd(X_train_s,y_center,lam,alpha_val)
  preds <- X_train_s%*%beta + y_mean
  enet_mse <- c(enet_mse,
                mean((y_train-preds)^2))
}

best_lambda_enet <- lambda_grid[which.min(enet_mse)]

beta_enet <- elastic_cd(X_train_s,y_center,
                        best_lambda_enet,alpha_val)

enet_preds_log <- X_test_s%*%beta_enet + y_mean
enet_preds_real <- expm1(enet_preds_log)

rmse_enet <- sqrt(mean((test_data$revenue_original -
                          enet_preds_real)^2))

r2_enet <- 1 - sum((test_data$revenue_original -
                      enet_preds_real)^2) /
  sum((test_data$revenue_original -
         mean(test_data$revenue_original))^2)

############################################################
# FINAL COMPARISON
############################################################

results_final <- data.frame(
  Model=c("Subset","Ridge","Lasso","Elastic Net"),
  RMSE=c(rmse_subset,rmse_ridge,rmse_lasso,rmse_enet),
  R2=c(r2_subset,r2_ridge,r2_lasso,r2_enet)
)

results_final