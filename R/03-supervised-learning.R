library(car)
library(leaps)
library(glmnet)
library(pls)
library(rpart)
library(rpart.plot)
library(randomForest)

ads_data <- readRDS("data/cleaned/global_ads_performance_dataset_cleaned.rds")
ads_data$date <- NULL
ads_data$ROAS <- NULL
ads_data <- na.omit(ads_data)

str(ads_data)

# Train/test split
set.seed(123)
n <- nrow(ads_data)
train_id <- sample(seq_len(n), size = floor(0.75 * n))

train_data <- ads_data[train_id, ]
test_data  <- ads_data[-train_id, ]

y_train <- train_data$revenue
y_test  <- test_data$revenue

#Linear model

lm_full <- lm(revenue ~ ., data = train_data)
summary(lm_full)
vif(lm_full)

lm_pred <- predict(lm_full, test_data)

rmse_lm <- sqrt(mean((y_test - lm_pred)^2))
r2_lm <- 1 - sum((y_test - lm_pred)^2) /
  sum((y_test - mean(y_test))^2)

#Forward subset

regfit.fwd <- regsubsets(revenue ~ .,
                         data = train_data,
                         method = "forward",
                         nvmax = ncol(train_data) - 1)

reg.summary <- summary(regfit.fwd)

plot(reg.summary$cp,
     type = "b",
     pch = 19,
     main = "Forward Selection - Cp",
     xlab = "Number of Variables",
     ylab = "Cp")

best_subset_size <- which.min(reg.summary$cp)
coef(regfit.fwd, best_subset_size)

predict.regsubsets <- function(object, newdata, id){
  form  <- as.formula(object$call[[2]])
  mat   <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  mat[,names(coefi)] %*% coefi
}

subset_pred <- predict.regsubsets(regfit.fwd,
                                  test_data,
                                  best_subset_size)


rmse_subset <- sqrt(mean((y_test - subset_pred)^2))
r2_subset <- 1 - sum((y_test - subset_pred)^2) /
  sum((y_test - mean(y_test))^2)

#Ridge

x_train <- model.matrix(revenue ~ . -1, train_data)
x_test  <- model.matrix(revenue ~ . -1, test_data)

cv.ridge <- cv.glmnet(x_train, y_train, alpha = 0)
plot(cv.ridge)
title("Ridge - CV MSE")

ridge_pred <- predict(cv.ridge,
                      s = "lambda.min",
                      newx = x_test)

rmse_ridge <- sqrt(mean((y_test - ridge_pred)^2))
r2_ridge <- 1 - sum((y_test - ridge_pred)^2) /
  sum((y_test - mean(y_test))^2)

#Ridge coefficients

ridge_coefs <- coef(cv.ridge, s = "lambda.min")

cat("Best lambda (Ridge):", cv.ridge$lambda.min, "\n\n")

print(ridge_coefs)

rmse_ridge <- sqrt(mean((y_test - ridge_pred)^2))
r2_ridge <- 1 - sum((y_test - ridge_pred)^2) /
  sum((y_test - mean(y_test))^2)

#Lasso

cv.lasso <- cv.glmnet(x_train, y_train, alpha = 1)
plot(cv.lasso)
title("Lasso - CV MSE")

lasso_pred <- predict(cv.lasso,
                      s = "lambda.min",
                      newx = x_test)

rmse_lasso <- sqrt(mean((y_test - lasso_pred)^2))
r2_lasso <- 1 - sum((y_test - lasso_pred)^2) /
  sum((y_test - mean(y_test))^2)

#Lasso coefficients

lasso_coefs <- coef(cv.lasso, s = "lambda.min")

cat("Best lambda (Lasso):", cv.lasso$lambda.min, "\n\n")

print(lasso_coefs)

rmse_lasso <- sqrt(mean((y_test - lasso_pred)^2))
r2_lasso <- 1 - sum((y_test - lasso_pred)^2) /
  sum((y_test - mean(y_test))^2)

#PCR

pcr.fit <- pcr(revenue ~ .,
               data = train_data,
               scale = TRUE,
               validation = "CV")

msep_pcr <- RMSEP(pcr.fit)
rmse_pcr_cv <- sqrt(msep_pcr$val[1,1,-1])
best_pcr_comp <- which.min(rmse_pcr_cv)

pcr_pred <- predict(pcr.fit,
                    test_data,
                    ncomp = best_pcr_comp)

rmse_pcr <- sqrt(mean((y_test - pcr_pred)^2))
r2_pcr <- 1 - sum((y_test - pcr_pred)^2) /
  sum((y_test - mean(y_test))^2)

validationplot(pcr.fit, val.type="MSEP", main = "PCR - CV MSEP")
abline(v = best_pcr_comp, col="red", lwd=2)

#PLS

pls.fit <- plsr(revenue ~ .,
                data = train_data,
                scale = TRUE,
                validation = "CV")

msep_pls <- RMSEP(pls.fit)
rmse_pls_cv <- sqrt(msep_pls$val[1,1,-1])
best_pls_comp <- which.min(rmse_pls_cv)

pls_pred <- predict(pls.fit,
                    test_data,
                    ncomp = best_pls_comp)

rmse_pls <- sqrt(mean((y_test - pls_pred)^2))
r2_pls <- 1 - sum((y_test - pls_pred)^2) /
  sum((y_test - mean(y_test))^2)

validationplot(pls.fit, val.type="MSEP", main = "PLS - CV MSEP")
abline(v = best_pls_comp, col="red", lwd=2)

#Prunning

tree_model <- rpart(revenue ~ .,
                    data = train_data,
                    method = "anova",
                    control = rpart.control(cp = 0,
                                            minsplit = 5,
                                            xval = 10))

plotcp(tree_model)
title("Regression Tree - CV Error")

best_cp <- tree_model$cptable[
  which.min(tree_model$cptable[,"xerror"]),
  "CP"]

pruned_tree <- prune(tree_model, cp = best_cp)

plot(pruned_tree)
text(pruned_tree, use.n = TRUE)

tree_pred <- predict(pruned_tree, test_data)

rmse_tree <- sqrt(mean((y_test - tree_pred)^2))
r2_tree <- 1 - sum((y_test - tree_pred)^2) /
  sum((y_test - mean(y_test))^2)

best_cp
length(unique(pruned_tree$where))

#Random Forest

rf_model <- randomForest(revenue ~ .,
                         data = train_data,
                         ntree = 500,
                         mtry = floor(sqrt(ncol(train_data) - 1)),
                         importance = TRUE)

varImpPlot(rf_model,
           main = "Random Forest - Variable Importance")

plot(sqrt(rf_model$mse),
     type = "l",
     main = "Random Forest - OOB RMSE",
     xlab = "Number of Trees",
     ylab = "OOB RMSE")

rf_pred <- predict(rf_model, test_data)

rmse_rf <- sqrt(mean((y_test - rf_pred)^2))
r2_rf <- 1 - sum((y_test - rf_pred)^2) /
  sum((y_test - mean(y_test))^2)

#Model comparison

final_results <- data.frame(
  Model = c("Linear",
            "Forward Subset",
            "Ridge",
            "Lasso",
            "PCR",
            "PLS",
            "Pruned Tree",
            "Random Forest"),
  RMSE = c(rmse_lm,
           rmse_subset,
           rmse_ridge,
           rmse_lasso,
           rmse_pcr,
           rmse_pls,
           rmse_tree,
           rmse_rf),
  R2 = c(r2_lm,
         r2_subset,
         r2_ridge,
         r2_lasso,
         r2_pcr,
         r2_pls,
         r2_tree,
         r2_rf)
)

final_results <- final_results[order(final_results$RMSE), ]
print(final_results)
