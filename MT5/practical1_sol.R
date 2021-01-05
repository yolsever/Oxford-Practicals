##
## 1
##

loblolly <- read.table(
    "prepd-loblolly.txt.gz", 
    header = TRUE, 
    colClasses = "numeric"
)
dim(loblolly)

##
## 2
##

library(penalized)
ridge <- penalized(
    response = T, 
    penalized = ~ . - T, 
    data = loblolly, 
    lambda2 = 60,
    trace = FALSE
)
ridge

##
## 3
##

cbPalette <- c(
    "#999999", "#E69F00", "#56B4E9", "#009E73", 
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

plot_observed_vs_fitted <- function(observed, fitted) {
    x <- fitted
    y <- observed
    plot(
        x = x,
        y = y,
        xlab = "fitted values", 
        ylab = "observed",
        main = "Comparing fitted versus observed values",
        pch = 16,
        col = cbPalette[2]
    )
    ## 
    abline(a = 0, b = 1, col = cbPalette[3], lwd = 2)
    text(
        x = min(x) + diff(range(x)) * 0.1, 
        y = max(y) - diff(range(y)) * 0.1, 
        paste0("r2 = ", round(cor(x, y) ** 2, 3))
    )
}
plot_observed_vs_fitted(
    observed = loblolly[, "T"],
    fitted = fitted(ridge)
)
    
##
## 4
##

plot_residuals <- function(observed, fitted) {
    x <- fitted
    y <- observed - fitted
    plot(
        x = x,
        y = y,
        xlab = "fitted values", 
        ylab = "residuals (observed - fitted)",
        main = "Residual plot",
        pch = 16,
        col = cbPalette[2]
    )
    abline(h = 0, col = cbPalette[3], lwd = 2)
}
plot_comparison_and_residuals <- function(observed, fitted) {
    par(mfrow = c(1, 2))
    plot_observed_vs_fitted(observed = observed, fitted = fitted)
    plot_residuals(observed = observed, fitted = fitted)
}

plot_comparison_and_residuals(
    observed = loblolly[, "T"],
    fitted = fitted(ridge)
)

##
## 5
##

perform_ridge_regression_one_fold <- function(
    data, 
    fold, 
    lambda2
) {
    dtrain <- data[-fold, ]
    dtest <- data[fold, ]
    ridge_fold <- penalized(
        T, 
        penalized = ~ . - T, 
        data = dtrain,
        lambda2 = lambda2, 
        trace = FALSE
    )
    pred <- predict(ridge_fold, data = dtest)[, "mu"]
    return(data.frame(PRED = pred, OBS = dtest$T))
}

perform_ridge_regression <- function(
    data, 
    k, 
    lambda2
) {
    n <- nrow(data)
    folds <- suppressWarnings(split(sample(n), seq_len(k)))
    pairs <- lapply(
        folds, 
        perform_ridge_regression_one_fold, 
        data = data, 
        lambda2 = lambda2
    )
    results <- do.call("rbind", pairs)
    return(
        list(
            results = results,
            r2 = cor(results[, "PRED"], results[, "OBS"]) ** 2
        )
    )
}

##
## 6
##

out <- perform_ridge_regression(loblolly, k = 10, lambda2 = 60)
r2 <- out[["r2"]]
r2

plot_comparison_and_residuals(
    observed = out[["results"]][, "OBS"],
    fitted = out[["results"]][, "PRED"]
)

##
## 7
##

library("parallel")

perform_ridge_regression <- function(
    data, 
    k, 
    lambda2,
    nCores = 1
) {
    n <- nrow(data)
    folds <- suppressWarnings(split(sample(n), seq_len(k)))
    cl <- makeCluster(nCores)
    clusterExport(cl = cl, "perform_ridge_regression_one_fold")
    clusterEvalQ(cl = cl, library("penalized"))
    pairs <- parLapply(
        cl = cl,
        folds, 
        perform_ridge_regression_one_fold, 
        data = data, 
        lambda2 = lambda2
    )
    stopCluster(cl)    
    results <- do.call("rbind", pairs)
    return(
        list(
            results = results,
            r2 = cor(results[, "PRED"], results[, "OBS"]) ** 2
        )
    )
}

##
## 8
##

time_how_long_for_nCores <- function(nCores) {
    system.time(
        perform_ridge_regression(
            data = loblolly,
            k = 10,
            lambda2 = 60,
            nCores = nCores
        )
    )["elapsed"]
}
one_time <- time_how_long_for_nCores(1)
two_time <- time_how_long_for_nCores(8)

one_time
two_time

t_overhead <- two_time - one_time / 2
t_overhead

## on my machine, not much overhead, pretty clean

##
## 9
##

library(randomForest)

perform_model_fit_one_fold <- function(
    data, 
    fold, 
    lambda2,
    ntree,
    model
) {
    dtrain <- data[-fold, ]
    dtest <- data[fold, ]
    if (model == "ridge") {
        ridge_fold <- penalized(
            T, 
            penalized = ~ . - T, 
            data = dtrain,
            lambda2 = lambda2, 
            trace = FALSE
        )
        pred <- predict(ridge_fold, data = dtest)[, "mu"]
    } else if (model == "randomForest") {
        random_fold <- randomForest(
            T ~ . -T, 
            data = dtrain,
            ntree = ntree
        )
        pred <- predict(random_fold, dtest)
    } else {
        stop("Bad choice of model!")
    }
    return(data.frame(PRED = pred, OBS = dtest$T))
}

perform_model_fit <- function(
    data,
    model,
    k = 10, 
    lambda2 = 60,
    ntree = 20,
    nCores = 1
) {
    n <- nrow(data)
    folds <- suppressWarnings(split(sample(n), seq_len(k)))
    cl <- makeCluster(nCores)
    clusterExport(cl = cl, "perform_ridge_regression_one_fold")
    clusterEvalQ(cl = cl, library("penalized"))
    clusterEvalQ(cl = cl, library("randomForest"))    
    pairs <- parLapply(
        cl = cl,
        X = folds, 
        fun = perform_model_fit_one_fold,
        data = data, 
        lambda2 = lambda2,
        model = model,
        ntree = ntree
    )
    stopCluster(cl)    
    results <- do.call("rbind", pairs)
    return(
        list(
            results = results,
            r2 = cor(results[, "PRED"], results[, "OBS"]) ** 2
        )
    )
}

##
## 10
##

out_ridge <- perform_model_fit(
    data = loblolly,
    model = "ridge", 
    k = 10,
    lambda2 = 60,
    nCores = 4
)

out_ridge[["r2"]]
## not plotting, this was done earlier

out_RF <- perform_model_fit(
    loblolly, 
    model = "randomForest", 
    ntree = 20, 
    nCores = 4
)
plot_comparison_and_residuals(
    observed = out_RF[["results"]][, "OBS"],
    fitted = out_RF[["results"]][, "PRED"]
)
out_RF[["r2"]]

