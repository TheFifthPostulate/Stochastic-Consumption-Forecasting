library(stats)

set.seed(42)

analyze_variance_mean <- function(data, window=8) {
  
  y <- data$y
  rolling_stats <- data.frame()
  
  for (i in window:length(y)) {
    train_data <- y[(i - window + 1):i]
    rolling_stats <- rbind(rolling_stats, data.frame(
      time_point = i,
      rolling_mean=mean(train_data),
      rolling_var=var(train_data),
      rolling_ratio=var(train_data)/mean(train_data)
    ))
  }
  
  par(mfrow=c(1,2))
  
  plot(rolling_stats$rolling_mean, rolling_stats$rolling_var,
       main="Variance-Mean relationship",
       xlab="Rolling Mean", ylab="Rolling Variance")
  
  if (nrow(rolling_stats) > 3) {
    lm_fit <- lm(rolling_var ~ rolling_mean, data=rolling_stats)
    abline(lm_fit, col="red")
    quad_fit <- lm(rolling_var ~ rolling_mean + I(rolling_mean^2), data=rolling_stats)
    print(summary(quad_fit))
    pred_vals <- predict(quad_fit, newdata=data.frame(rolling_mean = sort(rolling_stats$rolling_mean)))
    lines(pred_vals ~ sort(rolling_stats$rolling_mean))
  }
  
  
  plot(rolling_stats$time_point, rolling_stats$rolling_ratio,
       main="Var/Mean Ratio over Time (Weeks)",
       xlab="Week", ylab="Ratio")
  
  title(data$material[[1]], side = 3, line=-1, outer = TRUE)
  
  par(mfrow=c(1,1))
  
  mean_rolling_ratio <- mean(rolling_stats$rolling_ratio, na.rm=TRUE)
  sd_rolling_ratio <- sd(rolling_stats$rolling_ratio, na.rm=TRUE)
  ratio_cv <- sd_rolling_ratio / mean_rolling_ratio
  
  cat("ratio_cv: ", ratio_cv, "\n")
  
  return(rolling_stats)
  
}

fit_nb_model <- function(train_data, level_weights=FALSE) {
  
  y <- train_data$y
  total <- train_data$stock
  n <- train_data$n
  
  b0 <- median(n); a0 <- median(y) / b0
  
  if (level_weights==FALSE) {
    a <- a0 + sum(y); b <- b0 + sum(n)
  } else {
    
    var_mean_ratio <- var(y) / mean(y)
    
    if (var_mean_ratio > 3) {
      effective_y_idx <- (y >= quantile(y, 0.10) & (y <= quantile(y, 0.90)))
      effective_y <- y[effective_y_idx]
      effective_n <- n[effective_y_idx]
    } else {
      
      effective_y <- y
      effective_n <- n
    }
    
    a <- a0 + sum(effective_y)
    b <- b0 + sum(effective_n)
    
  }
  
  return(list(
    a = a,
    b = b
  ))
  
}

predict_consumption_nb <- function(train_data, H_days, Nmc=5000, level_weights=FALSE) {
  
  predictions <- numeric(Nmc)
  
  train <- fit_nb_model(train_data=train_data, level_weights=level_weights)
  predictions <- rnbinom(Nmc, size=train$a, prob=train$b / (train$b + H_days))
  expected_demand <- median(predictions)
  prediction_error <- mad(predictions)
  
  return(list(expected_demand = expected_demand,
              prediction_error = prediction_error,
              predictions = predictions))
  
}

choose_restock_quantity <- function(pred_samples,
                                    waste_frac_max = 0.15,
                                    Q_min = NULL,
                                    Q_max = NULL,
                                    step = 1) {
  
  if (is.null(Q_min)) Q_min <- quantile(pred_samples, 0.1)
  if (is.null(Q_max)) Q_max <- quantile(pred_samples, 0.95) * 2.0
  
  Q_grid <- seq(Q_min, Q_max, by = step)
  
  waste_frac <- sapply(Q_grid, function(Q) {
    expected_waste <- mean(pmax(Q - pred_samples, 0))
    expected_waste / Q
  })
  
  ok_idx <- which(waste_frac <= waste_frac_max)
  
  if (length(ok_idx) == 0) {
    return(list(
      Q = median(pred_samples),
      waste_frac = NA,
      Q_grid = Q_grid,
      waste_frac_grid = waste_frac
    ))
  }
  
  best_Q <- Q_grid[max(ok_idx)]
  
  list(
    Q = best_Q,
    Q_grid = Q_grid,
    waste_frac_grid = waste_frac
  )
}

cross_validate_nb <- function(data, H_days, train_window, step=3, Nmc=1000, level_weights=FALSE, waste_frac_max=0.15) {
  
  test_window <- floor(H_days/ 7)
  
  forecast_date <- c(); actual_consumed <- c(); prediction <- c(); residual <- c();
  mean_data <- c(); var_data <- c(); var_mean_ratio <- c(); posteriors <- vector("list", 0)
  
  for (i in seq(from=1, to=(nrow(data) - train_window - test_window), by=step)) {
    train_data <- data[i:(i + train_window - 1), ]
    test_data <- data[(i + train_window):(i + train_window + test_window - 1), ]
    
    actual_consumed_i <- sum(test_data$y)
    
    predict <- predict_consumption_nb(train_data=train_data, H_days=H_days, Nmc=Nmc, level_weights=level_weights)
    decision <- choose_restock_quantity(pred_samples = predict$predictions, waste_frac_max=waste_frac_max)
    
    predict_i <- decision$Q
    residual_i <- actual_consumed_i - predict_i
    mean_data_i <- mean(train_data$y)
    var_data_i <- var(train_data$y)
    var_mean_ratio_i <- var_data_i / mean_data_i
    
    plot_df_i <- data.frame(predict=predict$predictions)
    
    forecast_date <- c(tail(train_data$date, 1), forecast_date)
    actual_consumed <- c(actual_consumed_i, actual_consumed)
    prediction <- c(predict_i, prediction)
    residual <- c(residual_i, residual)
    mean_data <- c(mean_data_i, mean_data)
    var_data <- c(var_data_i, var_data)
    var_mean_ratio <- c(var_mean_ratio_i, var_mean_ratio)
    posteriors[[length(posteriors) + 1]] <- predict$predictions
    
  }
  
  results_df <- data.frame(
    forecast_date = forecast_date,
    actual_consumed = actual_consumed,
    prediction = prediction,
    residual = residual,
    mean_data = mean_data,
    var_data = var_data,
    var_mean_ratio = var_mean_ratio
  )
  
  results_df <- results_df%>%arrange(forecast_date)
  
  return(list(results_df=results_df, posteriors=posteriors))
  
}
