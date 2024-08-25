################################################################################
## Main R function
################################################################################

PTE_VP <- function(n1 = 10, n2 = 50, rho_1 = 0.3, deltat = 0.06, alpha = 0.05, M = 1) {
  rho_2 <- rho_1 + deltat / sqrt(n1 + n2) 
  if (abs(rho_2) >= 1) {
    return(return(data.frame(n1 = n1, n2 = n2, nu1 = NA, nu2 = NA,
                             deltat = deltat, alpha = alpha, rho1 = rho_1, 
                             Z2 = NA, RE1 = NA, RE2 = NA)))
  }
  # Generate simulated data
  X1 <- sapply(1:M, function(i) arima.sim(model = list(ar = rho_1), n = n1, sd = 1))
  X2 <- sapply(1:M, function(i) arima.sim(model = list(ar = rho_2), n = n2, sd = 1))
  # Fit AR(1) models
  est1 <- lapply(1:M, function(i) ar.ols(X1[,i], aic = FALSE, order.max = 1, 
                                         demean = FALSE, intercept = FALSE))
  est2 <- lapply(1:M, function(i) ar.ols(X2[,i], aic = FALSE, order.max = 1, 
                                         demean = FALSE, intercept = FALSE))
  rho_hat_1 <- sapply(1:M, function(i) est1[[i]]$ar[[1]])
  rho_hat_2 <- sapply(1:M, function(i) est2[[i]]$ar[[1]])
  gamma_hat_1 <- sapply(1:M, function(i) acf(X1[,i], plot = FALSE, lag.max = 0, 
                                             type = "covariance", demean = TRUE)$acf[1])
  gamma_hat_2 <- sapply(1:M, function(i) acf(X2[,i], plot = FALSE, lag.max = 0, 
                                             type = "covariance", demean = TRUE)$acf[1])
  Sigma2_hat_1 <- sapply(1:M, function(i) est1[[i]]$var.pred)
  Sigma2_hat_2 <- sapply(1:M, function(i) est2[[i]]$var.pred)
  nu_hat_1 <- Sigma2_hat_1 / gamma_hat_1
  nu_hat_2 <- Sigma2_hat_2 / gamma_hat_2
  rho_hat_P_1 <- ((n1 / nu_hat_1) * rho_hat_1 + (n2 / nu_hat_2) * rho_hat_2) / ((n1 / nu_hat_1) + (n2 / nu_hat_2))
  Z2 <- (rho_hat_1-rho_hat_2)^2 / ((nu_hat_1/n1)+(nu_hat_2/n2))
  rho_hat_PT_1 <- rho_hat_P_1 * (Z2 < qchisq(1 - alpha, df = 1)) + rho_hat_1 * (Z2 >= qchisq(1 - alpha, df = 1))
  mse_rho_hat_1 <- mean((rho_hat_1 - rho_1)^2)
  mse_rho_hat_P_1 <- mean((rho_hat_P_1 - rho_1)^2)
  mse_rho_hat_PT_1 <- mean((rho_hat_PT_1 - rho_1)^2)
  RE_1 <- mse_rho_hat_1 / mse_rho_hat_P_1
  RE_2 <- mse_rho_hat_1 / mse_rho_hat_PT_1
  return(data.frame(n1 = n1, n2 = n2, nu1 = mean(nu_hat_1),nu2 = mean(nu_hat_2),
                    deltat = deltat, alpha = alpha, rho1 = rho_1,
                    Z2 = mean(Z2), RE1 = RE_1, RE2 = RE_2))
}

fit_chisq <- function(data) {
  nll_chisq_non_central <- function(df, ncp) {
    -sum(dchisq(data, df = df, ncp = ncp, log = TRUE))
  }
  fit <- stats4::mle(nll_chisq_non_central, start = list(df = trunc(var(data)/2), 
                                                         ncp = trunc(mean(data))),
                     lower = list(df = 0.01, ncp = 0), optim = optimx::optimr)
  params <- coef(fit)
  return(params)
}

cal_statics <- function(serie) {
  est <- ar.ols(serie, aic = FALSE, order.max = 1, demean = FALSE, intercept = FALSE)
  gamma <- acf(serie, plot = FALSE, lag.max = 0, type = "covariance", demean = TRUE)$acf[1]
  Sigma2 <-est$var.pred
  mean <-mean(serie)
  variance <- var(serie)
  n <- length(serie)
  nu <- Sigma2/gamma
  rho <- est$ar[[1]]
  return(c(n, mean, variance,Sigma2, gamma,nu, rho))
}
