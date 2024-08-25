#########################
### test statistic Z2 ###
#########################

#######################
###    Table 3      ###
#######################

# Parameters for different values of rho_1 and deltat 
# change only parameters deltat 
deltat <- 2
n1 <- 500; n2 <- 500
rho_1 <- c(0.3, 0.5, 0.7)
M <- 10000
pi <- n1 / (n1 + n2)
one_minus_pi <- n2 / (n1 + n2)
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
results_list <- list()
results_list <- foreach(rho_1 = rho_1, .combine = rbind, .packages = c("foreach", "doParallel", "stats")) %dopar% {
  Z2_samples <- numeric(M)
  nu1_samples <- numeric(M)
  nu2_samples <- numeric(M)
  set.seed(921)
  for (i in 1:M) {
    res <- PTE_VP(n1 = n1, n2 = n2, rho_1 = rho_1, deltat = deltat, M = 1)
    Z2_samples[i] <- res$Z2
    nu1_samples[i] <- res$nu1
    nu2_samples[i] <- res$nu2
  }
  nu1_mean <- mean(nu1_samples, na.rm = TRUE)
  nu2_mean <- mean(nu2_samples, na.rm = TRUE)
  ncp_values <- (pi * one_minus_pi * deltat^2) / ((one_minus_pi * nu1_mean) + (pi * nu2_mean))
  data.frame(rho_1 = rho_1, Z2 = Z2_samples, nu1 = nu1_mean, 
             nu2 = nu2_mean, NCP = ncp_values)
}
stopCluster(cl)

summary_stats <- results_list %>%
  group_by(rho_1)%>%
  summarise(
    Mean =round(mean(Z2, na.rm =TRUE),4),
    Variance =round(var(Z2, na.rm =TRUE),4),
    Median =round(median(Z2, na.rm =TRUE),4),
    Skewness =round(skewness(Z2, na.rm =TRUE),4),
    Kurtosis =round(kurtosis(Z2, na.rm =TRUE),4),
    .groups ='drop')
fit_results <- results_list %>%
  group_by(rho_1) %>%
  do({
    data <- .$Z2
    params <- fit_chisq(data)
    data.frame(rho_1 = unique(.$rho_1), df_est = round(params["df"],4), 
               ncp_est = round(params["ncp"],4))
  })
Tab3 <- left_join(summary_stats, fit_results, by ="rho_1")
Tab3

#######################
###  Plot Fig02     ###
#######################

results_list <- left_join(results_list, fit_results, by = "rho_1")
results_list

p <- ggplot() +
  geom_histogram(data = subset(results_list, rho_1 == 0.3),
                 aes(x = Z2, y = after_stat(density), fill = factor(rho_1)),
                 breaks = pretty(range(results_list$Z2[results_list$rho_1 == 0.3]),
                                 n = nclass.FD(results_list$Z2[results_list$rho_1 == 0.3])),
                 color = "black", alpha = 0.4, boundary = 0, na.rm = TRUE) +
  geom_histogram(data = subset(results_list, rho_1 == 0.5),
                 aes(x = Z2, y = after_stat(density), fill = factor(rho_1)),
                 breaks = pretty(range(results_list$Z2[results_list$rho_1 == 0.5]),
                                 n = nclass.FD(results_list$Z2[results_list$rho_1 == 0.5])),
                 color = "black", alpha = 0.4, boundary = 0, na.rm = TRUE) +
  geom_histogram(data = subset(results_list, rho_1 == 0.7),
                 aes(x = Z2, y = after_stat(density), fill = factor(rho_1)),
                 breaks = pretty(range(results_list$Z2[results_list$rho_1 == 0.7]),
                                 n = nclass.FD(results_list$Z2[results_list$rho_1 == 0.7])),
                 color = "black", alpha = 0.4, boundary = 0, na.rm = TRUE) +
  stat_function(data = subset(results_list, rho_1 == 0.3),
                fun = function(x) stats::dchisq(x,
                                                df = unique(results_list$df_est[results_list$rho_1 == 0.3]),
                                                ncp = unique(results_list$ncp_est[results_list$rho_1 == 0.3])),
                color = "black", linewidth = 0.65, show.legend = FALSE,inherit.aes=FALSE) +
  stat_function(data = subset(results_list, rho_1 == 0.5),
                fun = function(x) stats::dchisq(x,
                                                df = unique(results_list$df_est[results_list$rho_1 == 0.5]),
                                                ncp = unique(results_list$ncp_est[results_list$rho_1 == 0.5])),
                color = "black", linewidth = 0.65, show.legend = FALSE,inherit.aes=FALSE) +
  stat_function(data = subset(results_list, rho_1 == 0.7),
                fun = function(x) stats::dchisq(x,
                                                df = unique(results_list$df_est[results_list$rho_1 == 0.7]),
                                                ncp = unique(results_list$ncp_est[results_list$rho_1 == 0.7])),
                color = "black", linewidth = 0.75, show.legend = FALSE,inherit.aes=FALSE) +
  labs(title = bquote(delta==.(deltat)), x = expression(bolditalic(Z[2])), y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.spacing = unit(0.08, "lines"),
        legend.position = "nono",
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.key.height = unit(0.5, "cm"), 
        legend.key.width  = unit(0.5, "cm"), 
        legend.box.margin = margin(t = -15), 
        legend.box.spacing = unit(0.01, "cm"), 
        legend.spacing = unit(0, "cm"),
        legend.spacing.y = unit(0.01, "cm"),
        legend.spacing.x = unit(1, "cm"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(size = 14, face = "bold.italic",margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75)) +
  scale_x_continuous(limits = c(0, 10), breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10),labels = scales::number_format(accuracy = 0.1)) + 
  facet_wrap(~ rho_1, 
             labeller = label_bquote(rho[1] == .(rho_1)~~~~~~over(phantom(0)*phantom(0),phantom(0))~~chi^2~(.(round(unique(results_list$df_est[results_list$rho_1 == rho_1]), 4))~","~.(round(unique(results_list$ncp_est[results_list$rho_1 == rho_1]),4)))))
p
