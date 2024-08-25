########################
## Plots with ggplot2
########################

#######################
### Plot Fig01(top) ###
#######################

n1 <- 25
n2 <- 50
deltat_seq <- seq(0,6,by=0.1)
alpha_seq <- c(0.05, 0.1)
rho1_seq <- c(0.3, 0.5, 0.7)
param_list <- expand.grid(n1 = n1, n2 = n2, rho1 = rho1_seq, alpha = alpha_seq, 
                          deltat = deltat_seq)
# Set up parallel execution
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
results <- foreach(i = 1:nrow(param_list), .combine = rbind, .packages = c("stats", "foreach", "doParallel")) %dopar% {
  params <- param_list[i, ]
  set.seed(321)
  res <- PTE_VP(n1 = params$n1, n2 = n2, rho_1 = params$rho1, deltat = params$deltat, 
                alpha = params$alpha, M = 10000)
  res
}
stopCluster(cl)
Tab1 <- round(results,10)
Tab1 <- Tab1[order(Tab1$n1, Tab1$alpha,Tab1$rho1), ]
Tab1$RE0 <- rep(1,dim(Tab1)[1])
Tab1 <- Tab1[order(Tab1$n1, Tab1$n2, Tab1$alpha), ]
Tab1
subset_data <- Tab1[Tab1$n1 == 25 & Tab1$n2 == 50, ]
unique_n1 <- unique(subset_data$n1)
unique_n2 <- unique(subset_data$n2)
subset_data_long <- melt(subset_data, id.vars = c("deltat", "alpha", "rho1"), measure.vars = c("RE0","RE1", "RE2"))

plot1 <- ggplot(subset_data_long, aes(x = deltat, y = value, color = factor(rho1), linetype = variable)) +
  geom_line(data = subset(subset_data_long, variable != "RE0"),linewidth = 0.6) +
  geom_line(data = subset(subset_data_long, variable == "RE0"), color = "black",linewidth = 0.6) +
  labs(title = NULL, x = expression(delta), y = "Simulated Relative Efficiency", color = expression(rho[1]), linetype = NULL) + 
  scale_linetype_manual(values = c("RE0" = "dotted","RE1" = "dashed", "RE2" = "solid"), 
                        breaks = c("RE0", "RE1", "RE2"),
                        labels = c(expression(bold(UE)),expression(bold(P)), expression(bold(PT)))) + 
  scale_x_continuous(breaks = pretty_breaks(n = 12),labels = scales::number_format(accuracy = 0.1)) + 
  scale_y_continuous(breaks = pretty_breaks(n = 10),labels = scales::number_format(accuracy = 0.1)) + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.spacing = unit(0.08, "lines"),
        legend.position = "top",
        legend.title = element_text(size = 12, face = "bold", hjust = 1.5, margin = margin(b = 0)), 
        legend.text = element_text(size = 12, face = "bold"),
        legend.box.margin = margin(t = -11), 
        legend.box.spacing = unit(0.01, "cm"), 
        legend.spacing.y = unit(0.01, "cm"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(size = 15, face = "bold.italic",margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75)) + 
  facet_wrap(~ alpha, labeller = label_bquote("n"[1] == .(unique_n1) ~~ ", n"[2] == .(unique_n2) ~~ "," ~ alpha == .(alpha)))
plot1

##########################
### Plot Fig01(bottom) ###
##########################

n1 <- 500
n2 <- 500
deltat_seq <- seq(0,20,by=0.5)
alpha_seq <- c(0.05, 0.1)
rho1_seq <- c(0.3, 0.5, 0.7)
param_list <- expand.grid(n1 = n1, n2 = n2, rho1 = rho1_seq, alpha = alpha_seq, 
                          deltat = deltat_seq)
# Set up parallel execution
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
results <- foreach(i = 1:nrow(param_list), .combine = rbind, .packages = c("stats", "foreach", "doParallel")) %dopar% {
  params <- param_list[i, ]
  set.seed(921)
  res <- PTE_VP(n1 = params$n1, n2 = n2, rho_1 = params$rho1, deltat = params$deltat, 
                alpha = params$alpha, M = 10000)
  res
}
stopCluster(cl)
Tab2 <- round(results,10)
Tab2 <- Tab2[order(Tab2$n1, Tab2$alpha,Tab2$rho1), ]
Tab2$RE0 <- rep(1,dim(Tab2)[1])
Tab2 <- Tab2[order(Tab2$n1, Tab2$n2, Tab2$alpha), ]
Tab2
subset_data <- Tab2[Tab2$n1 == 500 & Tab2$n2 == 500, ]
unique_n1 <- unique(subset_data$n1)
unique_n2 <- unique(subset_data$n2)
subset_data_long <- melt(subset_data, id.vars = c("deltat", "alpha", "rho1"), measure.vars = c("RE0","RE1", "RE2"))

# Plot for both alpha values with alpha as an expression
plot2 <- ggplot(subset_data_long, aes(x = deltat, y = value, color = factor(rho1), linetype = variable)) +
  geom_line(data = subset(subset_data_long, variable != "RE0"),linewidth = 0.6) +
  geom_line(data = subset(subset_data_long, variable == "RE0"), color = "black",linewidth = 0.6) +
  labs(title = NULL, x = expression(delta), y = "Simulated Relative Efficiency", color = expression(rho[0]), linetype = NULL) + 
  scale_linetype_manual(values = c("RE0" = "dotted","RE1" = "dashed", "RE2" = "solid"), 
                        breaks = c("RE0", "RE1", "RE2"),
                        labels = c(expression(bold(UE)),expression(bold(P)), expression(bold(PT)))) + 
  scale_x_continuous(limits = c(0, 20),breaks = pretty_breaks(n = 10),labels = scales::number_format(accuracy = 0.1)) + 
  scale_y_continuous(breaks = pretty_breaks(n = 10),labels = scales::number_format(accuracy = 0.1)) + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.spacing = unit(0.08, "lines"),
        legend.position = "top",
        legend.title = element_text(size = 12, face = "bold", hjust = 1.5, margin = margin(b = 0)), 
        legend.text = element_text(size = 12, face = "bold"),
        legend.box.margin = margin(t = -11), 
        legend.box.spacing = unit(0.01, "cm"), 
        legend.spacing.y = unit(0.01, "cm"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(size = 15, face = "bold.italic",margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75)) + 
  facet_wrap(~ alpha, labeller = label_bquote("n"[1] == .(unique_n1) ~ ", n"[2] == .(unique_n2) ~ "," ~ alpha == .(alpha)))
plot2
