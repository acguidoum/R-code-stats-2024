###############################
## Real Data Applications 1  ##
## Mean Annual Precipitation ##
###############################

## data available on the National Weather Service website 
## at https://www.weather.gov/wrh/Climate?wfo=sew
## mean annual precipitation
## X1 <- "HOQUIAM BOWERMAN AP" and X2 <- "ABERDEEN"
#######################
###      Data       ###
#######################

X1 <- c(0.19, 0.22, 0.20, 0.18, 0.21, 0.22, 0.15, 0.20, 0.15, 0.19, 0.17, 0.21,
        0.21, 0.22, 0.19, 0.11, 0.18, 0.22, 0.15, 0.16, 0.24, 0.18, 0.16, 0.15,
        0.19, 0.22, 0.24, 0.22, 0.21, 0.23, 0.15, 0.18, 0.15, 0.20, 0.15, 0.17,
        0.21, 0.15, 0.16, 0.18, 0.20, 0.19, 0.23, 0.15, 0.22, 0.18, 0.24, 0.21,
        0.18, 0.12, 0.20, 0.20, 0.19, 0.13, 0.20)

X2 <- c(0.27, 0.21, 0.16, 0.27, 0.22, 0.25, 0.24, 0.20, 0.24, 0.24, 0.22, 0.25,
        0.21, 0.22, 0.26, 0.21, 0.23, 0.24, 0.26, 0.19, 0.23, 0.26, 0.24, 0.21,
        0.25, 0.27, 0.17, 0.22, 0.16, 0.21, 0.20, 0.25, 0.27, 0.28, 0.25, 0.16,
        0.23, 0.20, 0.20, 0.20, 0.29, 0.22, 0.18, 0.17, 0.21, 0.27, 0.26, 0.29,
        0.27, 0.31, 0.16, 0.23, 0.20, 0.25, 0.19, 0.21, 0.28, 0.22, 0.18, 0.23,
        0.25, 0.23, 0.29, 0.18, 0.25, 0.23, 0.29, 0.27, 0.22, 0.14, 0.25, 0.26,
        0.30)

ts_X1 <- ts(X1, start = 1970, frequency = 1)
ts_X2 <- ts(X2, start = 1950, frequency = 1)

#######################
###    Table 4      ###
#######################

stats_X1 <- round(cal_statics(ts_X1),4)
stats_X2 <- round(cal_statics(ts_X2),4)
total_n <- stats_X1[1] + stats_X2[1]
nj_ratio_X1 <- round(stats_X1[1] / total_n, 4)
nj_ratio_X2 <- round(stats_X2[1] / total_n, 4)
table <- data.frame(
  Station = c("$X_{1t}$", "$X_{2t}$"),
  `n_j` = c(stats_X1[1], stats_X2[1]),
  `n_j/n1+n2` = c(nj_ratio_X1, nj_ratio_X2),
  Mean = c(stats_X1[2], stats_X2[2]),
  Variance = c(stats_X1[3], stats_X2[3]),
  Sigma2 = c(stats_X1[4], stats_X2[4]),
  Gamma = c(stats_X1[5], stats_X2[5]),
  `nu_j` = c(stats_X1[6], stats_X2[6]),
  `rho_j` = c(stats_X1[7], stats_X2[7])
)
table

## Results of the test

alpha = 0.05; deltat = 0
Delta = (nj_ratio_X1 * nj_ratio_X2 * deltat^2) / ((nj_ratio_X2 * stats_X1[6]) + (nj_ratio_X1 * stats_X2[6]))
rho_1 <- stats_X1[7]
rho_P <- round(((stats_X1[1] / stats_X1[6]) * stats_X1[7] + (stats_X2[1] / stats_X2[6])* stats_X2[7]) / ((stats_X1[1] / stats_X1[6]) + (stats_X2[1] / stats_X2[6])),4)
Z2 <- (stats_X1[7]-stats_X2[7])^2 / ((stats_X1[6]/stats_X1[1])+(stats_X2[6]/stats_X2[1]))
pval <- pchisq(Z2, df = 1,lower.tail=FALSE)
chisq <- qchisq(1 - alpha, df = 1,ncp = Delta)
rho_PT <- rho_P * (Z2 < qchisq(1 - alpha, df = 1,ncp = Delta)) + 
  rho_1 * (Z2 >= qchisq(1 - alpha, df = 1, ncp = Delta))
condition_check <- Z2 < qchisq(1 - alpha, df = 1,ncp = Delta)
results <- data.frame(
  rho_1 = rho_1,
  rho_P = rho_P,
  Z2 = Z2,
  Delta = Delta,
  chisq = chisq,
  Condition_Check = condition_check,
  rho_PT = rho_PT,
  p_value=pval
)
results
###################
# Graphic Figure 3
###################

df_X1 <- data.frame(Year = 1970:2024, Precipitation = X1, Series = "X1")
df_X2 <- data.frame(Year = 1950:2022, Precipitation = X2, Series = "X2")
df_combined <- rbind(df_X1, df_X2)
p1 <- ggplot(df_combined, aes(x = Year, y = Precipitation, color = Series)) +
  geom_line(linewidth = 0.74) +
  scale_color_manual(
    values = c("#9999FF", "#FF9999"),
    labels = c(expression(X[1*t]), expression(X[2*t]))
  ) +
  labs(title = NULL,
       x = "Year",
       y = "Mean Precipitation",
       color = "Series") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.spacing = unit(0.08, "lines"),
        legend.position = "top",
        legend.title = element_text(), 
        legend.text = element_text(size = 12, face = "bold"),
        legend.box.margin = margin(t = -14), 
        legend.box.spacing = unit(0.01, "cm"), 
        legend.spacing.y = unit(0.01, "cm"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold",vjust =-0.25),
        strip.text = element_text(size = 15, face = "bold.italic", margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75),
        plot.margin = margin(4, 1, 0, 0)) +
  scale_x_continuous(limits = c(1950, 2024), breaks = seq(1950,2024,by=6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

p2 <- ggplot(df_combined, aes(x = Series, y = Precipitation)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +  
  geom_boxplot(varwidth = TRUE,fill = c("#9999FF","#FF9999"),position = "jitter",outlier.shape = NA) +
  geom_jitter(width = 0.1,color = "black", alpha = 0.4) +
  labs(title = NULL,
       x = "",
       y = "Value") +
  scale_x_discrete(labels = c(expression(X[1*t]), expression(X[2*t]))) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.spacing = unit(0.08, "lines"),
        legend.position = "top",
        legend.title = element_text(), 
        legend.text = element_text(size = 12, face = "bold"),
        legend.box.margin = margin(t = -14), 
        legend.box.spacing = unit(0.01, "cm"), 
        legend.spacing.y = unit(0.01, "cm"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold",vjust =-0.25),
        axis.text.x = element_text(size = 12,face = "bold"),
        strip.text = element_text(size = 15, face = "bold.italic", margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75),
        plot.margin = margin(4, 1, 0, 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

# ACF plot
acf_X1 <- acf(X1, plot = FALSE,lag.max = 10, type = "correlation", demean = TRUE)  
acf_X2 <- acf(X2, plot = FALSE,lag.max = 10, type = "correlation", demean = TRUE) 
df_acf_X1 <- data.frame(Lag = acf_X1$lag, ACF = acf_X1$acf, Series = "X1")
df_acf_X2 <- data.frame(Lag = acf_X2$lag, ACF = acf_X2$acf, Series = "X2")
df_acf_combined <- rbind(df_acf_X1, df_acf_X2)
p_acf <- ggplot(df_acf_combined, aes(x = Lag, y = ACF, fill = Series)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = NULL,x = "Lag",y = "Auto-Correlation Function") +
  scale_fill_manual(values = c("X1" = "#9999FF", "X2" = "#FF9999"),
                    labels = c(expression(X[1*t]), expression(X[2*t])) )+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.spacing = unit(0.08, "lines"),
        legend.position = "top",
        legend.title = element_text(), 
        legend.text = element_text(size = 12, face = "bold"),
        legend.box.margin = margin(t = -14), 
        legend.box.spacing = unit(0.01, "cm"), 
        legend.spacing.y = unit(0.01, "cm"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold",vjust =-0.25),
        strip.text = element_text(size = 15, face = "bold.italic", margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75),
        plot.margin = margin(4, 1, 0, 0))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))

grid.arrange(p1,p_acf, p2, layout_matrix = rbind(c(1, 2), c(3, 3)), 
             padding = unit(0.01, "line"))
