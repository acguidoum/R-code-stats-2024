###############################
## Real Data Applications 2  ##
## Consumer confidence index ##
###############################

indicators <- WDIsearch("interest rate")
head(indicators)

#######################
###      Data       ###
#######################

inflation <- WDI(indicator = "FP.CPI.TOTL.ZG", country = c("PL","DZ"), 
                 start = 1960, end = 2023)
inflation_data_filtered <- inflation %>%
  select(year, iso2c, FP.CPI.TOTL.ZG) %>%
  spread(iso2c, FP.CPI.TOTL.ZG)

PL <- ts(na.omit(inflation_data_filtered$PL), start = 1971, frequency = 1)
DZ <- ts(na.omit(inflation_data_filtered$DZ), start = 1970, frequency = 1)

#######################
###    Table 5      ###
#######################

stats_X1 <- round(cal_statics(PL),4)
stats_X2 <- round(cal_statics(DZ),4)
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

### results 

alpha = 0.05; deltat = 0
Delta = (nj_ratio_X1 * nj_ratio_X2 * deltat^2) / ((nj_ratio_X2 * stats_X1[6]) + (nj_ratio_X1 * stats_X2[6]))
rho_1 <- stats_X1[7]
rho_P <- round(((stats_X1[1] / stats_X1[6]) * stats_X1[7] + (stats_X2[1] / stats_X2[6])* stats_X2[7]) / ((stats_X1[1] / stats_X1[6]) + (stats_X2[1] / stats_X2[6])),4)
Z2 <- (stats_X1[7]-stats_X2[7])^2 / ((stats_X1[6]/stats_X1[1])+(stats_X2[6]/stats_X2[1]))
pval <- pchisq(Z2, df = 1,ncp = Delta,lower.tail=FALSE)
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

#######################
###    Table 6      ###
#######################

deltat_values <- seq(0, 2, by = 0.2)
final_results <- data.frame()
for (deltat in deltat_values) {
  Delta <- (nj_ratio_X1 * nj_ratio_X2 * deltat^2) / ((nj_ratio_X2 * stats_X1[6]) + (nj_ratio_X1 * stats_X2[6]))
  rho_1 <- stats_X1[7]
  rho_P <- round(((stats_X1[1] / stats_X1[6]) * stats_X1[7] + (stats_X2[1] / stats_X2[6]) * stats_X2[7]) / 
                   ((stats_X1[1] / stats_X1[6]) + (stats_X2[1] / stats_X2[6])), 4)
  Z2 <- (stats_X1[7] - stats_X2[7])^2 / ((stats_X1[6] / stats_X1[1]) + (stats_X2[6] / stats_X2[1]))
  pval <- pchisq(Z2, df = 1,ncp = Delta, lower.tail = FALSE)
  chisq <- qchisq(1 - alpha, df = 1, ncp = Delta)
  condition_check <- Z2 < chisq
  rho_PT <- ifelse(condition_check, rho_P, rho_1)
  final_results <- rbind(final_results, data.frame(
    deltat = deltat,
    Delta = Delta,
    chisq = chisq,
    p_value = pval,
    Condition_Check = condition_check,
    rho_PT = rho_PT
  ))
}
final_results

###################
# Graphic Figure 4
###################

df_X1 <- data.frame(Year = 1971:2023, AIR = as.numeric(PL), Series = "PL")
df_X2 <- data.frame(Year = 1970:2023, AIR = as.numeric(DZ), Series = "DZ")
df_combined <- rbind(df_X1, df_X2)

# ts plot
p1 <- ggplot(df_combined, aes(x = Year, y = AIR, color = Series)) +
  geom_line(linewidth = 0.74) +
  scale_color_manual(values = c("#9999FF", "#FF9999"),labels = c("DZ","PL")) +
  labs(title = NULL,
       x = "Year",
       y = "Annual Inflation Rates",
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
        axis.title.y = element_text(face = "bold",vjust =-0.5),
        strip.text = element_text(size = 15, face = "bold.italic", margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75),
        plot.margin = margin(4, 1, 0, 0)) +
  scale_x_continuous(limits = c(1970, 2024), breaks = seq(1970,2024,by=6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

# boxplot
p2 <- ggplot(df_combined, aes(x = Series, y = log(AIR))) +
  stat_boxplot(geom = "errorbar", width = 0.2) +  
  geom_boxplot(varwidth = TRUE,fill = c("#9999FF","#FF9999"),outlier.shape = NA) +
  geom_jitter(width = 0.1,color = "black", alpha = 0.4) +
  labs(title = NULL,x = "",y = "Log(Value)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.spacing = unit(0.08, "lines"),
        legend.position = "top",
        legend.title = element_text(size = 12, face = "bold", hjust = 1.5, margin = margin(b = 0)), 
        legend.text = element_text(size = 12, face = "bold"),
        legend.box.margin = margin(t = -14), 
        legend.box.spacing = unit(0.01, "cm"), 
        legend.spacing.y = unit(0.01, "cm"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold",vjust =-0.5),
        axis.text.x = element_text(size = 12,face = "bold"),
        strip.text = element_text(size = 15, face = "bold.italic", margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75),
        plot.margin = margin(4, 1, 0, 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),labels = scales::number_format(accuracy = 0.1))

## boxplot separated 
df_X1 <- df_combined %>% filter(Series == "PL")
df_X2 <- df_combined %>% filter(Series == "DZ")
p_2 <- ggplot(df_X1, aes(x = Series, y = log(AIR))) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(varwidth = TRUE,fill = "#FF9999") +
  geom_jitter(width = 0.1, color = "black", alpha = 0.4) +
  labs(title = NULL,x = "",y = "Log(Value)") +
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
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(size = 12,face = "bold"),
        strip.text = element_text(size = 15, face = "bold.italic", margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),labels = scales::number_format(accuracy = 0.1))

p_3 <- ggplot(df_X2, aes(x = Series, y = log(AIR))) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(varwidth = TRUE,fill = "#9999FF") +
  geom_jitter(width = 0.1, color = "black", alpha = 0.4) +
  labs(title = NULL, x = "", y = "Log(Value)") +
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
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(size = 12,face = "bold"),
        strip.text = element_text(size = 15, face = "bold.italic", margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),labels = scales::number_format(accuracy = 0.1))

# ACF plot
acf_X1 <- acf(PL, plot = FALSE,lag.max = 10, type = "correlation", demean = TRUE)  
acf_X2 <- acf(DZ, plot = FALSE,lag.max = 10, type = "correlation", demean = TRUE) 
df_acf_X1 <- data.frame(Lag = acf_X1$lag, ACF = acf_X1$acf, Series = "PL")
df_acf_X2 <- data.frame(Lag = acf_X2$lag, ACF = acf_X2$acf, Series = "DZ")
df_acf_combined <- rbind(df_acf_X1, df_acf_X2)
p_acf <- ggplot(df_acf_combined, aes(x = Lag, y = ACF, fill = Series)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = NULL,x = "Lag",y = "Auto-Correlation Function") +
  theme_minimal() +
  scale_fill_manual(values = c("PL" = "#FF9999", "DZ" = "#9999FF"))+
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
        axis.title.y = element_text(face = "bold",vjust =-0.5),
        strip.text = element_text(size = 15, face = "bold.italic", margin = margin(1, 1, 1, 1)),
        strip.background = element_rect(fill = "#f0f0f0", color = "black", linewidth = 0.75),
        plot.margin = margin(4, 1, 0, 0))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))

grid.arrange(p1,p_acf, p2, layout_matrix = rbind(c(1, 2), c(3, 3)), 
             padding = unit(0.01, "line"))
