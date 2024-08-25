################################################################################
## simulation study
################################################################################

#############
# Simple example for used 
#############

set.seed(321)
rho_1 = 0.3; n1 = 500; n2 = 500;
deltat_max <- (1 - rho_1) * sqrt(n1 + n2)
deltat_max
output <- PTE_VP(n1, n2, rho_1 , deltat = 0.8, alpha = 0.1, M = 10000)
output

#############
#### Table (change only the parameters)
#############

# Define parameters:
n1 <- c(250,500)
n2 <- 500
deltat_seq <- c(0,0.2,0.4,0.6,0.8,1,3,5,7,9,11,13,15,17,19,21)
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
Tab1 <- round(results,4)
Tab1 <- Tab1[order(Tab1$n1, Tab1$alpha,Tab1$rho1), ]
Tab1[,-c(3,4,8)]

# Format table
results_table <- Tab1 %>%
  pivot_wider(names_from = c(rho1, alpha), values_from = c(RE1, RE2), 
              names_sep = "_alpha_") %>%
  arrange(n1, deltat)
print(results_table,n=22)
