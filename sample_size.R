library(tidyverse)
library(scales)
library(SCCS)
library(survival)

# Sample size of cohort design --------------------------------------------

sample_size_fisher <- function(data){
  
  p0 <- data$prevalence
  p1 <- p0 * data$risk_ratio
  n1 <- data$n1
  ratio <- data$ratio
  n0 <- n1*ratio
  res_p <- numeric(iteration)
  i <- 1
  for (i in 1:iteration){
    # set.seed(i)
    expected_n1 <- rbinom(n1, 1, prob = p1)
    expected_n0 <- rbinom(n0, 1, prob = p0)
    
    num_expected_n1 <- sum(expected_n1)
    num_expected_n0 <- sum(expected_n0)
    
    if (sum(num_expected_n1, num_expected_n0) != 0){
      X <- matrix(c(num_expected_n1, num_expected_n0, n1-num_expected_n1, n0-num_expected_n0), nrow = 2)
      
      res_fisher <- fisher.test(X)
      res_p[i] <- res_fisher$p.value
    } else {
      res_p[i] <- 1
    }
    
  }
  res_p
}

iteration <- 1000
prevalence <- c(1/100000, 5/100000, 10/100000, 50/100000, 100/100000)
risk_ratio <- c(1.2, 1.5, 2, 5)
n1 <- c(100000, 500000, 1000000, 2000000, 3000000, 4000000, 5000000)
ratio <- c(1, 3, 5)

# data <- tibble(prevalence, risk_ratio, n1, ratio)

set.seed(1234)
res_sim <- expand_grid(prevalence, risk_ratio, n1, ratio) %>%
  mutate(no = row_number()) %>% 
  # filter(no == 1) %>%
  group_nest(no) %>% 
  mutate(result = map(data, ~sample_size_fisher(data = .x), .progress = TRUE))

# Mac proで実行
res_sim2 <- res_sim %>% 
  mutate(power = map_dbl(result, ~ (100*sum(.x < 0.05)/iteration))) %>% 
  unnest(data)

write_rds(res_sim2, "03_Output/result_cohort_yymmdd.rds")


# Sample size of SCCS design ----------------------------------------------

sample_size_sccs<- function(data){
  
  rate_ratio <- data$rate_ratio
  # n <- 100
  n <- data$n
  risk_period <- data$risk_period
  obserbed_age <- data$obserbed_age
  
  res_sccs_p <- numeric(iteration)
  
  for (i in 1:iteration){
    exposure_date <- floor(runif(n, 365*65+1, 365*obserbed_age))
    simdata <- simulatesccsdata(nindivs = n,
                                astart = 365*65+1, 
                                aend = 365*obserbed_age,
                                adrug = exposure_date,
                                aedrug = exposure_date + risk_period - 1,
                                eexpo = rate_ratio
    )
    simdata2 <- formatdata(indiv = indiv,
                           astart = astart,
                           aend = aend,
                           aevent = aevent,
                           adrug = adrug1,
                           aedrug = aedrug1,
                           data = simdata
                           )
    
    if (sum(simdata2$event[simdata2$adrug1 == 1]) > 0){
      num <- dim(simdata2)[1]
      sim.mod <- coxph(formula = Surv(rep(1, num), event) ~ adrug1 + strata(indivL) + offset(log(interval)), 
                       data = simdata2, 
                       method = "exact")
      res_sccs_p[i] <- summary(sim.mod)$coefficients[5]
    } else {
      res_sccs_p[i] <- 1
    }
  }
  res_sccs_p

}

iteration <- 1000
rate_ratio <- c(1.2, 1.5, 2, 5)
n <- c(seq(10, 100, by = 5), seq(200, 500, by = 100))
# n <- c(seq(10, 100, by = 2))
risk_period <- c(14, 30, 42)
obserbed_age <- c(66, 67, 68, 70, 75)

set.seed(5678)
sccs_sim <- expand_grid(rate_ratio, n, risk_period, obserbed_age)　%>%
  mutate(no = row_number()) %>% 
  group_nest(no)

res_sccs_sim <- sccs_sim %>% 
  mutate(result = map(data, ~sample_size_sccs(data = .x), .progress = TRUE))

res_sccs_sim2 <- res_sccs_sim %>% 
  mutate(power = map_dbl(result, ~ (100*sum(.x < 0.05)/iteration))) %>% 
  unnest(data)

write_rds(res_sccs_sim2, "03_Output/res_sccs_yymmdd.rds")
