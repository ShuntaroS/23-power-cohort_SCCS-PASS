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

iteration <- 5000
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

write_rds(res_sim2, "03_Output/result_cohort_231110.rds")
# res_sim2 <- read_rds("03_Output/result_cohort_230720.rds")
# 
# df_cohort_power <- res_sim2 %>% 
#   select(-no, -result) %>% 
#   filter(n1 %in% c(100000, 500000, 1000000, 2000000, 3000000, 4000000, 5000000, 8000000, 10000000)) %>% 
#   mutate(ratio = factor(ratio),
#          risk_ratio = factor(risk_ratio, labels = c("Risk ratio: 1.2", 
#                                                     "Risk ratio: 1.5",
#                                                     "Risk ratio: 2",
#                                                     "Risk ratio: 5")),
#          prevalence = factor(prevalence, labels = c("Prevalence: 0.7/100,000 (GBS)",
#                                                     "Prevalence: 9.5/100,000 (ITP)")))
# 
# fig_cohort_power <- ggplot(df_cohort_power, aes(x = n1, y = power, color = ratio, shape = ratio, group = ratio)) +
#   geom_point() +
#   geom_line() +
#   theme_bw() +
#   facet_grid(risk_ratio ~ prevalence) +
#   xlab("No. of exposured") +
#   ylab("Power (%)") +
#   labs(color = "No. of unexposured / no. of exposured",
#        shape = "No. of unexposured / no. of exposured") +
#   scale_x_continuous(breaks = c(100000, 500000, 1000000, 2000000, 3000000, 4000000, 5000000, 8000000, 10000000),
#                      labels = number_format()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         legend.position = "bottom",
#         panel.grid.minor = element_blank())
#   
# fig_cohort_power
# ggsave("03_Output/fig_cohort.pdf", width = 8, height = 8*0.68)

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

iteration <- 5000
rate_ratio <- c(1.2, 1.5, 2, 5)
n <- c(seq(10, 300, by = 10))
# n <- c(seq(10, 100, by = 2))
risk_period <- c(14, 30, 42)
obserbed_age <- c(66, 68, 70, 75)

set.seed(5678)
sccs_sim <- expand_grid(rate_ratio, n, risk_period, obserbed_age)　%>%
  mutate(no = row_number()) %>% 
  group_nest(no)

res_sccs_sim <- sccs_sim %>% 
  mutate(result = map(data, ~sample_size_sccs(data = .x), .progress = TRUE))

res_sccs_sim2 <- res_sccs_sim %>% 
  mutate(power = map_dbl(result, ~ (100*sum(.x < 0.05)/iteration))) %>% 
  unnest(data)

write_rds(res_sccs_sim2, "03_Output/res_sccs_231120.rds")
# res_sccs_sim2 <- read_rds("03_Output/res_sccs_230720.rds")
# 
# df_sccs_power <- res_sccs_sim2 %>% 
#   select(-no, -result) %>% 
#   # filter(n1 %in% c(100000, 500000, 1000000, 2000000, 3000000, 4000000, 5000000, 8000000, 10000000)) %>% 
#   filter(obserbed_age <= 75) %>% 
#   mutate(obserbed_age = factor(obserbed_age, labels = c("1 yr",
#                                                         "2 yrs",
#                                                         "3 yrs",
#                                                         "5 yrs",
#                                                         "10 yrs")),
#          risk_period = factor(risk_period),
#          rate_ratio = factor(rate_ratio, labels = c("Rate ratio: 1.2", 
#                                                     "Rate ratio: 1.5",
#                                                     "Rate ratio: 2",
#                                                     "Rate ratio: 5")))
# 
# fig_sccs_power <- ggplot(df_sccs_power, aes(x = n, y = power)) +
#   geom_point(size = 0.5) +
#   geom_line() +
#   theme_bw() +
#   facet_grid(rate_ratio ~ obserbed_age) +
#   xlab("No. of event") +
#   ylab("Power (%)") +
#   # labs(color = "No. of unexposured / no. of exposured",
#   #      shape = "No. of unexposured / no. of exposured") +
#   # scale_x_continuous(breaks = c(100000, 500000, 1000000, 2000000, 3000000, 4000000, 5000000, 8000000, 10000000),
#   #                    labels = number_format()) +
#   scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         legend.position = "bottom",
#         panel.grid.minor = element_blank())
# 
# fig_sccs_power
# ggsave("03_Output/fig_sccs.pdf", width = 8, height = 8*0.68)
