library(tidyverse)
library(scales)
# remotes::install_github("idmn/ggview", force = TRUE)
library(ggview)


res_sim2 <- read_rds("03_Output/result_cohort_231110.rds")

df_cohort_power <- res_sim2 %>%
  select(-no, -result) %>%
  # filter(n1 %in% c(100000, 500000, 1000000, 2000000, 3000000, 4000000, 5000000)) %>%
  mutate(ratio = factor(ratio),
         risk_ratio = factor(risk_ratio, labels = c("Risk ratio: 1.2",
                                                    "Risk ratio: 1.5",
                                                    "Risk ratio: 2",
                                                    "Risk ratio: 5")),
         prevalence = factor(prevalence, labels = c("Prevalence of unvaccinated:\n 1/100,000",
                                                    "Prevalence of unvaccinated:\n 5/100,000",
                                                    "Prevalence of unvaccinated:\n 10/100,000",
                                                    "Prevalence of unvaccinated:\n 50/100,000",
                                                    "Prevalence of unvaccinated:\n 100/100,000")))

fig_cohort_power <- ggplot(df_cohort_power, aes(x = n1, y = power, color = ratio, linetype = ratio, shape = ratio, group = ratio)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_grid(risk_ratio ~ prevalence) +
  xlab("Sample size of vaccinated") +
  ylab("Power (%)") +
  labs(
    color = "Ratio of vaccinated to unvaccinated",
    linetype = "Ratio of vaccinated to unvaccinated",
       shape = "Ratio of vaccinated to unvaccinated") +
  scale_x_continuous(breaks = c(100000, 500000, 1000000, 2000000, 3000000, 4000000, 5000000, 8000000, 10000000),
                     labels = number_format()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

fig_cohort_power
ggview(width = 10, height = 10*0.68)
ggsave("03_Output/fig_cohort_231129.pdf", width = 10, height = 10*0.68)
ggsave("03_Output/fig_cohort_231129.eps", width = 10, height = 10*0.68)


res_sccs_sim2 <- read_rds("03_Output/res_sccs_231120.rds")

df_sccs_power <- res_sccs_sim2 %>%
  select(-no, -result) %>%
  # filter(n1 %in% c(100000, 500000, 1000000, 2000000, 3000000, 4000000, 5000000, 8000000, 10000000)) %>%
  filter(obserbed_age <= 75) %>%
  mutate(obserbed_age = factor(obserbed_age, labels = c("1 yr",
                                                        "3 yrs",
                                                        "5 yrs",
                                                        "10 yrs")),
         risk_period = factor(risk_period, labels = c("Risk period: 14 days",
                                                      "Risk period: 30 days",
                                                      "Risk period: 42 days")),
         rate_ratio = factor(rate_ratio, labels = c("Rate ratio: 1.2",
                                           "Rate ratio: 1.5",
                                           "Rate ratio: 2",
                                           "Rate ratio: 5")))

fig_sccs_power_rp14 <- df_sccs_power %>% 
  filter(risk_period == "Risk period: 14 days")　%>% 
  ggplot(aes(x = n, y = power)) +
  geom_point(size = 0.5) +
  geom_line() +
  theme_bw() +
  facet_grid(rate_ratio ~ obserbed_age) +
  xlab("The number of events") +
  ylab("Power (%)") +
  labs(title = "Risk period: 14 days") +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

fig_sccs_power_rp14
ggview(width = 8, height = 8*0.68)
ggsave("03_Output/fig_sccs_power_rp14_231126.pdf", width = 8, height = 8*0.68)
ggsave("03_Output/fig_sccs_power_rp14_231126.eps", width = 8, height = 8*0.68)

fig_sccs_power_rp30 <- df_sccs_power %>% 
  filter(risk_period == "Risk period: 30 days") %>% 
  ggplot(aes(x = n, y = power)) +
  geom_point(size = 0.5) +
  geom_line() +
  theme_bw() +
  facet_grid(rate_ratio ~ obserbed_age) +
  xlab("The number of events") +
  ylab("Power (%)") +
  labs(title = "Risk period: 30 days") +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

fig_sccs_power_rp30
ggview(width = 8, height = 8*0.68)
ggsave("03_Output/fig_sccs_power_rp30_231126.pdf", width = 8, height = 8*0.68)
ggsave("03_Output/fig_sccs_power_rp30_231126.eps", width = 8, height = 8*0.68)

fig_sccs_power_rp42 <- df_sccs_power %>% 
  filter(risk_period == "Risk period: 42 days") %>% 
  ggplot(aes(x = n, y = power)) +
  geom_point(size = 0.5) +
  geom_line() +
  theme_bw() +
  facet_grid(rate_ratio ~ obserbed_age) +
  xlab("The number of events") +
  ylab("Power (%)") +
  labs(title = "Risk period: 42 days") +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

fig_sccs_power_rp42
ggview(width = 8, height = 8*0.68)
ggsave("03_Output/fig_sccs_power_rp42_231126.pdf", width = 8, height = 8*0.68)
ggsave("03_Output/fig_sccs_power_rp42_231126.eps", width = 8, height = 8*0.68)

