source("anja.R")
library(ggpubr)

## set start_date & end_date
start_date <- "2020-02-29"
end_date <- "2020-09-14"

## prepare data (date, t_i, Y_i) for each borough

df_hosp_wave1_nyc <-  df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(hospitalized_count)
  ) %>%
  select(!hospitalized_count)
  
df_hosp_wave1_bx <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, bx_hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(bx_hospitalized_count)
  ) %>%
  select(!bx_hospitalized_count)

df_hosp_wave1_bk <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, bk_hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(bk_hospitalized_count)
  ) %>%
  select(!bk_hospitalized_count)

df_hosp_wave1_mn <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, mn_hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(mn_hospitalized_count)
  ) %>%
  select(!mn_hospitalized_count)

df_hosp_wave1_qn <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, qn_hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(qn_hospitalized_count)
  ) %>%
  select(!qn_hospitalized_count)

df_hosp_wave1_si <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, si_hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(si_hospitalized_count)
  ) %>%
  select(!si_hospitalized_count)

## function to figure out starting parameters
get_start_hosp_wave1 <- function (df_wave, start_date = "2020-02-29", infl_point = "2020-04-03", d = 0.5) {
  # a is just the maximum number of cumulative cases in the wave
  a_start <- max(df_wave$Y_i)
  # t0 can be some t_i near the inflection point
  t0_start <- get_diff(start_date, infl_point)
  # k can be the slope near the inflection standardized by the cumulative cases near inflection point 
  lm_k <- lm(
    Y_i ~ t_i, data = df_wave %>% 
      filter(t_i %in% c(t0_start - 4, t0_start + 4))
  )
  k_start <- lm_k$coef[2] / df_wave %>% filter(t_i == t0_start) %>% pull(Y_i)
  # d can just be 0.5 
  d_start <- d
  # set starting theta vector
  start <- c(a_start, k_start, d_start, t0_start)
  return(start)
}


start_hosp_wave1_nyc <- get_start_hosp_wave1(df_hosp_wave1_nyc)
res_hosp_wave1_nyc <- NewtonRaphson(df_hosp_wave1_nyc, start_hosp_wave1_nyc)
final_hosp_wave1_nyc <- res_hosp_wave1_nyc[nrow(res_hosp_wave1_nyc), 4:7]


## apply newton-raphson to each borough
## Bronx
start_hosp_wave1_bx <- get_start_hosp_wave1(df_hosp_wave1_bx)
res_hosp_wave1_bx <- NewtonRaphson(df_hosp_wave1_bx, start_hosp_wave1_bx)
final_hosp_wave1_bx <- res_hosp_wave1_bx[nrow(res_hosp_wave1_bx), 4:7]

## Brooklyn
start_hosp_wave1_bk <- get_start_hosp_wave1(df_wave = df_hosp_wave1_bk, infl_point =  "2020-03-31")
res_hosp_wave1_bk <- NewtonRaphson(df_hosp_wave1_bk, start_hosp_wave1_bk)
final_hosp_wave1_bk <- res_hosp_wave1_bk[nrow(res_hosp_wave1_bk), 4:7]

## Manhattan
start_hosp_wave1_mn <- get_start_hosp_wave1(df_wave = df_hosp_wave1_mn, infl_point = "2020-03-31")
res_hosp_wave1_mn <- NewtonRaphson(df_hosp_wave1_mn, start_hosp_wave1_mn)
final_hosp_wave1_mn <- res_hosp_wave1_mn[nrow(res_hosp_wave1_mn), 4:7]

## Queens
start_hosp_wave1_qn <- get_start_hosp_wave1(df_hosp_wave1_qn)
res_hosp_wave1_qn <- NewtonRaphson(df_hosp_wave1_qn, start_hosp_wave1_qn)
final_hosp_wave1_qn <- res_hosp_wave1_qn[nrow(res_hosp_wave1_qn), 4:7]

## Staten Island
start_hosp_wave1_si <- get_start_hosp_wave1(df_wave = df_hosp_wave1_si, infl_point = "2020-04-01")
res_hosp_wave1_si <- NewtonRaphson(df_hosp_wave1_si, start_hosp_wave1_si)
final_hosp_wave1_si <- res_hosp_wave1_si[nrow(res_hosp_wave1_si), 4:7]


## plots
## Bronx
hosp_wave1_bx <- df_hosp_wave1_bx %>%
  mutate(
    Y_i_hat_start = N(t_i, start_hosp_wave1_bx),
    Y_i_hat_final = N(t_i, final_hosp_wave1_bx)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Bronx", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Brooklyn
hosp_wave1_bk <- df_hosp_wave1_bk %>%
  mutate(
    Y_i_hat_start = N(t_i, start_hosp_wave1_bk),
    Y_i_hat_final = N(t_i, final_hosp_wave1_bk)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Brooklyn", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Manhattan
hosp_wave1_mn <- df_hosp_wave1_mn %>%
  mutate(
    Y_i_hat_start = N(t_i, start_hosp_wave1_mn),
    Y_i_hat_final = N(t_i, final_hosp_wave1_mn)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Manhattan", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Queens
hosp_wave1_qn <- df_hosp_wave1_qn %>%
  mutate(
    Y_i_hat_start = N(t_i, start_hosp_wave1_qn),
    Y_i_hat_final = N(t_i, final_hosp_wave1_qn)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Queens", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Staten Island
hosp_wave1_si <- df_hosp_wave1_si %>%
  mutate(
    Y_i_hat_start = N(t_i, start_hosp_wave1_si),
    Y_i_hat_final = N(t_i, final_hosp_wave1_si)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Staten Island", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## NYC
hosp_wave1_nyc <- df_hosp_wave1_nyc %>%
  mutate(
    Y_i_hat_start = N(t_i, start_hosp_wave1_nyc),
    Y_i_hat_final = N(t_i, final_hosp_wave1_nyc)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "New York City", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 


ggarrange(hosp_wave1_nyc, hosp_wave1_bx, hosp_wave1_bk, hosp_wave1_mn, hosp_wave1_qn, hosp_wave1_si,  nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom") 
