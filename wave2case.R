source("anja.R")
library(ggpubr)

## set start_date & end_date
start_date <- "2020-09-15"
end_date <- "2020-12-11"

## prepare data (date, t_i, Y_i) for NYC
df_wave2_nyc <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(date:case_count) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(case_count)
  ) %>%
  select(!case_count)

## prepare data (date, t_i, Y_i) for each borough
df_wave2_bx <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, bx_case_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(bx_case_count)
  ) %>%
  select(!bx_case_count)

df_wave2_bk <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, bk_case_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(bk_case_count)
  ) %>%
  select(!bk_case_count)

df_wave2_mn <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, mn_case_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(mn_case_count)
  ) %>%
  select(!mn_case_count)

df_wave2_qn <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, qn_case_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(qn_case_count)
  ) %>%
  select(!qn_case_count)

df_wave2_si <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, si_case_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(si_case_count)
  ) %>%
  select(!si_case_count)

## function to figure out starting parameters
get_start <- function(df_wave, start_date = "2020-09-15", infl_point = "2020-12-09", d = 0.5) {
  # a is just the maximum number of cumulative cases in the wave
  a_start <- 2 * max(df_wave$Y_i)
  # t0 can be some t_i near the inflection point
  t0_start <- get_diff(start_date, infl_point)
  lm_k <- lm(
    Y_i ~ t_i, data = df_wave %>% 
      filter(t_i %in% c(t0_start - 2, t0_start + 2))
  )
  k_start <- lm_k$coef[2] / df_wave %>% filter(t_i == t0_start) %>% pull(Y_i) 
  # d can just be 0.5 
  d_start <- 0.5
  # set starting theta vector
  start <- c(a_start, k_start, d_start, t0_start)
  return(start)
}

## NYC
start_nyc <- get_start(df_wave2_nyc)
res_nyc <- NewtonRaphson(df_wave2_nyc, start_nyc)
final_nyc <- res_nyc[nrow(res_nyc), 4:7]

## apply newton-raphson to each borough
## Bronx
start_bx <- get_start(df_wave2_bx)
res_bx <- NewtonRaphson(df_wave2_bx, start_bx)
final_bx <- res_bx[nrow(res_bx), 4:7]

## Brooklyn
start_bk <- get_start(df_wave2_bk)
res_bk <- NewtonRaphson(df_wave2_bk, start_bk)
final_bk <- res_bk[nrow(res_bk), 4:7]

## Manhattan
start_mn <- get_start(df_wave2_mn)
res_mn <- NewtonRaphson(df_wave2_mn, start_mn)
final_mn <- res_mn[nrow(res_mn), 4:7]

## Queens
start_qn <- get_start(df_wave2_qn)
res_qn <- NewtonRaphson(df_wave2_qn, start_qn)
final_qn <- res_qn[nrow(res_qn), 4:7]

## Staten Island
start_si <- get_start(df_wave2_si, d = 0.25)
res_si <- NewtonRaphson(df_wave2_si, start_si)
final_si <- res_si[nrow(res_si), 4:7]


## plots
## Bronx
case_wave2_bx <- df_wave2_bx %>%
  mutate(
    Y_i_hat_start = N(t_i, start_bx),
    Y_i_hat_final = N(t_i, final_bx)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Bronx", x ="Days (since Sep 15, 2020)", y = "Cumulative cases") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Brooklyn
case_wave2_bk <- df_wave2_bk %>%
  mutate(
    Y_i_hat_start = N(t_i, start_bk),
    Y_i_hat_final = N(t_i, final_bk)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Brooklyn", x ="Days (since Sep 15, 2020)", y = "Cumulative cases") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Manhattan
case_wave2_mn <- df_wave2_mn %>%
  mutate(
    Y_i_hat_start = N(t_i, start_mn),
    Y_i_hat_final = N(t_i, final_mn)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Manhattan", x ="Days (since Sep 15, 2020)", y = "Cumulative cases") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Queens
case_wave2_qn <- df_wave2_qn %>%
  mutate(
    Y_i_hat_start = N(t_i, start_qn),
    Y_i_hat_final = N(t_i, final_qn)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Queens", x ="Days (since Sep 15, 2020)", y = "Cumulative cases") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Staten Island
case_wave2_si <- df_wave2_si %>%
  mutate(
    Y_i_hat_start = N(t_i, start_si),
    Y_i_hat_final = N(t_i, final_si)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Staten Island", x ="Days (since Sep 15, 2020)", y = "Cumulative cases") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## NYC
case_wave2_nyc <- df_wave2_nyc %>%
  mutate(
    Y_i_hat_start = N(t_i, start_nyc),
    Y_i_hat_final = N(t_i, final_nyc)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "New York City", x ="Days (since Sep 15, 2020)", y = "Cumulative cases") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 


ggarrange(case_wave2_nyc, case_wave2_bx, case_wave2_bk, case_wave2_mn, 
          case_wave2_qn, case_wave2_si,  nrow = 2, ncol = 3, 
          common.legend = TRUE, legend = "bottom") 

# Task 1.3

## NYC extended curve
more_days <- seq(from = as.Date("2020-12-12"), to = as.Date("2021-06-12"), 
                 by = 'day')
more_days <- ymd(more_days)
currow <- nrow(df_wave2_nyc)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

## Adding data to current data set
df_wave2_nyc_ext <- rbind(df_wave2_nyc, more_data)

case_wave2_nyc_ext <- df_wave2_nyc_ext %>%
  mutate(
    Y_i_hat_final = N(t_i, final_nyc)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = Y_i_hat_final, color = "red")) +
  labs(title = "New York City", x = "Days (since Sep 15, 2020)", y = "Cumulative cases") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300))

## BX extended curve

# Constructing a vector of future dates
more_days <- seq(from = as.Date("2020-12-12"), to = as.Date("2021-06-12"), 
                 by = 'day')
more_days <- ymd(more_days)
currow <- nrow(df_wave2_bx)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_wave2_bx_ext <- rbind(df_wave2_bx, more_data)

# Saving population value
pop_bx <- 1472654

# Making new plot
case_wave2_bx_ext <- df_wave2_bx_ext %>%
  mutate(
    Y_i_hat_final = N(t_i, final_bx)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = (Y_i/pop_bx)*100000), size = 1) +
  geom_line(aes(y = (Y_i_hat_final/pop_bx)*100000), color = "red") +
  labs(title = "Bronx", x ="Days (since Sep 15, 2020)", 
       y = "Cumulative cases per 100,000 capita") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 4500))

## BK extended curve

# Constructing a vector of future dates
more_days <- seq(from = as.Date("2020-12-12"), to = as.Date("2021-06-12"), 
                 by = 'day')
more_days <- ymd(more_days)
currow <- nrow(df_wave2_bk)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_wave2_bk_ext <- rbind(df_wave2_bk, more_data)

# Saving population value
pop_bk <- 2736074

# Making new plot
case_wave2_bk_ext <- df_wave2_bk_ext %>%
  mutate(
    Y_i_hat_final = N(t_i, final_bk)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = (Y_i/pop_bk)*100000), size = 1) +
  geom_line(aes(y = (Y_i_hat_final/pop_bk)*100000), color = "red") +
  labs(title = "Brooklyn", x ="Days (since Sep 15, 2020)", 
       y = "Cumulative cases per 100,000 capita") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 4500))

## MN extended curve

more_days <- seq(from = as.Date("2020-12-12"), to = as.Date("2021-06-12"), 
                 by = 'day')
more_days <- ymd(more_days)
currow <- nrow(df_wave2_mn)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_wave2_mn_ext <- rbind(df_wave2_mn, more_data)

# Saving population value
pop_mn <- 1694251

# Making new plot
case_wave2_mn_ext <- df_wave2_mn_ext %>%
  mutate(
    Y_i_hat_final = N(t_i, final_mn)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = (Y_i/pop_mn)*100000), size = 1) +
  geom_line(aes(y = (Y_i_hat_final/pop_mn)*100000), color = "red") +
  labs(title = "Manhattan", x ="Days (since Sep 15, 2020)", 
       y = "Cumulative cases per 100,000 capita") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 4500))

## QN extended curve

more_days <- seq(from = as.Date("2020-12-12"), to = as.Date("2021-06-12"), 
                 by = 'day')
more_days <- ymd(more_days)
currow <- nrow(df_wave2_qn)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_wave2_qn_ext <- rbind(df_wave2_qn, more_data)

# Saving population value
pop_qn <- 2405464

# Making new plot
case_wave2_qn_ext <- df_wave2_qn_ext %>%
  mutate(
    Y_i_hat_final = N(t_i, final_qn)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = (Y_i/pop_qn)*100000), size = 1) +
  geom_line(aes(y = (Y_i_hat_final/pop_qn)*100000), color = "red") +
  labs(title = "Queens", x ="Days (since Sep 15, 2020)", 
       y = "Cumulative cases per 100,000 capita") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 4500))

## SI extended curve

# Constructing a vector of future dates
more_days <- seq(from = as.Date("2020-12-12"), to = as.Date("2021-06-12"), 
                by = 'day')
more_days <- ymd(more_days)
currow <- nrow(df_wave2_si)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_wave2_si_ext <- rbind(df_wave2_si, more_data)

# Saving population value
pop_si <- 495747

# Making new plot
case_wave2_si_ext <- df_wave2_si_ext %>%
  mutate(
    Y_i_hat_final = N(t_i, final_si)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = (Y_i/pop_si)*100000), size = 1) +
  geom_line(aes(y = (Y_i_hat_final/pop_si)*100000), color = "red") +
  labs(title = "Staten Island", x ="Days (since Sep 15, 2020)", 
       y = "Cumulative cases per 100,000 capita") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 4500))


