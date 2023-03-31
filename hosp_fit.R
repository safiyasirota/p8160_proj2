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
    #Y_i_hat_start = N(t_i, start_hosp_wave1_bx),
    Y_i_hat_final = N(t_i, final_hosp_wave1_bx)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Bronx", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Brooklyn
hosp_wave1_bk <- df_hosp_wave1_bk %>%
  mutate(
   # Y_i_hat_start = N(t_i, start_hosp_wave1_bk),
    Y_i_hat_final = N(t_i, final_hosp_wave1_bk)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Brooklyn", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Manhattan
hosp_wave1_mn <- df_hosp_wave1_mn %>%
  mutate(
   # Y_i_hat_start = N(t_i, start_hosp_wave1_mn),
    Y_i_hat_final = N(t_i, final_hosp_wave1_mn)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Manhattan", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Queens
hosp_wave1_qn <- df_hosp_wave1_qn %>%
  mutate(
   # Y_i_hat_start = N(t_i, start_hosp_wave1_qn),
    Y_i_hat_final = N(t_i, final_hosp_wave1_qn)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Queens", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Staten Island
hosp_wave1_si <- df_hosp_wave1_si %>%
  mutate(
   # Y_i_hat_start = N(t_i, start_hosp_wave1_si),
    Y_i_hat_final = N(t_i, final_hosp_wave1_si)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Staten Island", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## NYC
hosp_wave1_nyc <- df_hosp_wave1_nyc %>%
  mutate(
    #Y_i_hat_start = N(t_i, start_hosp_wave1_nyc),
    Y_i_hat_final = N(t_i, final_hosp_wave1_nyc)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "New York City", x ="Days (since Feb 29, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 


ggarrange(hosp_wave1_nyc, hosp_wave1_bx, hosp_wave1_bk, hosp_wave1_mn, hosp_wave1_qn, hosp_wave1_si,  nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom") 

############################### WAVE 2 NOW ! #####################################
## set start_date & end_date
start_date <- "2020-09-15"
end_date <- "2020-12-11"

## prepare data (date, t_i, Y_i) for NYC
df_hosp_wave2_nyc <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(date:hospitalized_count) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(hospitalized_count)
  ) %>%
  select(!hospitalized_count)

## prepare data (date, t_i, Y_i) for each borough
df_hosp_wave2_bx <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, bx_hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(bx_hospitalized_count)
  ) %>%
  select(!bx_hospitalized_count)

df_hosp_wave2_bk <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, bk_hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(bk_hospitalized_count)
  ) %>%
  select(!bk_hospitalized_count)

df_hosp_wave2_mn <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, mn_hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(mn_hospitalized_count)
  ) %>%
  select(!mn_hospitalized_count)

df_hosp_wave2_qn <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, qn_hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(qn_hospitalized_count)
  ) %>%
  select(!qn_hospitalized_count)

df_hosp_wave2_si <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(c(date, si_hospitalized_count)) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(si_hospitalized_count)
  ) %>%
  select(!si_hospitalized_count)

## function to figure out starting parameters
get_start <- function(df_wave, start_date = "2020-09-15", infl_point = "2020-12-09", d = 0.5) {
  # a is just the maximum number of cumulative cases in the wave
  a_start <- 2.2 * max(df_wave$Y_i)
  # t0 can be some t_i near the inflection point
  t0_start <- get_diff(start_date, infl_point)
  lm_k <- lm(
    Y_i ~ t_i, data = df_wave %>% 
      filter(t_i %in% c(t0_start - 2, t0_start + 2))
  )
  k_start <- lm_k$coef[2] / df_wave %>% filter(t_i == t0_start) %>% pull(Y_i) 
  # d can just be 0.5 
  d_start <- d
  # set starting theta vector
  start <- c(a_start, k_start, d_start, t0_start)
  return(start)
}

## NYC
start_hosp_wave2_nyc <- get_start(df_wave = df_hosp_wave2_nyc)
res_hosp_wave2_nyc <- NewtonRaphson(df_hosp_wave2_nyc, start_hosp_wave2_nyc)
final_hosp_wave2_nyc <- res_hosp_wave2_nyc[nrow(res_hosp_wave2_nyc), 4:7]

## apply newton-raphson to each borough
## Bronx
start_hosp_wave2_bx <- get_start(df_hosp_wave2_bx)
res_hosp_wave2_bx <- NewtonRaphson(df_hosp_wave2_bx, start_hosp_wave2_bx)
final_hosp_wave2_bx <- res_hosp_wave2_bx[nrow(res_hosp_wave2_bx), 4:7]

## Brooklyn
start_hosp_wave2_bk <- get_start(df_hosp_wave2_bk)
res_hosp_wave2_bk <- NewtonRaphson(df_hosp_wave2_bk, start_hosp_wave2_bk)
final_hosp_wave2_bk <- res_hosp_wave2_bk[nrow(res_hosp_wave2_bk), 4:7]

## Manhattan
start_hosp_wave2_mn <- get_start(df_hosp_wave2_mn)
res_hosp_wave2_mn <- NewtonRaphson(df_hosp_wave2_mn, start_hosp_wave2_mn)
final_hosp_wave2_mn <- res_hosp_wave2_mn[nrow(res_hosp_wave2_mn), 4:7]

## Queens
start_hosp_wave2_qn <- get_start(df_hosp_wave2_qn,infl_point = "2020-12-09")
res_hosp_wave2_qn <- NewtonRaphson(df_hosp_wave2_qn, start_hosp_wave2_qn)
final_hosp_wave2_qn <- res_hosp_wave2_qn[nrow(res_hosp_wave2_qn), 4:7]

## Staten Island
start_hosp_wave2_si <- get_start(df_hosp_wave2_si, d=0.25 )
res_hosp_wave2_si <- NewtonRaphson(df_hosp_wave2_si, start_hosp_wave2_si)
final_hosp_wave2_si <- res_hosp_wave2_si[nrow(res_hosp_wave2_si), 4:7]


## plots
## Bronx
hosp_wave2_bx <- df_hosp_wave2_bx %>%
  mutate(
   # Y_i_hat_start = N(t_i, start_hosp_wave2_bx),
    Y_i_hat_final = N(t_i, final_hosp_wave2_bx)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Bronx", x ="Days (since Sep 15, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Brooklyn
hosp_wave2_bk <- df_hosp_wave2_bk %>%
  mutate(
   # Y_i_hat_start = N(t_i, start_hosp_wave2_bk),
    Y_i_hat_final = N(t_i, final_hosp_wave2_bk)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Brooklyn", x ="Days (since Sep 15, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Manhattan
hosp_wave2_mn <- df_hosp_wave2_mn %>%
  mutate(
   # Y_i_hat_start = N(t_i, start_hosp_wave2_mn),
    Y_i_hat_final = N(t_i, final_hosp_wave2_mn)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Manhattan", x ="Days (since Sep 15, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Queens
hosp_wave2_qn <- df_hosp_wave2_qn %>%
  mutate(
  #  Y_i_hat_start = N(t_i, start_hosp_wave2_qn),
    Y_i_hat_final = N(t_i, final_hosp_wave2_qn)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Queens", x ="Days (since Sep 15, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## Staten Island
hosp_wave2_si <- df_hosp_wave2_si %>%
  mutate(
   # Y_i_hat_start = N(t_i, start_hosp_wave2_si),
    Y_i_hat_final = N(t_i, final_hosp_wave2_si)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "Staten Island", x ="Days (since Sep 15, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 

## NYC
hosp_wave2_nyc <- df_hosp_wave2_nyc %>%
  mutate(
  #  Y_i_hat_start = N(t_i, start_hosp_wave2_nyc),
    Y_i_hat_final = N(t_i, final_hosp_wave2_nyc)
  ) %>%
  #pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  pivot_longer(Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type)) +
  labs(title = "New York City", x ="Days (since Sep 15, 2020)", y = "Cumulative hospitalizations") +   
  scale_color_discrete(name = "Fitted model", labels = c("Final", "Start")) 


ggarrange(hosp_wave2_nyc, hosp_wave2_bx, hosp_wave2_bk, hosp_wave2_mn, hosp_wave2_qn, hosp_wave2_si,  nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom") 

#############################PREDICTIONS!#####################################

# Constructing a vector of future dates
more_days <- seq(from = as.Date("2020-12-12"), to = as.Date("2021-06-12"), 
                 by = 'day')
more_days <- ymd(more_days)
currow <- nrow(df_hosp_wave2_nyc)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_hosp_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_hosp_wave2_nyc_next <- rbind(df_hosp_wave2_nyc %>% select(-case_count), more_hosp_data)

hosp_wave2_nyc_next <- df_hosp_wave2_nyc_next %>%
  mutate(
    Y_i_hat_final = N(t_i, final_hosp_wave2_nyc)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = Y_i_hat_final, color = "red")) +
  labs(title = "New York City", x ="Days (since Sep 15, 2020)", y = "Cumulative hospitalizations") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300))

###BX
currow <- nrow(df_hosp_wave2_bx)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_hosp_bx_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_hosp_wave2_bx_next <- rbind(df_hosp_wave2_bx, more_hosp_bx_data)

# Saving population value
pop_bx <- 1472654

# Making new plot
hosp_wave2_bx_next <- df_hosp_wave2_bx_next %>%
  mutate(
    Y_i_hat_final = N(t_i, final_hosp_wave2_bx)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = (Y_i/pop_bx)*100000), size = 1) +
  geom_line(aes(y = (Y_i_hat_final/pop_bx)*100000), color = "red") +
  labs(title = "Bronx", x ="Days (since Sep 15, 2020)", 
       y = "Cumulative hospitalizations per 100,000 capita") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 400))

#####bk
currow <- nrow(df_hosp_wave2_bk)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_hosp_bk_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_hosp_wave2_bk_next <- rbind(df_hosp_wave2_bk, more_hosp_bk_data)

# Saving population value
pop_bk <- 2736074

# Making new plot
hosp_wave2_bk_next <- df_hosp_wave2_bk_next %>%
  mutate(
    Y_i_hat_final = N(t_i, final_hosp_wave2_bk)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = (Y_i/pop_bk)*100000), size = 1) +
  geom_line(aes(y = (Y_i_hat_final/pop_bk)*100000), color = "red") +
  labs(title = "Brooklyn", x ="Days (since Sep 15, 2020)", 
       y = "Cumulative hospitalizations per 100,000 capita") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 400))


####mn
currow <- nrow(df_hosp_wave2_mn)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_hosp_mn_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_hosp_wave2_mn_next <- rbind(df_hosp_wave2_mn, more_hosp_mn_data)

# Saving population value
pop_mn <- 1694251

# Making new plot
hosp_wave2_mn_next <- df_hosp_wave2_mn_next %>%
  mutate(
    Y_i_hat_final = N(t_i, final_hosp_wave2_mn)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = (Y_i/pop_mn)*100000), size = 1) +
  geom_line(aes(y = (Y_i_hat_final/pop_mn)*100000), color = "red") +
  labs(title = "Manhattan", x ="Days (since Sep 15, 2020)", 
       y = "Cumulative hospitalizations per 100,000 capita") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 400))


#####qn
currow <- nrow(df_hosp_wave2_qn)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_hosp_qn_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_hosp_wave2_qn_next <- rbind(df_hosp_wave2_qn, more_hosp_qn_data)

# Saving population value
pop_qn <- 2405464

# Making new plot
hosp_wave2_qn_next <- df_hosp_wave2_qn_next %>%
  mutate(
    Y_i_hat_final = N(t_i, final_hosp_wave2_qn)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = (Y_i/pop_qn)*100000), size = 1) +
  geom_line(aes(y = (Y_i_hat_final/pop_qn)*100000), color = "red") +
  labs(title = "Queens", x ="Days (since Sep 15, 2020)", 
       y = "Cumulative hospitalizations per 100,000 capita") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 400))


######
currow <- nrow(df_hosp_wave2_si)
more_times <- seq(from = currow + 1, 
                  to = length(more_days) + currow)
Ys <- rep(NA, length(more_days))
more_hosp_si_data <- tibble(date = more_days, t_i = more_times, Y_i = Ys)

# Adding data to current data set
df_hosp_wave2_si_next <- rbind(df_hosp_wave2_si, more_hosp_si_data)

# Saving population value
pop_si <- 495747

# Making new plot
hosp_wave2_si_next <- df_hosp_wave2_si_next %>%
  mutate(
    Y_i_hat_final = N(t_i, final_hosp_wave2_si)
  ) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = (Y_i/pop_si)*100000), size = 1) +
  geom_line(aes(y = (Y_i_hat_final/pop_si)*100000), color = "red") +
  labs(title = "Staten Island", x ="Days (since Sep 15, 2020)", 
       y = "Cumulative hospitalizations per 100,000 capita") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 400))


ggarrange(hosp_wave2_bx_next, 
          hosp_wave2_bk_next, hosp_wave2_mn_next, 
          hosp_wave2_qn_next, hosp_wave2_si_next,  
          nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom") 

