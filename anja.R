
### SET UP ---------------------------------------------------------------------
# load packages
library(lubridate)
library(tidyverse)
theme_set(
  theme_bw()
)

# read in data
df <- read_csv("data-by-day year 1.csv")

# clean data frame
df <- df %>%
  # looks like there's a bunch of NA rows so let's remove those 
  filter(if_all(everything(), ~ !is.na(.))) %>%
  # make all column names lower case
  rename_with(tolower) %>%
  # rename date column and make it a date object
  rename(date = date_of_interest) %>%
  mutate(date = mdy(date))

# let's look at the data to identify the pandemic waves
df %>%
  select(date:death_count) %>%
  # mutate(across(case_count:death_count, cumsum)) %>%
  pivot_longer(case_count:death_count, names_to = "type", values_to = "val") %>%
  ggplot(aes(x = date, y = val, color = type)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") 
  
# maybe first wave could be "2020-02-29" to "2020-06-01"???

### GRADIENT DESCENT -----------------------------------------------------------
# function to calculate difference in number of days between two dates
get_diff <- function(x,y) time_length(x %--% y, unit = "day")
# function to calculate N(t_i, theta)
N <- function(t_i, theta) {
  a <- theta[1]
  k <- theta[2]
  d <- theta[3]
  t0 <- theta[4]
  a * (1 + d * exp(-k * (t_i - t0)))^(-1/d)
}

# we'll just use the city-wide, first-wave data for our cum case count input into the algo
start_date <- "2020-02-29"
end_date <- "2020-09-14"
df_wave <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  select(date:case_count) %>%
  mutate(
    t_i = get_diff(start_date, date),
    Y_i = cumsum(case_count)
  ) %>%
  select(!case_count)

# look at cumulative cases over the wave 
df_wave %>% 
  ggplot(aes(x = date, y = Y_i)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

# figure out starting parameters
# a is just the maximum number of cumulative cases in the wave
a_start <- max(df_wave$Y_i)
# t0 can be some t_i near the inflection point
infl_point <- "2020-04-01"
t0_start <- get_diff(start_date, infl_point)
# k can be the slope near the inflection standardized by the cumulative cases near inflection point 
lm_k <- lm(
  Y_i ~ t_i, data = df_wave %>% 
    filter(t_i %in% c(t0_start - 4, t0_start + 4))
)
k_start <- lm_k$coef[2] / df_wave %>% filter(t_i == t0_start) %>% pull(Y_i)
# d can just be 0.5 
d_start <- 0.5
# set starting theta vector
start <- c(a_start, k_start, d_start, t0_start)

# look at Y_hat based on the starting theta vector
df_wave %>%
  mutate(Y_i_hat = N(t_i, start)) %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = Y_i_hat), color = "red")

# function to calculate objective f'n (sse) and its gradient
sse_stuff <- function(df_wave, theta) {
  a <- theta[1]
  k <- theta[2]
  d <- theta[3]
  t0 <- theta[4]
  t <- df_wave$t_i
  Y <- df_wave$Y_i
  Y_hat <- N(t, theta)
  # calculate objective function f
  diff <- Y - Y_hat
  f <- sum(diff^2)
  # calculate gradient for N(t,theta)
  da_N <- (1 + d * exp(-k*(t-t0)))^(-1/d)
  dk_N <- (a * (t-t0) * exp(-k*(t-t0))) / (1 + d * exp(-k*(t-t0)))^(1 + 1/d)
  dd_N <- -a * (d * exp(-k*(t-t0)) - log(1 + d*exp(-k*(t-t0))) * (1 + d * exp(-k*(t-t0)))) / 
    (d^2 * ((1 + d * exp(-k*(t-t0)))^(1 + 1/d)))
  dt0_N <- - (a * k * exp(-k*(t-t0))) / (1 + d * exp(-k*(t-t0)))^(1 + 1/d)
  # calculate gradient for objective function f
  grad_f <- c(-2 * sum(diff*da_N),
              -2 * sum(diff*dk_N),
              -2 * sum(diff*dd_N),
              -2 * sum(diff*dt0_N))

  res <- list(f = f, grad = grad_f)
  return(res)
}
# sse_stuff(df_wave, start)

# df_wave: data frame of t_i, Y_i for the wave of interest
# start: starting theta vector
# tol: tolerance needed to stop iterating
# maxiter: max number of iterations 
NewtonRaphson <- function(df_wave, start, tol=1e-5, maxiter = 10000) {
  # set up
  i <- 0
  cur <- start
  stuff <- sse_stuff(df_wave, cur)
  res <- matrix(0, nrow = 1, ncol = 7)
  colnames(res) <- c(
    "i", "lambda", "sse",
    "a", "k", "d", "t0"
  )
  res[1,] <- c(0, 1, stuff$f, cur)
  prevf <- -Inf   
  # begin iterating to update current theta vector
  while(i < maxiter && abs(stuff$f - prevf) > tol){
    i <- i + 1
    prev <- cur
    prevf <- stuff$f
    prevgrad <- stuff$grad
    # set lambda to start off
    lambda <- 1e-10
    # calculate new values where lambda = 1 
    cur <- prev - lambda * prevgrad
    stuff <- sse_stuff(df_wave, cur)
    # continue to decrease lambda until we see a decrease in SSE
    while(stuff$f >= prevf | any(is.na(stuff$grad))) {
      # divide lambda
      lambda <- lambda / 10
      # calculate new values at the updated lambda value and its corresponding information
      cur <- prev - lambda * prevgrad
      stuff <- sse_stuff(df_wave, cur)
    }
    res <- rbind(res, c(i, lambda, stuff$f, cur))
  }
  return(res)
}
res <- suppressWarnings(NewtonRaphson(df_wave, start))
head(res)
tail(res)
final <- res[nrow(res), 4:7]
  
df_wave %>%
  mutate(
    Y_i_hat_start = N(t_i, start),
    Y_i_hat_final = N(t_i, final)
  ) %>%
  pivot_longer(Y_i_hat_start:Y_i_hat_final, names_to = "type", values_to = "value") %>%
  ggplot(aes(x = t_i)) + 
  geom_point(aes(y = Y_i), size = 1) +
  geom_line(aes(y = value, color = type))



