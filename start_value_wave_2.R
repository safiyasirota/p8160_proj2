# choose a start as two times the cumulative cases at around inflection point
a_start <- 2 * max(df_wave$Y_i)

# choose inflection point at around the end of the data set
infl_point <- "2020-12-09"

# choose k as the standardized rate of change at inflection point
t0_start <- get_diff(start_date, infl_point)
lm_k <- lm(
  Y_i ~ t_i, data = df_wave %>% 
    filter(t_i %in% c(t0_start - 2, t0_start + 2))
)
k_start <- lm_k$coef[2] / df_wave %>% filter(t_i == t0_start) %>% pull(Y_i) 

# choose d to be 0.5
d_start <- 0.5

# set starting theta vector
start <- c(a_start, k_start, d_start, t0_start)
