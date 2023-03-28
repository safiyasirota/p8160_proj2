a_start <- 2 * max(df_wave$Y_i)
# t0 can be some t_i near the inflection point
infl_point <- "2020-12-09"
t0_start <- get_diff(start_date, infl_point)
# k can be the slope near the inflection standardized by the cumulative cases near inflection point 
lm_k <- lm(
  Y_i ~ t_i, data = df_wave %>% 
    filter(t_i %in% c(t0_start - 2, t0_start + 2))
)
k_start <- lm_k$coef[2] / df_wave_1 %>% filter(t_i == t0_start_1) %>% pull(Y_i) 
d_start <- 0.5
# set starting theta vector
start <- c(a_start, k_start, d_start, t0_start)