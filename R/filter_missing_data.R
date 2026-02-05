filter_low_coverage_days <- function(df, min_frac = 0.9, steps_per_day = 48) {

  min_steps <- ceiling(min_frac * steps_per_day)

  df %>%
    group_by(date = floor_date(date, "day")) %>%
    mutate(n_steps = n()) %>%
    ungroup() %>%
    filter(n_steps >= min_steps) %>%
    select(-n_steps)
}
