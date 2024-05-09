ln_tradeoff_model <- function(DT) {

  data <- DT %>% filter(!is.na(mean_ln_dist)) %>%
    mutate(dist_to_last_yr = dist_to_last_yr/1000)

  m <- lme4::lmer(
    formula = mean_ln_dist ~ dist_to_last_yr + (1|season_yr),
    data = data)

  params <- parameters::parameters(m)
  insight::standardize_names(params)


}
