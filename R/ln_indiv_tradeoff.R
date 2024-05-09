ln_indiv_tradeoff <- function(for_analyses) {

  data <- for_analyses %>% filter(!is.na(mean_ln_dist)) %>%
    mutate(dist_to_last_yr = dist_to_last_yr/1000)

  indiv <- data %>% group_by(id) %>%
    summarise(n = n()) %>%
    mutate(incl = ifelse(n > 2, "yes", "no"))
  # 56 of 117 individuals have at least 3 points

  indiv %>%
    group_by(incl) %>%
    summarise(n = sum(n))
  # 213 of 298 obs retained, 71% of the obs

  reps <- c(subset(indiv, incl == "yes")$id)

  repeats <- data %>% filter(id %in% reps)

  indiv_model <- lmer(mean_ln_dist ~ (dist_to_last_yr|id), data = repeats,
                      control = lmerControl(optimizer="optimx",
                                            optCtrl=list(method='nlminb')))

  # seems like the random effect might be doing something?
  fixed <- lm(mean_ln_dist ~ dist_to_last_yr, data = repeats)
  summary(fixed)
  # slope is 0.143 +- 0.001
  AIC(fixed, indiv_model)
  # not actually improving the model at all according to AIC
  # But biologically, i think something interesting is happening here

  # What about on standardized distances?
  repeats %<>% group_by(Herd) %>%
    mutate(z.spat = (dist_to_last_yr - mean(dist_to_last_yr))/sd(dist_to_last_yr))

  z_indiv <- lmer(mean_ln_dist ~ (z.spat|id), data = repeats)
  z_fixed <- lm(mean_ln_dist ~ (z.spat), data = repeats)

  AIC(z_indiv, z_fixed)
  # when using standardized spatial fidelity, including ID does substantially
  #  improve the model but AIC of the non-standardized are overall better than
  #  either z-score model leave the z-score alone for now

  ## Extracting slopes ----
  slopes <- coef(indiv_model)$id
  slopes$id <- row.names(slopes)
  median(slopes$dist_to_last_yr) # 0.0036
  mean(slopes$dist_to_last_yr) # 0.0031
  slopes %<>% rename(slope = dist_to_last_yr)
  # 23 of 56 individuals were negative, in fact, but we're talking -0.008 to + 0.02

  repeats <- left_join(repeats, slopes, by = "id")

  return(repeats)

}
