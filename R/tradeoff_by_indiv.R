tradeoff_by_indiv <- function(for_analyses) {

  data <- for_analyses %>% filter(!is.na(mean_diff_dist)) %>%
    mutate(dist_to_last_yr = dist_to_last_yr/1000,
           mean_diff_dist = mean_diff_dist/1000)

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

  indiv_model <- lmer(dist_to_last_yr ~ (mean_diff_dist|id), data = repeats,
                      control = lmerControl(optimizer="optimx",
                                            optCtrl=list(method='nlminb')))

  # seems like the random effect might be doing something?
  fixed <- lm(dist_to_last_yr ~ mean_diff_dist, data = repeats)
  summary(fixed)
  # slope is 1.09 +- 0.04
  AIC(fixed, indiv_model)
  # not actually improving the model at all according to AIC
  # But biologically, i think something interesting is happening here

  # What about on standardized distances?
  repeats %<>% group_by(Herd) %>%
    mutate(z.spat = (dist_to_last_yr - mean(dist_to_last_yr))/sd(dist_to_last_yr),
           z.soc = (mean_diff_dist - mean(mean_diff_dist))/sd(mean_diff_dist))

  z_indiv <- lmer(z.spat ~ (z.soc|id), data = repeats)
  z_fixed <- lm(z.spat ~ (z.soc), data = repeats)
  AIC(z_indiv, z_fixed)
  # again, not an improvement on the fixed model


  std.slopes <- coef(z_indiv)$id
  std.slopes$id <- row.names(std.slopes)
  median(std.slopes$z.soc) # 0.35
  mean(std.slopes$z.soc) # 0.40
  # The relationship persists when looking at standardized values of spatial and social fidelity - it's not just a consequence of the social metric being noisier

  ## Extracting slopes ----
  slopes <- coef(indiv_model)$id
  slopes$id <- row.names(slopes)
  median(slopes$mean_diff_dist) # 0.62
  mean(slopes$mean_diff_dist) # 0.68
  slopes %<>% rename(slope = mean_diff_dist)
  7/56 # 12.5 % of individuals had a negative relationship

  repeats <- left_join(repeats, slopes, by = "id")

  return(repeats)

}
