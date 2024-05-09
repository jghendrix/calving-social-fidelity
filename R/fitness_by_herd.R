fitness_by_herd <- function(for_analyses) {

  for_analyses %<>% mutate(Survival = ifelse(lost == "FALSE", 1, 0))

  ## Fitness by spatial fidelity
  ggplot(for_analyses, aes(x = herd,
                           y = dist_to_last_yr/1000,
                           colour = lost)) +
    #geom_point(size = 2, alpha = 0.8, position = position_dodge(width = 0.5)) +
    geom_boxplot() +
    scale_colour_viridis(discrete = TRUE) +
    theme_bw() +
    ylab("Distance to prior year's calving site (km)") +
    xlab("Herd")
  ggsave("graphics/fitness_spatial-fidelity.png",
         width = 2000, height = 1500, units="px")

  # Does not appear that being born closer to last year makes you more likely to survive
  # BUT a lot of these lost == FALSE are really unknowns, not confirmed surviving


  ## Fitness by neighbour distance change?
  ggplot(for_analyses, aes(x = herd,
                           y = mean_diff_dist/1000,
                           colour = lost)) +
    geom_boxplot() +
    scale_colour_viridis(discrete = TRUE) +
    theme_bw() +
    ylab("Average change in distance to each neighbour (km)") +
    xlab("Herd")
  ggsave("graphics/fitness_social-fidelity.png",
         width = 2000, height = 1500, units="px")

  ## Fitness by log() neighbour distance change?
  ggplot(for_analyses, aes(x = herd,
                           y = mean_ln_dist,
                           colour = lost)) +
    geom_boxplot() +
    scale_colour_viridis(discrete = TRUE) +
    theme_bw() +
    ylab("Average change in log(distance) to each neighbour (km)") +
    xlab("Herd")
  ggsave("graphics/fitness_log-social-fidelity.png",
         width = 2000, height = 1500, units="px")


  ## Fitness by calving network strength
  ggplot(for_analyses, aes(y = strength_calving,
                           x = herd,
                           colour = lost)) +
    geom_boxplot() +
    scale_colour_viridis(discrete = TRUE) +
    theme_bw() +
    ylab("Network strength in post-calving period") +
    xlab("Herd")
  ggsave("graphics/fitness_calving-strength.png",
         width = 2000, height = 1500, units="px")


}
