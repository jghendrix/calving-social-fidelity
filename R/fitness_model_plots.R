fitness_model_plots <- function(for_analyses) {

  data <- for_analyses %>% filter(!is.na(dist_to_last_yr)) %>%
    group_by(Herd) %>%
    mutate(z.spat = scale(dist_to_last_yr),
           z.soc = scale(mean_diff_dist),
           z.ln = scale(mean_ln_dist)) %>%
    ungroup() %>%
    mutate(Survival = ifelse(lost == "FALSE", 1, 0))


  ## Fitness by z.spat
  ggplot(data, aes(x = z.spat, y = Survival, facets = Herd)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(method = "lm") +
    #scale_colour_viridis(discrete = TRUE) +
    theme_bw() +
    ylab("Calf survival") +
    xlab("Std. distance to prior site") +
    facet_wrap(vars(Herd)) +
    theme(legend.position = "bottom")
  ggsave("graphics/fitness_z.spat.png",
         width = 4000, height = 3000, units="px")

  # Topsails seems like spatial fidelity matters, most other herds either
  #  no effect or a slight positive??


  ## Fitness by z.soc
  ggplot(data, aes(x = z.soc, y = Survival, facets = Herd)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(method = "lm") +
    #scale_colour_viridis(discrete = TRUE) +
    theme_bw() +
    ylab("Calf survival") +
    xlab("Std. change in neighbour distances") +
    facet_wrap(vars(Herd)) +
    theme(legend.position = "bottom")
  ggsave("graphics/fitness_z.soc.png",
         width = 4000, height = 3000, units="px")

  # Similar trends as for spatial fidelity - lower neighbour change is
  #  beneficial in Topsails and maybe grey, otherwise neutral or slightly
  #  negative?


  ## Fitness by z.ln
  ggplot(data, aes(x = z.ln, y = Survival, facets = Herd)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(method = "lm") +
    #scale_colour_viridis(discrete = TRUE) +
    theme_bw() +
    ylab("Calf survival") +
    xlab("Std. ln() change in neighbour distances") +
    facet_wrap(vars(Herd)) +
    theme(legend.position = "bottom")
  ggsave("graphics/fitness_z.ln.png",
         width = 4000, height = 3000, units="px")

  # Idk what this means honestly... some of the slopes changed a
  #  little bit but not that much tbh

  # Socio-spatial heat map of fitness?
  d1 <- data %>% mutate(spat_bin = cut(z.spat, breaks = 5),
                        soc_bin = cut(z.soc, breaks = 5))
  d1 %<>%
    group_by(spat_bin, soc_bin) %>%
    summarise(n = n(), surv = mean(Survival)) %>% ungroup()

  ggplot(d1, aes(x = spat_bin, y = soc_bin)) +
    geom_tile(aes(fill = surv)) +
    scale_fill_gradient(low = "lightpink", high = "navy") +
    theme_bw() +
    xlab("Std. distance to prior site") +
    ylab("Std. change in neighbour distances") +
   # facet_wrap(vars(Herd)) +
    theme(legend.position = "bottom")
  ggsave("graphics/fitness_heatmap.png",
         width = 4000, height = 3000, units="px")

  # No this is terrible, ignore this


  # Fitness in year prior against fidelity metrics?
  prev_data <- for_analyses %>%
    mutate(IDyr = paste0(id, "_", season_yr)) %>%
    group_by(id) %>%
    mutate(first = min(season_yr)) %>%
        filter(!is.na(dist_to_last_yr) |
             first == season_yr & is.na(dist_to_last_yr)) %>%
    filter(!duplicated(IDyr)) %>%
    arrange(season_yr) %>%
    mutate(prev_loss = lag(lost),
           prev_year = season_yr - lag(season_yr)) %>%
    mutate(prev_loss = ifelse(prev_year == 1, prev_loss, NA)) %>%
    ungroup() %>%
      group_by(Herd) %>%
    mutate(z.spat = as.numeric(scale(dist_to_last_yr)),
           z.soc = as.numeric(scale(mean_diff_dist)),
           z.ln = as.numeric(scale(mean_ln_dist))) %>%
    ungroup() %>%
    filter(!is.na(prev_loss)) %>%
    mutate(Previous_survival = ifelse(prev_loss == "FALSE", 1, 0))

  ## Prior fitness by z.spat
  ggplot(prev_data, aes(x = z.spat, y = Previous_survival, facets = Herd)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(method = "lm", colour = "darkgreen") +
    #scale_colour_viridis(discrete = TRUE) +
    theme_bw() +
    ylab("Prior year calf survival") +
    xlab("Std. distance to prior site") +
    facet_wrap(vars(Herd)) +
    theme(legend.position = "bottom")
  ggsave("graphics/prior_fitness_z.spat.png",
         width = 4000, height = 3000, units="px")

  # Topsails seems like spatial fidelity matters, most other herds
  #  either no effect or a slight positive??


  ## Fitness by z.soc
  ggplot(prev_data, aes(x = z.soc, y = Previous_survival, facets = Herd)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(method = "lm", colour = "darkgreen") +
    #scale_colour_viridis(discrete = TRUE) +
    theme_bw() +
    ylab("Prior year calf survival") +
    xlab("Std. change in neighbour distances") +
    facet_wrap(vars(Herd)) +
    theme(legend.position = "bottom")
  ggsave("graphics/prior_fitness_z.soc.png",
         width = 4000, height = 3000, units="px")

  # Similar trends as for spatial fidelity - lower neighbour change
  #  is beneficial in Topsails and maybe grey, otherwise neutral
  #  or slightly negative?


  ## Fitness by z.ln
  ggplot(prev_data, aes(x = z.ln, y = Previous_survival, facets = Herd)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(method = "lm", colour = "darkgreen") +
    #scale_colour_viridis(discrete = TRUE) +
    theme_bw() +
    ylab("Prior year calf survival") +
    xlab("Std. ln() change in neighbour distances") +
    facet_wrap(vars(Herd)) +
    theme(legend.position = "bottom")
  ggsave("graphics/prior_fitness_z.ln.png",
         width = 4000, height = 3000, units="px")


}
