plot_mantel <- function(DT) {

  df <- as.data.frame(DT)

  df %<>%  mutate(n = choose(n_row, 2),
             Weight = n/sum(n),
             weighted_r = r*Weight) %>%
    filter(!is.na(r))

  ## Actual r values for each herd, point size = sample size, with weighted mean
  ggplot(df, aes(x = herd, y = r, size = Weight, colour = herd)) +
      geom_point(alpha = 0.6) +
      xlab(element_blank()) +
      ylab("Mantel r-statistic") +
      geom_point(data = (
        df %>% group_by(herd) %>%
          reframe(in_grp = n/sum(n), mean = sum(r*in_grp))),
        aes(herd, mean),
        shape = 95, size = 24) +
      # this ^ looks disgusting, but I want to insert the weighted average
      #  for each herd, so have to recalculate weight within
      #  group rather than for the entire population
     scale_colour_viridis(discrete = TRUE, option = "C", end = 0.9, guide = "none") +
      coord_cartesian(ylim = c(0, 1)) +
      theme_light() +
     theme(legend.position = "none")
   ggsave("graphics/mantel_by_herd.png",
          width = 2200, height = 1300, units="px")

  ## does r depend on the size of the sample?
  r_vs_n <- ggplot(df, aes(x = n, y = r, colour = herd)) +
    geom_point()
  # somewhat of a funnel, maybe? Highest values are for smallest herds

  summary(aov(weighted_r ~ herd, data = df))
  # no differences between herds (when only looking at Fogo and Buchans, at least)
}
