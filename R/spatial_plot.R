spatial_plotting <- function(self_others) {

  self_others %<>% filter(!duplicated(dyadID, season_yr))

  ggplot(self_others, aes(x = distance/1000, fill = comparison, facets = Herd)) +
    geom_density(alpha = 0.5) +
    scale_fill_viridis(discrete = TRUE, end = 0.6) +
    theme_bw() +
    xlab("Distance between calving sites (km)") +
    ylab("Relative density") +
    facet_wrap(vars(Herd)) +
    theme(legend.position = "bottom")
  ggsave("graphics/spatial-fidelity.png",
         width = 4000, height = 3000, units="px")

  # subsetting for ease of visibility

  # without Fogo
  ggplot(subset(self_others, Herd != "Fogo"), aes(x = distance/1000, fill = comparison, facets = Herd)) +
    geom_density(alpha = 0.5) +
    scale_fill_viridis(discrete = TRUE, end = 0.6) +
    theme_bw() +
    xlab("Distance between calving sites (km)") +
    ylab("Relative density") +
    facet_wrap(vars(Herd)) +
    #annotate("text", x = 25, y = 0.125, label = "A", size = 10) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  ggsave("graphics/spatial-fidelity_noFogo.png",
         width = 2100, height = 1500, units="px")

  # only Fogo
  ggplot(subset(self_others, Herd == "Fogo"), aes(x = distance/1000, fill = comparison)) +
    geom_density(alpha = 0.5) +
    scale_fill_viridis(discrete = TRUE, end = 0.6) +
    theme_bw() +
    xlab("Distance between calving sites (km) - Fogo") +
    ylab("Relative density") +
    annotate("text", x = 25, y = 0.125, label = "B", size = 10) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  ggsave("graphics/spatial-fidelity_onlyFogo.png",
         width = 2100, height = 1350, units="px")

}
