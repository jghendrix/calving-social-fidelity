mantel_model <- function(data) {

  data %<>% mutate(n = choose(n_row, 2),
                   weight = n/sum(n),
                   weighted_r = r*weight) %>%
    filter(!is.na(r))

 # data$herd <- factor(data$herd, levels = c('BUCHANS', 'POTHILL', 'TOPSAILS', 'FOGO', 'LAPOILE', 'MIDRIDGE', 'GREY'))

  m <- lm(r ~ herd, weights = weight, data = data)

  params <- parameters::parameters(m)
  insight::standardize_names(params)
  # parameters:: and report:: don't support Tukey tests... fair,
  #  they're pretty basic. But I'd just like to know if the mantel
  #  r-stats are different between herds?

  # from the figure, it seems like Lapoile is lower than others and
  #  MR is higher, the rest are relatively similar
  # when Lapoile is ref: LP < everyone
  # Fogo ref: FO , < GR, < BC
  # MR as reference: LP < FO < GR < BC
  # TS ref: LP < TS, TS < BC
  # PH ref: LP < PH, otherwise no diffs with anyone else

  # LP    FO    MR    TS   PH    GR    BC
  # a     b     b     bc   bcd   cd    d

}
