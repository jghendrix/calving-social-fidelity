ln_fitness_model <- function(for_analyses) {

  data <- for_analyses %>% filter(!is.na(mean_ln_dist)) %>%
    group_by(Herd) %>%
    mutate(z.spat = scale(dist_to_last_yr),
           z.soc = scale(mean_ln_dist)) %>%
    ungroup()


  # tried including individual intercepts in addition to spatial - does not contribute anything useful

  #  mt <- lmer(z.soc ~ z.spat + (1|season_yr) + (1|id),
  #   data = data)
  # int <- coef(mt)$id
  # int$id <- row.names(int)
  # int %<>% dplyr::select(-c(z.spat)) %>%
  #   rename(intercept = '(Intercept)')
  # data <- left_join(data, int, by = c("id"))


  herd <- '(1|Herd)'
  year <- '(1|season_yr)'
  spat_herd <- '(z.spat|Herd)'
  soc_herd <- '(z.soc|Herd)'

  terms <- list(
    c(herd),
    c(year),
    c(herd, year),
    c('z.spat', herd, year),
#    c('z.spat', 'intercept', herd, year),
    c('z.soc', herd, year),
    c(spat_herd, year),
 #   c(spat_herd, 'intercept', year),
    c(soc_herd, year),
    c(herd, year, 'prop_forest'),
    c('z.spat', herd, year, 'prop_forest'),
    c('z.soc', herd, year, 'prop_forest'),
    c(spat_herd, year, 'prop_forest'),
    c(soc_herd, year, 'prop_forest')
  )

  model_ls <- lapply(terms, function(t) {
    lmer(
      formula = do.call(reformulate, list(t, response = 'lost')),
      data = data
    )
  }
  )

  names(model_ls) <- seq.int(length(model_ls))

  model_params <- lapply(seq_along(model_ls), function(i) {
    mod <- model_ls[[i]]
    nm <- names(model_ls)[[i]]

    DT  <- parameters::parameters(mod) |>
      insight::standardize_names()
    setDT(DT)

    DT[duplicated(DT, by = c('Parameter', 'Effects', 'Group')),
       Parameter :=  paste0(Parameter, '_1')]

    DT[, model_numb := nm]

    return(DT)
  }) |> rbindlist()
}
