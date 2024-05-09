fitness_AIC <- function(for_analyses) {

  data <- for_analyses %>% filter(!is.na(mean_diff_dist)) %>%
    group_by(Herd) %>%
    mutate(z.spat = scale(dist_to_last_yr),
           z.soc = scale(mean_diff_dist)) %>%
    ungroup()


  herd <- '(1|Herd)'
  year <- '(1|season_yr)'
  spat_herd <- '(z.spat|Herd)'
  soc_herd <- '(z.soc|Herd)'


  terms <- list(
      c(herd),
      c(year),
      c(herd, year),
      c('z.spat', herd, year),
      c('z.soc', herd, year),
      c(spat_herd, year),
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
  })

  names(model_ls) <- seq.int(length(model_ls))



  model_aic_terms <- lapply(seq_along(model_ls), function(i) {
    mod <- model_ls[[i]]
    nm <- names(model_ls)[[i]]

    data.table(
      model_numb = nm,
      AIC = AIC(mod),
      terms = paste(terms[[i]], collapse = '+')
    )

    #setDT(DT)
    #return(DT)

  }) |> rbindlist()

  model_aic_terms %<>% mutate(deltaAIC = AIC - min(AIC))

  weights <- data.frame(akaike.weights(model_aic_terms$AIC))
  weights$model_numb <- names(model_ls)
  model_aic_terms <- left_join(model_aic_terms, weights,
                               by = c('model_numb', "deltaAIC"))

  # models 16 and 17 have identical delta AIC, have to remove those two rows
  model_aic_terms %<>% filter(!duplicated(model_numb))

  write_csv(model_aic_terms, 'output/non_ln_fitness_AIC.csv')
}
