fitness_coef <- function(DT) {

  data <- DT %>% filter(!is.na(mean_diff_dist)) %>%
    group_by(Herd) %>%
    mutate(z.spat = as.numeric(scale(dist_to_last_yr)),
           z.soc = as.numeric(scale(mean_diff_dist)),
           z.ln = as.numeric(scale(mean_ln_dist))) %>%
    ungroup()


  herd <- '(1|Herd)'
  year <- '(1|season_yr)'
  spat_herd <- '(z.spat|Herd)'
  soc_herd <- '(z.soc|Herd)'
  ln_herd <- '(z.ln|Herd)'
  spat_soc <- paste('z.spat', 'z.soc', sep = "*")
  spat_ln <- paste('z.spat', 'z.ln', sep = "*")
  spat_soc_interact <- paste(spat_herd, soc_herd, sep = '*')
  spat_ln_interact <- paste(spat_herd, ln_herd, sep = "*")
  spat_soc_herd <- paste(spat_herd, soc_herd, sep = '*')

  terms <- list(
    c(herd),
    c(year),
    c(herd, year),
    c('z.spat', herd, year),
    c('z.soc', herd, year),
    c('z.spat', 'z.soc', herd, year),
    c(spat_soc_interact, herd, year),
    c(spat_herd, year),
    c(spat_herd, 'z.soc', year),
    c(soc_herd, year),
    c(soc_herd, 'z.spat', year),
    c(spat_herd, soc_herd, year),
    c(spat_soc_herd, year)
  )

  model_ls <- lapply(terms, function(t) {
    lmer(
      formula = do.call(reformulate, list(t, response = 'lost')),
      data = data
    )
  })

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
