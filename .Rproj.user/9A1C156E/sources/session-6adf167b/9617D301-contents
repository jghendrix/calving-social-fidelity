# === Social fidelity -----------------------------------------------------



# Source ------------------------------------------------------------------
targets::tar_source('R')



# Options -----------------------------------------------------------------
tar_option_set(format = 'fst_dt', workspace_on_error = TRUE)


# Variables ---------------------------------------------------------------
# Paths to data
path_distances <- file.path('input', 'pairwise_distances.csv')
path_mantel <- file.path('input', 'mantel.csv')
path_fitness <- file.path('input', 'fitness_by_ID.csv')

# Targets: input ----------------------------------------------------------

targets_input <- c(

  tar_target(
    self_others,
    fread(path_distances)
  ),

  tar_target(
    dist_between_id_by_yr,
    fread(path_mantel)
  ),
  tar_target(
    for_analyses,
    fread(path_fitness)
  )
  )


# Targets: grouping -------------------------------------------------------
targets_grouping <- c(

  tar_target(
    self_others_by_herd,
    self_others[, tar_group := .GRP, by = "Herd"],
    iteration = 'group'
  ),

  tar_target(
    dist_by_herd,
    dist_between_id_by_yr[, tar_group := .GRP, by = "Herd"],
    iteration = 'group'
  ),

  tar_target(
    fitness_by_herd,
    for_analyses[, tar_group := .GRP, by = "Herd"],
    iteration = 'group'
  ),

  tar_target(
    splits,
    unique(fitness_by_herd[, .SD, .SDcols = c('Herd')]),
    map(dist_by_herd)
  )
)


# Targets: models ---------------------------------------------------------
targets_models <- c(

  # Objective 1: evidence for social & spatial fidelity

  tar_target(
    model_part_dists,
    model_mantel_distances(dist_by_herd),
    map(dist_by_herd)
  ),

  tar_target(
    herd_rep,
    mantel_model(model_part_dists)
  ),

  tar_target(
    spatial_test,
    spatial_model(self_others_by_herd),
    map(self_others_by_herd)
  ),

  tar_target(
    spatial_herd,
    cbind(spatial_test, splits),
    map(spatial_test, splits)
  ),

  # Objective 2: tradeoffs between spatial-social fidelity

  tar_target(
    ln_tradeoffs_all,
    ln_tradeoff_model(for_analyses)
  ),


  tar_target(
    ln_tradeoff_separate,
    ln_tradeoff_model(fitness_by_herd),
    map(fitness_by_herd)
  ),

  tar_target(
    ln_tradeoff_by_herd,
    cbind(ln_tradeoff_separate, splits),
    map(ln_tradeoff_separate, splits)
  ),

  tar_target(
    indiv_tradeoffs,
    ln_indiv_tradeoff(for_analyses)
  ),

  # Objective 3: fidelity and calf survival

  tar_target(
    ln_fitness,
    ln_fitness_model(for_analyses)
  ),

 # tar_target(
#    ln_fitness_comp,
#    ln_fitness_AIC(for_analyses)
#  ),


  # and the non log-transformed analyses (for Appendix)
  tar_target(
    tradeoff_test_all,
    tradeoff_model(for_analyses)
  ),

  tar_target(
    tradeoff_test_separate,
    tradeoff_model(fitness_by_herd),
    map(fitness_by_herd)
  ),

  tar_target(
    tradeoff_by_herd,
    cbind(tradeoff_test_separate, splits),
    map(tradeoff_test_separate, splits)
  ),

  tar_target(
    fitness_comp,
    fitness_AIC(for_analyses)
  ),

  tar_target(
    fitness_params,
    fitness_coef(for_analyses)
  )
)



# Targets: plots ----------------------------------------------------------
targets_plots <- c(
  tar_target(
    mantel_by_herd,
    plot_mantel(model_part_dists),
    iteration = 'list',
    format = 'rds'
  ),

  tar_target(
    spatial_plot,
    spatial_plotting(self_others)
  ),

  tar_target(
    socio_spatial_plots,
    social_spatial_plotting(for_analyses, indiv_tradeoffs)
  ),

  tar_target(
    fitness_plots,
    fitness_model_plots(for_analyses)
  )
)



# Targets: all ------------------------------------------------------------
# Automatically grab all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)
