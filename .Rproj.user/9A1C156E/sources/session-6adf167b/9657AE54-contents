model_mantel_distances <- function(DT) {
  Herd <- unique(DT$Herd)
  id_by_year <- DT[, .(id = unique(c(ID1, ID2))), by = season_yr]
  ls_years <- unique(DT$season_yr)
  drop_first <- data.table::last(ls_years, length(ls_years) - 1)

  l_mantel <- lapply(drop_first, function(y) {
    compare_y <- y - 1L

    ids <- intersect(
      id_by_year[season_yr == y, id],
      id_by_year[season_yr == compare_y, id]
    )

    one <- DT[ID1 %in% ids & ID2 %in% ids & season_yr == y]
    two <- DT[ID1 %in% ids & ID2 %in% ids & season_yr == compare_y]

    one_sym <- make_symmetrical(one)
    two_sym <- make_symmetrical(two)

    m <- vegan::mantel(
      one_sym,
      two_sym,
      method = "spearman",
      permutations = 999
    )

    m$n_row <- dim(one_sym)[1]
    m$n_col <- dim(one_sym)[2]

    m
  })

  out <- lapply(seq_along(l_mantel), function(i) {
    m <- l_mantel[[i]]
    data.frame(
      r = m$statistic,
      signif = m$signif,
      n_permutations = m$permutations,
      n_row = m$n_row,
      n_col = m$n_col,
      herd = Herd,
      year = drop_first[[i]],
      compare_year = drop_first[[i]] - 1
    )
  })

  return(data.table::rbindlist(out))
}
