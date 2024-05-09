make_symmetrical <- function(DT,
                             left_id = 'ID1',
                             right_id = 'ID2',
                             measure = 'distance') {
  x <- DT[, .SD, .SDcols = c(left_id, right_id, measure)]
  y <- DT[, .SD, .SDcols = c(right_id, left_id, measure)]
  bind <- data.table::rbindlist(
    list(x, y),
    use.names = FALSE
  )
  m <- data.table::dcast(
    bind,
    reformulate(right_id, response = left_id),
    value.var = measure,
    fill = 0,
    fun.aggregate = unique
  )
  rownames(m) <- m[[1]]
  m[, 1 := NULL]
  return(m)
}


