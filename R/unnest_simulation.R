unnest_simulation <- function(e) {

  # tidyr::unnest can be extremely slow, which seems to be related to
  # https://github.com/r-lib/vctrs/issues/530
  s <- try(tidyr::unnest(e$simulation, cols = c("results")),
    silent = TRUE
  )
  if (class(s)[[1]] != "try-error") {
    e$simulation <- s
    return(e)
  }
  s <- e$simulation
  s$.row_nmb <- base::seq_len(nrow(s))

  r <- dplyr::select(s, "results", ".row_nmb")
  r <- try(tidyr::unnest(r, cols = c("results")), silent = TRUE)

  if (class(r)[[1]] == "try-error") {
    return(e)
  }

  s <- dplyr::select(s, -"results")
  s <- dplyr::inner_join(s, r, by = ".row_nmb")
  s <- dplyr::select(s, -".row_nmb")
  e$simulation <- s
  e
}
