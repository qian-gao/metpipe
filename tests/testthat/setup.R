if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(path = ".", quiet = TRUE)
} else {
  r_files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
  for (f in r_files) {
    source(f, local = .GlobalEnv)
  }
}
