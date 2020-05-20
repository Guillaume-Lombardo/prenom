.onLoad <- function(libname, pkgname) {
  op <- options()
  op.prenom <- list(
    prenom_tmp_dir = fs::path(tempdir(), 'covid_temp_dir')
  )
  toset <- !(names(op.prenom) %in% names(op))
  if (any(toset)) options(op.prenom[toset])

  if (!dir.exists(getOption("prenom_tmp_dir"))) dir.create(getOption("prenom_tmp_dir"))
  assign(".prenom", new.env(), parent.env(environment()))

  invisible()
}
