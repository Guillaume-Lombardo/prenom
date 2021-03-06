% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_helpers.R
\name{helper_functions}
\alias{helper_functions}
\alias{proportionneur}
\alias{selecteur}
\alias{table_mef}
\alias{graph_annee}
\alias{graph_departement}
\alias{parseur}
\title{fonctions d'aide}
\usage{
proportionneur(dt = prenom_insee, sexe = "F", annee = NULL, departement = NULL)

selecteur(dt = prenom_insee, sexe = "F", annee = NULL, departement = NULL)

table_mef(
  dt = prenom_insee,
  sexe = "F",
  annee = NULL,
  departement = NULL,
  variables = c("annee", "dpt")
)

graph_annee(
  dt = prenom_insee,
  candidat = "GINETTE",
  sexe = "F",
  departement = NULL
)

graph_departement(
  dt = prenom_insee,
  relatif = TRUE,
  candidat = "GINETTE",
  annee = NULL,
  sexe = "F",
  dpt = NULL,
  map = departement_simplifie
)

parseur(texte, sep = ",")
}
\arguments{
\item{dt}{data.frame of prenom.}

\item{sexe}{'F', 'M' or c('F', 'M').}

\item{annee}{either null or a vector of year.}

\item{departement, dpt}{either null or a vector of departement.}

\item{variables}{a character(0), 'annee', 'dpt', or c('annee', 'dpt').}

\item{candidat}{a vector of names.}

\item{relatif}{TRUE for relative, FALSE for absolute.}

\item{map}{a sf object of departement.}

\item{texte}{a character string.}

\item{sep}{a separator pattern.}
}
\description{
fonctions d'aide
}
