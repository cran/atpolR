% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/atpol_grids.R
\name{atpol_div}
\alias{atpol_div}
\title{atpol_div creates ATPOL grid divided by 2, 4 or 5 (based on divider parameter) and returns it as sf object. Useful for grids like 5 x 5 km (divider = 2), 250 x 250 m (divider = 4) or 20 x 20 m (divider = 5). For details see \insertCite{vereyStandaryzacjaZapisuPodzialow2018;textual}{atpolR}}
\usage{
atpol_div(grid, divider)
}
\arguments{
\item{grid}{any valid ATPOL grid like "BE" or "DC5128"}

\item{divider}{divide by parameter: 2, 4, 5}
}
\value{
Simple Feature (sf) grid of polygons for ATPOL grid divided by 2, 4 or 5
}
\description{
atpol_div creates ATPOL grid divided by 2, 4 or 5 (based on divider parameter) and returns it as sf object. Useful for grids like 5 x 5 km (divider = 2), 250 x 250 m (divider = 4) or 20 x 20 m (divider = 5). For details see \insertCite{vereyStandaryzacjaZapisuPodzialow2018;textual}{atpolR}
}
\examples{
\donttest{
atpol_div("BE", 2)
atpol_div(grid = c("BE23", "DC5128"), divider = 4)
}
}
\references{
\insertAllCited{}
}
