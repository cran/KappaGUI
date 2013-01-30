\name{Sel_procedure}
\alias{Sel_procedure}
\title{
Tcltk window, which allows to define the parameters of the analysis.
}
\description{
Internal function.
}
\usage{
Sel_procedure()
}
\value{
List of parameters specified by the user :

$Nb_raters : number of specified raters (must be a multiple of the number of columns of your dataset)

$Weighting : one of c("unweighted", "equal", "squared"), to be passed in the irr's function kappa2.
}
\author{
Frederic Santos
}

