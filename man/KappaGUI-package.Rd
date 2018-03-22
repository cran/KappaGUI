\name{KappaGUI-package}
\alias{KappaGUI-package}
\alias{KappaGUI}
\docType{package}
\title{
An R-Shiny application for calculating Cohen's and Fleiss' Kappa
}
\description{
Offers a graphical user interface for the evaluation of inter-rater agreement with Cohen's and Fleiss' Kappa. The calculation of kappa statistics is done using the R package 'irr', so that 'KappaGUI' is essentially a Shiny front-end for 'irr'.
}
\details{
\tabular{ll}{
Package: \tab KappaGUI\cr
Type: \tab Package\cr
Version: \tab 2.0.2\cr
Date: \tab 2018-03-22\cr
License: \tab GPL >=2\cr
}

}
\author{
Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}
}
\references{
Cohen, J. (1960) A coefficient of agreement for nominal scales. \emph{Educational and Psychological Measurement}, \bold{20}, 37--46.

Cohen, J. (1968) Weighted kappa: Nominal scale agreement with provision for scaled disagreement or partial credit. \emph{Psychological Bulletin}, \bold{70}, 213--220.
}
\keyword{Kappa}
\seealso{
irr::kappa2
}
\examples{
\dontrun{ StartKappa() }
}
