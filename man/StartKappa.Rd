\name{StartKappa}
\alias{StartKappa}
\title{
GUI for Cohen's and Fleiss' Kappa. (Coded in tcltk)
}
\description{
This is the main function of the package. It offers a simple and quick GUI to work out Cohen's or Fleiss' Kappa, using the irr library for the calculus itself.
}
\usage{
StartKappa()
}
%- maybe also 'usage' for other objects documented here.
\details{
The GUI allows you to load your datafile and specifiy some details about its layout. Only CSV files with a semicolon separator are accepted.

If there are k raters for p variables observed on n individuals, the input file is a data frame with n rows and (k x p) columns. The first k columns represent the scores attributed by the k raters for the first variable ; the following k columns represent the scores attributed by the k raters for the second variable ; etc.

Weighting options for Cohen's Kappa are available.
}
\value{
A CSV file containing the Kappa values for each variable is saved in a folder which will be chosen by the user.
}
\references{
See the help of the irr package for more information about Cohen's and Fleiss' Kappa in R.
}
\author{
Frederic Santos
}

\seealso{
kappa2, kappam.fleiss
}

\keyword{ Kappa }
\keyword{ Fleiss }
\keyword{ Cohen }
