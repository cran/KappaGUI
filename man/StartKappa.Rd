\name{StartKappa}
\alias{StartKappa}
\title{
Shiny user interface for Cohen's and Fleiss' Kappa.
}
\description{
This is the main function of the package. It offers a graphical user interface to work out Cohen's or Fleiss' Kappa, using the irr library for the calculus itself.
}
\usage{
StartKappa()
}
\details{
Data importation is done directly through the graphical user interface. Only CSV and TXT files are accepted.

If there are p variables observed by k raters observed on n individuals, the input file should be a data frame with n rows and (k x p) columns. The first k columns represent the scores attributed by the k raters for the first variable ; the next k columns represent the scores attributed by the k raters for the second variable ; etc. Cohen's or Fleiss' kappas are returned for each variable.

Kappa values are calculated using the functions kappa2 and kappam.fleiss from the package 'irr'. Please check their help pages for more technical details, in particular about the weighting options for Cohen's kappa proposed here. For ordered factors, linear or quadratic weighting could be a good choice, as they give more importance to strong disgreements. If linear or quadratic weighting are chosen, the levels of the factors will be supposed to be ordered alphabetically (as a consequence, a factor with three levels "Low", "Medium" and "High" would be ordered in an inconvenient way: in this case, please recode the levels with names matching the natural order of the levels).
}
\value{
The function returns no value, but the table of results can be downloaded as a CSV file through the user interface.
}
\references{
Cohen, J. (1968). Weighted kappa: Nominal scale agreement with provision for scaled disagreement or partial credit. Psychological Bulletin, 70, 213-220.

Fleiss, J.L. (1971). Measuring nominal scale agreement among many raters. Psychological Bulletin, 76, 378-382.
}
\author{
Frederic Santos, <frederic.santos@u-bordeaux.fr>
}

\seealso{
kappa2, kappam.fleiss
}

\keyword{ Kappa }
\keyword{ Fleiss }
\keyword{ Cohen }
