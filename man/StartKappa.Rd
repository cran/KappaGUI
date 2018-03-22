\name{StartKappa}
\alias{StartKappa}

\title{
A graphical user interface for calculating Cohen's and Fleiss' Kappa
}

\description{
Launches the R-Shiny application. The user can retrieve inter-rater agreement scores from a file (.CSV or .TXT) loaded directly through the graphical interface.
}

\usage{
StartKappa()
}

\details{
Data importation is done directly through the graphical user interface. Only CSV and TXT files are accepted.

If there are \eqn{p}{p} variables observed by \eqn{k}{k} raters on \eqn{n}{n} individuals, the input file should be a data frame with \eqn{n}{n} rows and (\eqn{k \times p}{k x p}) columns. The first \eqn{k}{k} columns represent the scores attributed by the \eqn{k}{k} raters for the first variable; the next \eqn{k}{k} columns represent the scores attributed by the \eqn{k}{k} raters for the second variable; etc. Cohen's or Fleiss' kappas are returned for each variable. 

The data file must contains a header, and the columns must be labeled as follows: \sQuote{VariableName_X}, where X is a unique character (letter or number) associated with each rater. An example of correct data file with two raters is given here: \url{http://www.pacea.u-bordeaux.fr/IMG/csv/data_Kappa_Cohen.csv}.

Kappa values are calculated using the functions kappa2 and kappam.fleiss from the package \sQuote{irr}. Please check their help pages for more technical details, in particular about the weighting options for Cohen's kappa. For ordered factors, linear or quadratic weighting could be a good choice, as they give more importance to strong disgreements. If linear or quadratic weighting are chosen, the levels of the factors will be supposed to be ordered alphabetically (as a consequence, a factor with three levels "Low", "Medium" and "High" would be ordered in an inconvenient way: in this case, please recode the levels with names matching the natural order of the levels).
}

\value{
The function returns no value, but the table of results can be downloaded as a CSV file through the user interface.
}

\references{
Cohen, J. (1960) A coefficient of agreement for nominal scales. \emph{Educational and Psychological Measurement}, \bold{20}, 37--46.

Cohen, J. (1968) Weighted kappa: Nominal scale agreement with provision for scaled disagreement or partial credit. \emph{Psychological Bulletin}, \bold{70}, 213--220.
}

\author{
Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}
}

\seealso{
irr::kappa2, irr::kappam.fleiss
}

\keyword{ Kappa }
\keyword{ Fleiss }
\keyword{ Cohen }
