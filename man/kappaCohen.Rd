\name{kappaCohen}
\alias{kappaCohen}

\title{
Calculates Cohen's kappa for all pairs of columns in a given dataframe
}

\description{
This function is based on the function 'kappa2' from the package 'irr', and simply adds the possibility of calculating several kappas at once.
}

\usage{
kappaCohen(data, weight="unweighted")
}

\arguments{
  \item{data}{dataframe with \eqn{2 \times p}{2 x p} columns, \eqn{p}{p} being the number of traits coded by the two raters. The first two columns represent the scores attributed by the two raters for the first trait; the next two columns represent the scores attributed by the two raters for the second trait; etc. The dataframe must contains a header, and each column must be labeled as follows: \sQuote{VariableName_X}, where X is a unique character (letter or number) associated with each rater (cf. below for an example).}
  \item{weight}{character string specifying the weighting scheme ("unweighted", "equal" or "squared"). See the function \sQuote{kappa2} from the package \sQuote{irr}.}
}

\value{
A dataframe with \eqn{p} rows (one per trait) and three columns, giving respectively the kappa value for each trait, the number of individuals used to calculate this value, and the associated \eqn{p}-value.
}

\details{
For each trait, only complete cases are used for the calculation.
}

\references{
Cohen, J. (1960) A coefficient of agreement for nominal scales. \emph{Educational and Psychological Measurement}, \bold{20}, 37--46.

Cohen, J. (1968) Weighted kappa: Nominal scale agreement with provision for scaled disagreement or partial credit. \emph{Psychological Bulletin}, \bold{70}, 213--220.
}

\author{
Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}
}

\seealso{
irr::kappa2
}

\examples{
# Here we create and display an artifical dataset,
# describing two traits coded by two raters:
scores <- data.frame(
	Trait1_A = c(1,0,2,1,1,1,0,2,1,1),
	Trait1_B = c(1,2,0,1,2,1,0,1,2,1),
	Trait2_A = c(1,4,5,2,3,5,1,2,3,4),
	Trait2_B = c(2,5,2,2,4,5,1,3,1,4)
	)
scores

# Retrieve Cohen's kappa for Trait1 and Trait2,
# to evaluate inter-rater agreement between raters A and B:
kappaCohen(scores, weight="unweighted")
kappaCohen(scores, weight="squared")
}


\keyword{ Kappa }
\keyword{ Cohen }
