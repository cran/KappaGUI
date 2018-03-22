\name{kappaFleiss}
\alias{kappaFleiss}

\title{
Calculates Fleiss' kappa between \eqn{k} raters for all \eqn{k}-uplets of columns in a given dataframe
}

\description{
This function is based on the function 'kappam.fleiss' from the package 'irr', and simply adds the possibility of calculating several kappas at once.
}

\usage{
kappaFleiss(data, nb_raters=3)
}

\arguments{
  \item{data}{dataframe with \eqn{k \times p}{k x p} columns, \eqn{k}{k} being the number of raters, and \eqn{p}{p} the number of traits. The first \eqn{k}{k} columns represent the scores attributed by the \eqn{k}{k} raters for the first trait; the next \eqn{k}{k} columns represent the scores attributed by the \eqn{k}{k} raters for the second trait; etc. The dataframe must contains a header, and each column must be labeled as follows: \sQuote{VariableName_X}, where X is a unique character (letter or number) associated with each rater (cf. below for an example).}
  \item{nb_raters}{integer for the number of raters.}
}

\value{
A dataframe with \eqn{p} rows (one per trait) and two columns, giving respectively the kappa value for each trait, and the number of individuals used to calculate this value.
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
irr::kappam.fleiss
}

\examples{
# Here we create and display an artifical dataset,
# describing two traits coded by three raters:
scores <- data.frame(
	Trait1_A = c(1,0,2,1,1,1,0,2,1,1),
	Trait1_B = c(1,2,0,1,2,1,0,1,2,1),
	Trait1_C = c(2,2,2,1,1,1,0,1,2,1),
	Trait2_A = c(1,4,5,2,3,5,1,2,3,4),
	Trait2_B = c(2,5,2,2,4,5,1,3,1,4),
	Trait2_C = c(2,4,3,2,4,5,2,2,3,4)
	)
scores

# Retrieve Fleiss' kappa for Trait1 and Trait2,
# to evaluate inter-rater agreement between raters A, B and C:
kappaFleiss(scores, nb_raters=3)
}

\keyword{ Kappa }
\keyword{ Fleiss }
