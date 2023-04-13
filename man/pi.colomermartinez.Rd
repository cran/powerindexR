\name{pi.colomermartinez}
\alias{pi.colomermartinez}
\title{
Power based on the Colomer-Martinez index.
}
\description{
This function determines the distribution of the power based on the Colomer-Martinez index.
}
\usage{
pi.colomermartinez(quota, weights, minimal = FALSE)
}
\arguments{
\item{quota}{
Numerical value that represents the majority in a given voting.
}
  \item{weights}{
Numerical vector of dimension \eqn{n} that indicates the weights of \eqn{n} agents in a given voting.
}
  \item{minimal}{
Logical option to obtain the Minimal Winning Coalitions.
}
}

\value{
\item{Colomer-Martinez}{The Colomer-Martinez index.}
\item{Number of Minimal Winning Coalitions}{Total amount of Minimal Winning Coalitions.}
\item{Minimal Winning Coalitions}{Each row indicates a binary representation of each Minimal Winning Coalition.}
}
\references{
Colomer, J. M., & Martinez, F. (1995). The paradox of coalition trading. Journal of Theoretical Politics, 7(1), 41-63.
}
\author{
Livino M. Armijos-Toro, Jose M. Alonso-Meijide, Manuel A. Mosquera, Alejandro Saavedra-Nieves.
}

\examples{
weights<-c(137,85,71,32,9,8,5,2,1) 
quota<-176
pi.colomermartinez(176,weights,minimal=TRUE)
}
