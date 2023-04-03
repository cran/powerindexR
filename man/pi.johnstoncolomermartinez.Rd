\name{pi.johnstoncolomermartinez}
\alias{pi.johnstoncolomermartinez}
\title{
Power based on the Jonhston-Colomer-Martinez index.
}
\description{
This function determines the distribution of the power based on the Jonhston-Colomer-Martinez index.
}
\usage{
pi.johnstoncolomermartinez(quota, weights)
}
\arguments{
\item{quota}{
Numerical value that represents the majority in a given voting.
}
  \item{weights}{
Numerical vector of dimension \eqn{n} that indicates the weights of \eqn{n} agents in a given voting.
}
}
\value{
\item{Jonhston-Colomer-Martinez}{The Jonhston-Colomer-Martinez index.}
}
\references{
Colomer, J. M., & Martinez, F. (1995). The paradox of coalition trading. Journal of Theoretical Politics, 7(1), 41-63.

Johnston, R. J. (1978). On the measurement of power: Some reactions to Laver. Environment and Planning A, 10(8), 907-914.
}
\author{
Livino M. Armijos-Toro, Alejandro Saavedra-Nieves.
}
\examples{
weights<-c(137,85,71,32,9,8,5,2,1) 
quota<-176
pi.johnstoncolomermartinez(176,weights)
}
