\name{pi.johnston}
\alias{pi.johnston}
\title{
Power based on the Johnston index.
}
\description{
This function determines the distribution of the power based on the Johnston index.
}
\usage{
pi.johnston(quota, weights, quasiminimal = FALSE)
}
\arguments{
\item{quota}{
Numerical value that represents the majority in a given voting.
}
  \item{weights}{
Numerical vector of dimension \eqn{n} that indicates the weights of \eqn{n} agents in a given voting.
}
  \item{quasiminimal}{
Logical option to obtain the Quasi-Minimal Winning Coalitions.
}
}

\value{
  \item{Johnston}{The Jonhston index.}
  \item{Number of Quasi-Minimal Winning Coalitions}{Total amount of Quasi-Minimal Winning Coalitions.}
\item{Quasi-Minimal Winning Coalitions}{Each row indicates a binary representation of each Quasi-Minimal Winning Coalition.}
}
\references{
Johnston, R. J. (1978). On the measurement of power: Some reactions to Laver. Environment and Planning A, 10(8), 907-914.
}
\author{
Livino M. Armijos-Toro, Jose M. Alonso-Meijide, Manuel A. Mosquera, Alejandro Saavedra-Nieves.
}

\examples{
weights<-c(137,85,71,32,9,8,5,2,1) 
quota<-176
pi.johnston(176,weights,quasiminimal=TRUE)
}
