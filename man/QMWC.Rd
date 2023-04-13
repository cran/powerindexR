\name{QMWC}
\alias{QMWC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain the quasi-minimal winning coalitions
}
\description{
This function determines the quasi-minimal winning coalitions in a weighted majority game.
}
\usage{
QMWC(quota, weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{quota}{
Numerical value that represents the majority in a given voting.
}
  \item{weights}{
Numerical vector of dimension \eqn{n} that indicates the weights of \eqn{n} agents in a given voting.
}
}
\value{
\item{Number of Quasi-Minimal Winning Coalitions}{Total amount of Quasi-Minimal Winning Coalitions.}
\item{Quasi-Minimal Winning Coalitions}{Each row indicates a binary representation of each Quasi-Minimal Winning Coalition.}
}
\author{
Livino M. Armijos-Toro, Jose M. Alonso-Meijide, Manuel A. Mosquera, Alejandro Saavedra-Nieves.
}
\examples{
weights<-c(137,85,71,32,9,8,5,2,1) 
quota<-176
QMWC(quota,weights)
}
