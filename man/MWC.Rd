\name{MWC}
\alias{MWC}
\title{
Obtain the minimal winning coalitions
}
\description{
This function determines the minimal winning coalitions in a weighted majority game.
}
\usage{
MWC(quota, weights)
}
\author{
Livino M. Armijos-Toro, Alejandro Saavedra-Nieves.
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
\item{Number of Minimal Winning Coalitions}{Total amount of Minimal Winning Coalitions.}
\item{Minimal Winning Coalitions}{Each row indicates a binary representation of each Minimal Winning Coalition.}
}
\examples{
weights<-c(137,85,71,32,9,8,5,2,1) 
quota<-176
MWC(quota,weights)
}