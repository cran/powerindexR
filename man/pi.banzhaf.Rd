\name{pi.banzhaf}
\alias{pi.banzhaf}
\title{
Power based on the Banzhaf index.
}
\description{
This function determines the distribution of the power based on the Banzhaf index and the Banzaf-Owen value.
}
\usage{
pi.banzhaf(quota, weights, partition = NULL, normalized = FALSE, swing = FALSE)
}
\arguments{
  \item{quota}{
Numerical value that represents the majority in a given voting.
}
  \item{weights}{
Numerical vector of dimension \eqn{n} that indicates the weights of \eqn{n} agents in a given voting.
}
  \item{partition}{
Numerical vector that indicates the partition of voters. Each component indicates the element of the partition to which such voter belongs. If it is not \code{NULL}, it provides the distribution of the power based on the Banzhaf-Owen value.
}
 \item{normalized}{
Logical option to obtain the normalized Banzhaf values.
}
  \item{swing}{
Logical option to obtain the number of swings of each voter.
}
}
\value{
\item{Banzhaf value}{The Banzhaf value, if \code{partition=NULL}.}
\item{Banzhaf-Owen value}{The Banzhaf-Owen value, if \code{partition!=NULL}.}
}
\references{
Alonso-Meijide, J. M., & Bowles, C. (2005). Generating functions for coalitional power indices: An application to the IMF. Annals of Operations Research, 137, 21-44.

Brams, S. J., & Affuso, P. J. (1976). Power and size: A new paradox. Theory and Decision, 7(1-2), 29-56.
}
\author{
Livino M. Armijos-Toro, Jose M. Alonso-Meijide, Manuel A. Mosquera, Alejandro Saavedra-Nieves.
}
\examples{
# Example Banzhaf value
weights<-c(137,85,71,32,9,8,5,2,1) 
quota<-176
pi.banzhaf(quota,weights)
pi.banzhaf(quota,weights,normalized=TRUE)

# Example Banzhaf-Owen value
quota<-30
weights<-c(28, 16, 5, 4, 3, 3)
# Partition={{1},{2,4,6},{3,5}}
pi.banzhaf(quota,weights,partition=c(1,2,3,2,3,2))
}
