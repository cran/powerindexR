\name{pi.shapley}
\alias{pi.shapley}
\title{
Power based on the Shapley-Shubik index.
}
\description{
This function determines the distribution of the power based on the Shapley-Shubik index and the Owen value.
}
\usage{
pi.shapley(quota, weights, partition = NULL)
}
\arguments{
  \item{quota}{
Numerical value that represents the majority in a given voting.
}
  \item{weights}{
Numerical vector of dimension \eqn{n} that indicates the weights of \eqn{n} agents in a given voting.
}
  \item{partition}{
Numerical vector that indicates the partition of voters. Each component indicates the element of the partition to which such voter belongs. If it is not \code{NULL}, it provides the distribution of the power based on the Owen value.
}
}

\value{
\item{Shapley value}{The Shapley value, if \code{partition=NULL}.}
\item{Owen value}{The Owen value, if \code{partition!=NULL}.}
}
\references{
Alonso-Meijide, J. M., & Bowles, C. (2005). Generating functions for coalitional power indices: An application to the IMF. Annals of Operations Research, 137, 21-44.

Lucas, W. F. (1983). Measuring power in weighted voting systems (pp. 183-238). Springer New York.}
\author{
Livino M. Armijos-Toro, Jose M. Alonso-Meijide, Manuel A. Mosquera, Alejandro Saavedra-Nieves.
}
\examples{
# Example Shapley value
weights<-c(137,85,71,32,9,8,5,2,1) 
quota<-176
pi.shapley(quota,weights)

# Example Owen value
quota<-30
weights<-c(28, 16, 5, 4, 3, 3)
# Partition={{1},{2,4,6},{3,5}}
pi.shapley(quota,weights,partition=c(1,2,3,2,3,2))
}
