\name{powerindex}
\alias{powerindex}
\title{
Obtain several measures of power
}
\description{
This general function allows the determination of several distributions of the power under different approaches in a weighted voting situation.
}
\usage{
powerindex(quota, weights, index = c("S", "B", "J", "CM", "JCM"), 
partition = NULL, quasiminimal = FALSE, minimal = FALSE, normalized = FALSE, 
swing = FALSE)
}
\arguments{
  \item{quota}{
Numerical value that represents the majority in a given voting.
}
  \item{weights}{
Numerical vector of dimension \eqn{n} that indicates the weights of \eqn{n} agents in a given voting.
}
  \item{index}{
Character that indicates the used approach. \code{S} and \code{B} denote the Shapley-Shubik index and the Banzhaf index, and the Owen index and the Banzhaf-Owen index if \code{partition} exist. \code{J} is used for obtaining the Jonhston index, \code{CM} determines the Colomer-Martinez index and \code{JCM} is used for obtaining the Jonhston-Colomer-Martinez index.
}
  \item{partition}{
Numerical vector that indicates the partition of voters. Each component indicates the element of the partition to which such voter belongs.
}
  \item{quasiminimal}{
Logical option to obtain the Quasi-Minimal Winning Coalitions.
}
  \item{minimal}{
Logical option to obtain the Minimal Winning Coalitions.
}
  \item{normalized}{
Logical option to obtain the normalized Banzhaf values.
}
  \item{swing}{
Logical option to obtain the number of swings of each voter.
}
}
\value{
See the values of the respective functions.
}
\references{
Alonso-Meijide, J. M., & Bowles, C. (2005). Generating functions for coalitional power indices: An application to the IMF. Annals of Operations Research, 137, 21-44.

Brams, S. J., & Affuso, P. J. (1976). Power and size: A new paradox. Theory and Decision, 7(1-2), 29-56.

Colomer, J. M., & Martinez, F. (1995). The paradox of coalition trading. Journal of Theoretical Politics, 7(1), 41-63.

Johnston, R. J. (1978). On the measurement of power: Some reactions to Laver. Environment and Planning A, 10(8), 907-914.

Lucas, W. F. (1983). Measuring power in weighted voting systems (pp. 183-238). Springer New York.
}
\author{
Livino M. Armijos-Toro, Jose M. Alonso-Meijide, Manuel A. Mosquera, Alejandro Saavedra-Nieves.
}
\examples{
weights<-c(137,85,71,32,9,8,5,2,1) 
quota<-176
powerindex(quota,weights,index="S")
powerindex(quota,weights,index="B",swing=TRUE)
powerindex(quota,weights,index="B",partition=c(1,1,2,2,3,3,4,4,4),swing=TRUE)
powerindex(quota,weights,index="J",quasiminimal=TRUE)
}
