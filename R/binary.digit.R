binary.digit <- function(fnum,n) {
  bin_vect <-rep(0, 1+n) # Para que lo cree tan largo como sea necesario: array(0, dim=1 + floor(log(fnum, 2)))
  while (fnum >= 2) {
    pow <- floor(log(fnum, 2))
    bin_vect[1 + pow] <- 1
    fnum <- fnum - 2^pow
  } 
  bin_vect[1] <- fnum
  return(bin_vect)
} 
