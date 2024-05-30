pi.johnston <- function (quota, weights, quasiminimal = FALSE){
  
  if (sum(weights) < quota) {
    stop("The quota is greater than the sum of the weights. Please, introduce valid parameters.")
  }
  if (round(quota, 5) == 0) {
    stop("The quota is zero. Please, introduce valid parameters.")
  }
  
  n <- length(weights)
  
  coal_QMW <- vector("numeric")
  c_isuperd <- matrix(0, nrow = n, ncol = n)
  
  jug<-1:n
  
  for (i in jug) {
    lsu <- vector("numeric", 1)
    lco <- vector("numeric", 1)
    
    rest_sums <- sum(weights[jug[-i]])
    l_inf <- quota - weights[i]
    for (j in jug[-i]) {
      sums <- lsu + weights[j]
      coalic <- lco + 2^(j-1)
      
      rest_sums<-rest_sums-weights[j]
      
      ind <- (sums < quota) & ((sums+rest_sums) >= l_inf)
      
      
      lsu <-c(lsu, sums[ind])
      lco <- c(lco,coalic[ind])
      
    }
    
    lsu <- lsu + weights[i]
    lco <- lco + 2^(i-1)
    
    ind <- lsu >= quota  & !(lco %in% coal_QMW)
    
    lco<-lco[ind]
    
    lsu<-lsu[ind]
    
    if(weights[i]>=quota){ 
      c_isuperd[i,1] <- c_isuperd[i,1] + sum(ind)
    } else{
      for(ii in seq_along(lco)){
        xx <- coalition(binary.digit(lco[ii]-2^(i-1), n), n)
        
        Splayers <- (1:n)[xx == 1]
        
        ind<-(lsu[ii]-weights[Splayers])<quota
        
        chi<-Splayers[ind]
        nchi<-length(chi)
        chi[nchi+1]<-i
        nchi<-nchi+1
        c_isuperd[chi,nchi] <- c_isuperd[chi,nchi] + 1
      }
    }
    coal_QMW <- c(coal_QMW,lco)
  }
  
  
  Q <- length(coal_QMW)
  
  
  J <- vector("numeric", n)
  for (i in 1:n) {
    J[i] <- 1/Q * sum(c_isuperd[i, ]/(1:n))
  }
  result <- list()
  names.result <- c()
  result[[1]] <- J
  names.result <- c(names.result, "Johnston")
  if (quasiminimal == TRUE) {
    result[[2]] <- Q
    names.result <- c(names.result, "Number of Quasi-Minimal Winning Coalitions")
    result[[3]] <- t(sapply(coal_QMW, function(t) coalition(binary.digit(t, n), n)))
    names.result <- c(names.result, "Quasi-Minimal Winning Coalitions")
  }
  names(result) <- names.result
  return(result)
}

