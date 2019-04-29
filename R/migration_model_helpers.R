epsilon <- function(){
  return(stats::runif(1,0.05,0.2))
}

small.epsilon <- function(){
  return(0.001)
} 

triangle_quantile <- function(r,t.mode,t.min,t.max){
  ymax = 2/(t.max-t.min)
  s <- as.numeric(r > (t.mode-t.min)*ymax/2)
  x.upper <- t.max - sqrt(
    t.max^2 - (t.max*t.min-t.min*t.mode+t.max*t.mode+2*r*t.max/ymax-2*r*t.mode/ymax)
  )
  x.lower <- t.min + sqrt(
    t.min^2 + (2*r*(t.mode-t.min)/ymax - t.min^2)
  )
  return((1-s)*x.lower+s*x.upper)
}

rtriang <- function(t.mode,t.min,t.max){
  if((t.min > t.max) || (t.min > t.mode) || (t.mode > t.max)){
    stop("Triangle distribution parameters incorrect ordering")
  }
  if((t.min == t.mode) && (t.mode == t.max)){
    return(t.mode)
  } else {
    r <- stats::runif(1)
    return(triangle_quantile(r,t.mode,t.min,t.max))
  }
}

prob_generator_int <- function(prob.vector){
  # Renormalize
  prob.vector <- prob.vector/sum(prob.vector)
  cum.prob <- cumsum(prob.vector)
  r <- stats::runif(1)
  w <- which(cum.prob > r)[1]
  return(w)
}
prob_generator <- function(input.table,mig.source){
  # Renormalize
  w <- prob_generator_int(input.table[,mig.source])
  return(
    list(
      key = input.table[w,1],
      value = w
    )
  )
}
security_risk_prob_generator <- function(family.status.value,mig.source,security.risk.probs){
  p <- security.risk.probs[family.status.value,mig.source]
  r <- stats::runif(1)
  if(r <= p){
    return(1) # Security Risk
  } else {
    return(2)
  }
}


src_generator <- function(
  migrant.sources,
  source.rates,
  nationality.probs,
  nat=NULL,
  t=0
){
    if(is.null(nat)){
      s <- source.rates[order(source.rates$time),]
      w <- NULL
      for(ss in migrant.sources[,1]){
        w <- c(w,max(which(s$source==ss & s$time <= t)))
      }
      p <- s$rate[w]/sum(s$rate[w])
      src.int <- prob_generator_int(p)
    } else if(is.character(nat)){
      w.nat <- which(nationality.probs$nationality==nat)
      s <- source.rates[order(source.rates$time),]
      w <- NULL
      for(ss in migrant.sources[,1]){
        w <- c(w,max(which(s$source==ss & s$time <= t)))
      }
      p <- s$rate[w]/sum(s$rate[w])
      p.nat <- (nationality.probs[w.nat,migrant.sources[,1]]*p)/sum(nationality.probs[w.nat,migrant.sources[,1]]*p)
      src.int <- prob_generator_int(as.numeric(p.nat))
    } else {
      w.nat <- nat
      s <- source.rates[order(source.rates$time),]
      w <- NULL
      for(ss in migrant.sources[,1]){
        w <- c(w,max(which(s$source==ss & s$time <= t)))
      }
      p <- s$rate[w]/sum(s$rate[w])
      p.nat <- (nationality.probs[w.nat,migrant.sources[,1]]*p)/sum(nationality.probs[w.nat,migrant.sources[,1]]*p)
      src.int <- prob_generator_int(as.numeric(p.nat))
    }
  return(src.int)
}

