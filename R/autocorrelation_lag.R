autocorrelation_lag=function(l,z,minstep=0.01*range(l)){
  ind=which(!is.na(l) & !is.na(z))
  l=l[ind]
  z=z[ind]
  o=order(l)
  l=l[o]
  z=z[o]
  step=max(min(diff(l)),minstep)
  ll=seq(min(l),max(l),step)
  zz=approx(l,z,ll)$y
  res_acf=acf(diff(zz), length(zz),plot=FALSE)
  n=length(zz)
  limsup=-1/n+3/sqrt(n)
  lag=step*which(res_acf$acf<limsup)[1]
  return(lag)
}