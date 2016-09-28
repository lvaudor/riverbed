#' A function to calculate area between two profiles
#' 
#' This function takes two profiles defined as (l1,z1) and (l2,z2) as inputs and calculates area between them
#' @param l1 x coordinate of first profile
#' @param z1 z coordinate of first profile
#' @param l2 x coordinate of second profile
#' @param z2 z coordinate of second profile
#' @param h if provided by user, the second profile is supposed to be horizontal, with constant height=h (defaults to NA) 
#' @param precision precision with which the area is calculated (see Details section)
#' @param plot if TRUE the two profiles are plotted. Defaults to FALSE.
#' @param col color with which the area between profiles is plotted (if plot=TRUE). Defaults to "yellow"
#' @param plotstyle style of plot for both profiles. Can be set to "p" (points), "l" (only lines) or "b" (both). Defaults to "b".
#' @param upz individual standard imprecision on z 
#' @return uncertainty in the estimate of area
#' @export
#' @examples
#' l1=c(1,3,5,6,9)
#' z1=c(1,2,3,2.5,5)
#' l2=c(0.5,2.5,4,6,8)
#' z2=c(3,1,2,4,3)
#' area_between(l1,z1,l2,z2, plot=TRUE)
#' area_between(l1,z1,h=2, plot=TRUE, type="upper",plotstyle="l")
area_between=function(l1,
                      z1,
                      l2,
                      z2,
                      h=NA,
                      type="both",
                      plot=FALSE,
                      col=c("pink","light blue"),
                      plotstyle="b",
                      upz=NA,...){
  if(!is.na(h)){
    l2=c(min(l1,na.omit=T),max(l1,na.omit=T))
    z2=rep(h,2)
  }
  ## interpolate values of z1 for all abscissae l2
  z1n=approx(l1,z1,l2)$y
  dat1=data.frame(l1=c(l1,l2),
                  z1=c(z1,z1n),
                  p1=c(rep("observed",length(l1)),
                       rep("interpolated",length(l2)))
                  )
  dat1=dat1[order(dat1$l1),]
  ## interpolate values of z2 for all abscissae l1
  z2n=approx(l2,z2,l1)$y
  dat2=data.frame(l2=c(l2,l1),
                  z2=c(z2,z2n),
                  p2=c(rep("observed",length(l2)),
                       rep("interpolated",length(l1)))
                  )
  dat2=dat2[order(dat2$l2),]
  # get coordinates of all intersects between profiles
  ## get all locations in data where intersects occur
  gap=dat1$z1-dat2$z2
  gap=gap[1:(length(gap)-1)]*gap[2:(length(gap))]
  intersects=which(gap<=0)
  ## calculate the two line equations
  a=diff(dat1$z1)/diff(dat1$l1) #(y=ax+b)
  c=diff(dat2$z2)/diff(dat2$l2) #(y=cx+d)
  b=dat1$z1[1:(nrow(dat1)-1)]-a*dat1$l1[1:(nrow(dat1)-1)]
  d=dat2$z2[1:(nrow(dat2)-1)]-c*dat2$l2[1:(nrow(dat2)-1)]
  x=-(b-d)/(a-c)
  y=a*x+b
  ## add x and y coordinates of intersects to dataframe
  dat1=rbind(dat1,
             data.frame(l1=x[intersects],
                        z1=y[intersects],
                        p1=rep("intersect",
                               length(intersects))))
  dat1=dat1[order(dat1$l1),]
  dat2=rbind(dat2,
             data.frame(l2=x[intersects],
                        z2=y[intersects],
                        p2=rep("intersect",
                               length(intersects))))  
  dat2=dat2[order(dat2$l2),]
  # calculate area 
  ## calculate area of all successive trapezia 
  h=diff(dat1$l1)
  L=dat1$z1-dat2$z2
  La=L[1:(length(L)-1)]
  Lb=L[2:length(L)]
  a=h*(La+Lb)/2
  dat=data.frame(dat1,dat2,a=c(a,NA))
  ## calculate upper,lower and total area
  area_upper=sum(a[which(a>0)], na.rm=TRUE)
  area_lower=sum(a[which(a<0)], na.rm=TRUE)
  total_area=sum(a,na.rm=TRUE)
  #
  if(plot==TRUE){
        l=dat$l1
        z=dat$z1
        plot(x=l,y=z, col="white", 
             ylim=c(min(c(dat$z1,dat$z2),na.rm=T),max(c(dat$z1,dat$z2),na.rm=T)),...)
        points(dat$l2,dat$z2, col="white")
        datc=dat[which(!is.na(dat$z1)&!is.na(dat$z2)),]
        ind=1:nrow(datc)
        if(type=="lower"){ind=which(datc$a<0)}
        if(type=="upper"){ind=which(datc$a>0)}
        for (i in 1:length(ind)){
          mya=datc$a[i]
          if(length(col)==1){col=rep(col,2)}
          if(mya>0|is.na(mya)){mycol=col[1]}else{mycol=col[2]}
          polygon(c(datc$l1[ind[i]],
                    datc$l1[ind[i]+1],
                    datc$l2[ind[i]+1],
                    datc$l2[ind[i]]),
                  c(datc$z1[ind[i]],
                    datc$z1[ind[i]+1],
                    datc$z2[ind[i]+1],
                    datc$z2[ind[i]]),
                  col=mycol,border=mycol)
        }
        points(dat$l1[dat$p1=="observed"],dat$z1[dat$p1=="observed"], type=plotstyle, pch=20)
        points(dat$l2[dat$p2=="observed"],dat$z2[dat$p2=="observed"], type=plotstyle, pch=20,lty=2)
  }
  # calculation of uncertainty 
  uncertainty=upz*sqrt(sum(diff(sort(l1))^2)+sum(diff(sort(l2))^2))
  return(list(area=total_area,
              area_upper=area_upper,
              area_lower=area_lower,
              uncertainty=uncertainty,
              data=dat))
}