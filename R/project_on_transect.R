#' A function to project data on a bestfitting transect line
#'
#' @param x x coordinate of points
#' @param y y coordinate of points
#' @return xp x coordinate of points projected on the transect
#' @return yp y coordinate of points projected on the transect
#' @return xt x coordinate along the transect (new coordinate x)
#' @return yt y coordinate corresponding to the distance to the transect (new coordinate y)
#' @return slopexy slope of transect
#' @return interceptxy intercept of transect
#' @export
#' @examples
#' x=c(-73.1,-57.4,-54.6,-32.4,-13.9,  9.9, 46.8, 59.3, 91.8, 47.3)
#' y=c(-73.8,-57.6,-23.5,-20.6,-22.1,-41.9,-49.7,-26.3,-5.75,-34.1)
#' res=project_on_transect(x,y)
#' plot(x,y,col="blue",
#'      xlim=range(c(x,res$xt)),
#'      ylim=range(c(y,res$yt)))
#' abline(b=res$slopexy,a=res$interceptxy, col="red")
#' points(res$xp,res$yp, col="red")

project_on_transect=function(x,y){
    rdata=cbind(x,y)
    # center raw data
    means=apply(rdata,MARGIN=2, FUN="mean")
    data=apply(rdata,MARGIN=2,FUN=function(x){x-means})
    #
    dod=crossprod(data,data)
    eig=eigen(dod,symmetric=TRUE)
    M=eig$vectors
    # http://www.cs.otago.ac.nz/cosc453/student_tutorials/principal_components.pdf
    # transform data into final data (FD)
    RFV=t(M)
    RDA=t(data)
    FD=RFV%*%RDA
    FDb=FD
    # keep only first component of FD and transform back
    FDb[2,]=0
    RDAb=t(RFV)%*%FDb
    datan=t(RDAb)
    # "de-center" new coordinates
    datan=datan+matrix(rep(means,nrow(rdata)),byrow=TRUE, nrow=nrow(rdata))
    # get slope of first component in the x-y plan
    slopexy=eig$vectors[2,1]/eig$vectors[1,1]
    return(list(xp=datan[,1],
                yp=datan[,2],
                xt=FD[1,],
                yt=FD[2,],
                slopexy=slopexy,
                interceptxy=means[2]))
}
