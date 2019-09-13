
sigmoid = function(params, g) {
  exp(params[1]-params[2]*g) / (1 + exp(params[1] -  (g* params[2])))
}

sigmoid.up = function(params, g) {
  exp(params[1] + params[2]*g) / (1 + exp(params[1] +  (g* params[2])))
}


my.draw.clone <- function(x, x_r, y_r, x1,x2, wid=1, len=1, col='gray',  sig.shape=4, beta_in=3, #x real and y real
                        bell.curve.step = 0.25,
                        wscale=1

){
  
  # browser()

  x_r <- x_r
  x_old <- x #midpoint between y1 and y2
  y <- y_r
  in.len <- len
  in.wid <- wid

  vaf <- in.wid 
  
  
  beta <- len/beta_in
  y1 <- y + beta
  
  while(y1 <= y){
    y1 = y1+1
  }
  # xx = seq(x0+beta0,x1,(x1-(x0+beta0))/100)
  yy <- seq(y,y1,length.out=100)
  
  #params.d  <- c(-0.7310133, 4.8038079) #generic sigmoid
  
  params.d  <- c(-0.7310133, sig.shape)
  y.ex <- seq(-1,1.5,length.out=length(yy))
  xu.2 <- sigmoid(params.d,y.ex)
  
  #scale and shift each sigmoid
  x.rt.d <- (xu.2-min(xu.2))/(max(xu.2-min(xu.2)))*(x_r-x2)+x2
  x.rt.u <- (-xu.2-max(-xu.2))/(max(xu.2+max(-xu.2)))*(x1-x_r)+x1
 
     
  yy.plot <- c(y, yy, y+in.len, y+in.len, rev(yy))
  xx <- c(x_r, x.rt.u, x1, x2, rev(x.rt.d))

 # browser()
 return(list(x=xx,y=yy.plot,col=col))  
}
