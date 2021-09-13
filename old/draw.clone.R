draw.clone <- function(x, y, x_r, y_r, wid=1, len=1, col='gray',    #x real and y real
                       clone.shape='bell',
                       bell.curve.step = 0.25,
                       label=NA, cell.frac=NA,
                       #cell.frac.position='top.out',
                       cell.frac.position='right.mid',
                       cell.frac.top.out.space = 0.75,
                       cell.frac.side.arrow.width=1.5,
                       cell.frac.angle=NULL,
                       cell.frac.side.arrow=TRUE,
                       cell.frac.side.arrow.col='black',
                       variant.names=NULL,
                       variant.color='blue',
                       variant.angle=NULL,
                       text.size=1,
                       border.color='black',
                       border.width=1,
                       wscale=1
){
  beta = min(wid/5, (wid+len)/20)
  gamma = wid/2  #fraction vaf/2
  vaf = wid 
  #cat('len=',len, '\n')
  
#bell
      beta = min(wid/5, (wid+len)/10, len/3)
    beta = beta*wscale
    xx0= c(x, x+beta, x+len, x+len, x+beta)
    yy0 = c(y, y+gamma, y+gamma, y-gamma, y-gamma)
    #polygon(xx, yy, border='black', col=col, lwd=0.2)
    
    gamma.shift = min(bell.curve.step, 0.5*gamma)
    
    # this is to prevent a coeff from being NaN when curve is generated below
    #if (beta <= 0.25){beta = 0.3}
    zeta = min(0.25, max(beta-0.1,0))
    #print(len)
    
    # shorter time, curve earlier
    zeta = zeta*len/7
    zeta = zeta*wscale
    
    x0=x+zeta; y0=0; x1=x+beta; y1 = gamma - gamma.shift
    n = 3; n = 1 + len/3
    a = ((y0^n-y1^n)/(x0-x1))^(1/n)
    b = y0^n/a^n - x0
    c = y
    #cat('a=', a, 'b=', b, 'c=', c, 'gamma=', gamma, 'len=', len, 'x0=',
    #    x0, 'x1=', x1, 'y0=', y0, 'y1=', y1,'\n')
    #curve(a*(x+b)^(1/n)+c, n=501, add=T, col=col, xlim=c(x0,x1))
    #curve(-a*(x+b)^(1/n)+c, n=501, add=T, col=col, xlim=c(x0,x1))
    
    beta0 = beta/5
    if (x0+beta0 > x1){beta0 = (x1-x0)/10}
    gamma0 = gamma/10
    

    xx = seq(x0+beta0,x1,(x1-x0)/100)
    yy = a*(xx+b)^(1/n)+c
   # yy = c(y, y.sig.f, y+gamma, y-gamma, y.sig.r)
    yy = c(y, yy, y+gamma, y-gamma, -a*(rev(xx)+b)^(1/n)+c)
    xx = c(x, xx, x+len, x+len, rev(xx))
    polygon(xx, yy, border=border.color, col=col, lwd=border.width)
  }
    
    ####playing around with sigmoids
    x.sig=seq(-1,1-2/77,2/77)
   # x.sig = seq(x0+beta0,x1,(x1-x0)/100)
   
    
    
     b=x.sig[38]
    m=10
    y.sig <- exp((b - m*x.sig)) / (1 + exp((b - m*x.sig)))
    y.sig.f <- -1*(y.sig-1)*3.75+1
    plot(x.sig+3.48, y.sig.f, xlab="X", ylab="P(Y=1)")
    
    m_r=5
    y.sig.r  <- exp((b + m_r*x.sig)) / (1 + exp((b + m_r*x.sig)))
    y.sig.r <- (y.sig.r-1)*3.75+1
    plot(x.sig+3.48, rev(y.sig.r), xlab="X", ylab="P(Y=1)")
    ##smart
    
    
    # m=10
    # b=0
    # y.sig <- exp((b - m*x.sig)) / (1 + exp((b - m*x.sig)))
    # plot(x.sig, y.sig, xlab="X", ylab="P(Y=1)")
    
    #y-gamma=4.75
    #y1 = 1.75
    
    sigmoid = function(params, g) {
      exp(params[1]-params[2]*g) / (1 + exp(params[1] -  (g* params[2])))
    }
    
    sigmoid.up = function(params, g) {
      exp(params[1] + params[2]*g) / (1 + exp(params[1] +  (g* params[2])))
    }
    
    
    y.ex <- c(seq(1,1.1,length.out=30),rep(NA,0), seq(4.55,4.75,length.out=30)) +rnorm(60,sd=0.05)
    y.scaled <- (y.ex-min(y.ex,na.rm=TRUE))/max(y.ex-min(y.ex,na.rm=TRUE),na.rm=TRUE)
    x.ex <- seq(-1,1,length.out=60)
    y.scaled[30:46] <- NA
    fitmodel <- nls(y.scaled~exp(b - m_s*x.ex)/(1 + exp(b - (m_s*x.ex))), start=list(b=0,m_s=10))
    params <- coef(fitmodel)
    y2<- sigmoid(params,x.ex)
    plot(y2~x.ex,type="l")

    
    y.ex <- c(seq(1,1.1,length.out=30),rep(NA,0), seq(1.55,1.75,length.out=30)) +rnorm(60,sd=0.05)
    y.scaled <- (y.ex-min(y.ex,na.rm=TRUE))/(4.75-min(y.ex,na.rm=TRUE))
   plot(y.ex~x.ex,type="l")
   plot(y.scaled~x.ex,type="l")
    
    fitmodel <- nls(y.scaled~exp(b - m_s*x.ex)/(1 + exp(b - (m_s*x.ex))), start=list(b=0,m_s=10))
    params <- coef(fitmodel)
    y2<- sigmoid(params,x.ex)
    plot(y2~x.ex,type="l")
    
    
    y.rt <- (y2)*3.75+min(y.ex)
    plot(x.ex+3.48,y.rt)
    
    
    ####################
    x=2
    len=6
    y_old = 1
    y_r =3.75
    gamma <- 7.5/2
    y1 = y_old+gamma
    y2 = y_old-gamma
    
   # beta = min(wid/5, (wid+len)/10, len/3)
  #  beta = beta*wscale
    
   # beta0 = beta/5
    x0=x+zeta;  x1=x+beta; 
    if (x0+beta0 > x1){beta0 = (x1-x0)/10}
    gamma0 = gamma/10
    
    beta=len/2
    x1 = x +beta
    xx = seq(x0+beta0,x1,(x1-x0)/100)
  #  xx = seq(x0+beta0,x1,(x1-x0)/100)
    
    lx = length(xx)
    len_first = floor((lx-lx/4)/2)
    len_second = floor(lx/4)
    len_third = lx -len_first - len_second
    
    #lower bound
    y.ex.d <- c(seq(y_r,y_r+0.1,length.out=len_first),rep(NA,len_second), seq(y2-0.1,y2,length.out=len_third)) +rnorm(lx,sd=0.01)
    x.ex <- seq(-1,1,length.out=lx)
    y.scaled.d <- (y.ex.d-min(y.ex.d,na.rm=TRUE))/(y_old-min(y.ex.d,na.rm=TRUE))
    
    y.scaled.d <- (y.ex.d-min(y.ex.d,na.rm=TRUE))/((max(y.ex.d,na.rm=TRUE)-min(y.ex.d,na.rm=TRUE))*1.2)
    plot(y.scaled.d~x.ex,type="l")
    plot(y.scaled.d~x.ex,type="l")
    plot(y.ex.d~x.ex)
    
    fitmodel.d <- nls(y.scaled.d~exp(b - m_s*x.ex)/(1 + exp(b - (m_s*x.ex))), start=list(b=0,m_s=10))
    params.d <- coef(fitmodel.d)
    yd<- sigmoid(params.d,x.ex)
    plot(yd~x.ex,type="l")    
    
   # y.rt.d <- (yd)*(y_old-min(y.ex.d,na.rm=TRUE))+min(y.ex.d,na.rm=TRUE)
    y.rt.d <- (yd)*(max(y.ex.d,na.rm=TRUE)*1-min(y.ex.d,na.rm=TRUE))+min(y.ex.d,na.rm=TRUE)
    y.rt.d[y.rt.d>y_r] <- y_r   # fix any error
    y.rt.d[y.rt.d < y2] <- y2
    plot(x.ex,-y.rt.d)
    
    #upper bound
    
    y.ex.u <- c(seq(y_r,y_r+0.1,length.out=len_first),rep(NA,len_second), seq(y1-0.1,y1,length.out=len_third)) +rnorm(lx,sd=0.01)
    y.scaled.u <- (y.ex.u-min(y.ex.u,na.rm=TRUE))/((max(y.ex.u,na.rm=TRUE)-min(y.ex.u,na.rm=TRUE))*1.2)
    plot(y.scaled.u~x.ex,type="l")
    plot(y.ex.u~x.ex)
    
    fitmodel.up <- nls(y.scaled.u~exp(b + m_s*x.ex)/(1 + exp(b + (m_s*x.ex))), start=list(b=0,m_s=10))
    params.up <- coef(fitmodel.up)
    yu <- sigmoid.up(params.d,x.ex)
    yu.2 <- sigmoid(params.d,x.ex)
    plot(yu.2~x.ex,type="l")    
    plot(yu~x.ex)
    # 
    
    
    y_r=3
    
     y.rt.d <- ((yu.2)*(y_r-y2)) +y2
     y.rt.u <- ((-yu.2)*(y1-y_r)) + y1
    # y.rt.u <- (yu)*(max(y.ex.u,na.rm=TRUE)-min(y.ex.u,na.rm=TRUE))+min(y.ex.u,na.rm=TRUE)
    # y.rt.d[y.rt.d>y_r] <- y_r   # fix any error
    # y.rt.d[y.rt.d < y2] <- y2
     plot(x.ex,y.rt.d,type='l')
     plot(x.ex,y.rt.u,type='l')
     #plot(xx ,y.rt.u,type='l')
    # points(x.ex, -y.rt.u)
    #=
    
 #   y.rt.u <- (yu)*(max(y.ex.u,na.rm=TRUE)-min(y.ex.u,na.rm=TRUE))+min(y.ex.u,na.rm=TRUE)
    
  #  y.rt.u[y.rt.u>y1] <- y1   # fix any error
#    y.rt.u[y.rt.u < y_r] <- y_r
 #   y.rt.u <- rev(y.rt.u)
    plot(x.ex,y.rt.u,type='l')
    points(x.ex,-(y.rt.d)+7.5)
    
  
    ############
    
   # yy = a*(xx+b)^(1/n)+c

   # yy = c(y, yy, y+gamma, y-gamma, -a*(rev(xx)+b)^(1/n)+c)
  #  xx = c(x, xx, x+len, x+len, rev(xx))
    
    yy = c(y_r, y.rt.u, y1, y2, rev(y.rt.d))
    plot(c(0,10),c(-10,10),type="n")
    polygon(xx, yy, border=border.color, col=col, lwd=border.width)
    clone.plot + layer

  }