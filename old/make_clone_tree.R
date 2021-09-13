my.draw.sample.clone <- function(i,wid,clones,x,y,len,sig.shape=4,tree=NULL,wscale=1,beta_in=beta_in,branching=TRUE, fixed_angle=NULL, no_ccf=FALSE, spread=TRUE){
  v <- get("v",envir=parent.frame())
  # get the row of v that corresponds to the clone
  vi <- v[i,]
  # browser()
  tree <- get("tree",envir=parent.frame())
  # tree <- get("tree",envir=.GlobalEnv)
  # if (!is.na(vi$vaf) & vi$vaf > 0){
    if (!is.na(vi$parent) && vi$parent == -1){ #if root the clone extends the full width of the plot
      x0 <- x 
      xi <- y 
      y0 <- y
      len0 <- len
      x1 <- vi$x1
      x2 <- vi$x2
    
    }else{ #parent not root -- not trunk clone
      par <- v[v$lab == vi$parent,] #get parent clone
      # print(par)
      
      xi = vi$x.mid
      x1 = vi$x1
      x2 = vi$x2

##############SKETCHY CODE REPLACE###################      
    if(no_ccf==TRUE && spread==TRUE){
        if(branching==FALSE){
          parent_angle <- 0
        }


          siblings <- v[which(v$parent==par$lab),]

          if(nrow(siblings)>1 ){
            print(vi)
            x2_max <- siblings$x2[which.max(abs(siblings$x2))]
            x1_max <- siblings$x1[which.max(abs(siblings$x1))]
            # dist <- abs(x1_max-y2_mxx)/2
            dist_left <- abs(-wid/2 - par$y)
            dist_right <- abs(wid/2 - par$y)
            dist <- min(dist_left, dist_right)
            print("dist")
            print(dist)
            print(dist_left)
            print(dist_right)
           if (abs(xi-par$x.mid)<1e-3 & is.null(fixed_angle)){
                         # dist <- par$x.mid-par$y
                         # parent_angle <- atan(dist/par$len)
                         parent_angle <- 0
            } else if (xi >= par$x){

              parent_angle <- ifelse(is.null(fixed_angle),atan(dist/par$len), fixed_angle)
            } else if (xi<par$y){
              parent_angle <- ifelse(is.null(fixed_angle),atan(-dist/par$len), -fixed_angle)
            }
          } else{
            # dist <- par$x.mid-par$y
            # parent_angle <- ifelse(is.null(fixed_angle), atan(dist/par$len), 0)
            parent_angle <- tree$angle[which(tree$parent==par$parent & tree$tip == par$lab)]
          }
        
        r = tree$length[which(tree$parent==par$lab & tree$tip == vi$lab)]
        x.shift = r*sin(parent_angle)
        x0 = par$x + x.shift
        y.shift = r*cos(parent_angle)
        y0 = par$y+y.shift
        len0 = par$len - y.shift

        v[i,]$len <- len0
        v[i,]$y <- y0
        v[i,]$x <- x0
        tree$angle[which(tree$parent==par$lab & tree$tip == vi$lab)] <- parent_angle
   
        assign("v",v,envir=parent.frame())
        assign("tree",tree,envir=parent.frame())

       return(c(x.old=xi,x.new=x0,y.new=y0,len=len0,x1=x1,x2=x2))
 
      }
############SKETCHY CODE REPLACE#####################
if(no_ccf==TRUE && spread==FALSE){
        if(branching==FALSE){
          parent_angle <- 0
        }


          siblings <- v[which(v$parent==par$lab),]

          if(nrow(siblings)>1 & nrow(siblings)<4){
            print(vi)
            print(xi)
            x2_max <- siblings$x2[which.max(abs(siblings$x2))]
            x1_max <- siblings$x1[which.max(abs(siblings$x1))]
            # dist <- abs(x1_max-y2_mxx)/2
            dist_left <- abs(x1_max- par$x)
            dist_right <- abs(x2_max - par$x)
            dist <- min(dist_left, dist_right)
            print("dist here")
            print(dist)
            print(dist_left)
            print(dist_right)
           if (abs(xi-par$x.mid)<1e-3 & is.null(fixed_angle)){
                         # dist <- par$x.mid-par$y
                         # parent_angle <- atan(dist/par$len)
                         parent_angle <- 0
            } else if (xi > par$x.mid){

              parent_angle <- ifelse(is.null(fixed_angle),atan(dist/par$len), fixed_angle)
            print(parent_angle)

            } else if (xi<par$x.mid){
              parent_angle <- ifelse(is.null(fixed_angle),atan(-dist/par$len), -fixed_angle)
                    print(parent_angle)

            }
          } else if(nrow(siblings)>3 ){
            print(vi)
            print("gt 3")
            x2_max <- siblings$x2[which.max(abs(siblings$x2))]
            x1_max <- siblings$x1[which.max(abs(siblings$x1))]
            dist <- abs(x1_max-y2_mxx)/2
            if(x1_max != vi$x1 & x2_max != vi$x2){
              # dist_left <- abs(vi$x1- par$y)
              # dist_right <- abs(vi$x2 - par$y)
              # dist <- min(dist_left, dist_right)
              dist <- abs(xi - par$x)
            }
            print("dist")
            print(dist)
            # print(dist_left)
            # print(dist_right)
           if (abs(xi-par$x.mid)<1e-3){
                         # dist <- par$x.mid-par$y
                         # parent_angle <- atan(dist/par$len)
                         parent_angle <- 0
            } else if (xi > par$x.mid){

              parent_angle <- ifelse(is.null(fixed_angle),atan(dist/par$len), fixed_angle)
            } else if (xi<par$x.mid){
              parent_angle <- ifelse(is.null(fixed_angle),atan(-dist/par$len), -fixed_angle)
            }
          } else{
            # dist <- par$x.mid-par$y
            # parent_angle <- ifelse(is.null(fixed_angle), atan(dist/par$len), 0)
            parent_angle <- tree$angle[which(tree$parent == par$parent & tree$tip == par$lab)]
            # parent_angle <- 0
          }
        
        r = tree$length[which(tree$parent == par$lab & tree$tip == vi$lab)]
        x.shift = r*sin(parent_angle)
        x0 = par$x + x.shift
        y.shift = r*cos(parent_angle)
        y0 = par$y+y.shift
        len0 = par$len - y.shift
        print("y.shift")
        print(y.shift)

        v[i,]$len <- len0
        v[i,]$y <- y0
        v[i,]$x <- x0
        tree$angle[which(tree$parent==par$lab & tree$tip == vi$lab)] <- parent_angle
   
        assign("v",v,envir=parent.frame())
        assign("tree",tree,envir=parent.frame())

       return(c(x.old=xi,x.new=x0,y.new=y0,len=len0,x1=x1,x2=x2))
 
      }
#################################
      # if (par$parent == -1){   #if direct descendant from  truncal clone 
      #   siblings <- v[v$parent==par$lab,]
      #   if(nrow(siblings) ==1){
      #     parent_angle <- 0
      #   }        
      #   else if(nrow(siblings)==2){ #tree has branches
      #     if (yi > par$y){   #midpoint of clone is above the centre axis
      #       parent_angle <- ifelse(is.null(fixed_angle), atan(abs(par$y1)/par$len), fixed_angle)
      #     } else if (yi<par$y){ #midpoint of clone is below the centre axis
      #       parent_angle <- ifelse(is.null(fixed_angle), atan(-1*abs(par$y1)/par$len), -fixed_angle)
      #     }else{
      #       parent_angle=0
      #     }
      #   }else{
      #     if(v$lab == siblings$lab[which.min(siblings$x.mid)]){ #align leftmost child with the left outer clone border
      #       parent_angle <- ifelse(is.null(fixed_angle), atan(-1*abs(par$y1)/par$len), -fixed_angle)           
      #     } else if(v$lab == siblings$lab[which.max(siblings$x.mid)]){ #align rightmost child with the right outer clone border
      #       parent_angle <- ifelse(is.null(fixed_angle), atan(abs(par$y1)/par$len), fixed_angle)           
      #     } else{
      #       parent_angle <- atan((vi$x.mid - par$x.mid)/par$len)
      #     }
      #   }
      # }

      # else{  #not direct descendent of truncal clone compute the parent angle with the corners of the extreme clones rather than that particular clone

        siblings <- v[which(v$parent==par$lab),]
          if(nrow(siblings) == 1){
              dist <- par$x.mid-par$x
              parent_angle <- ifelse(is.null(fixed_angle) & no_ccf== FALSE, atan(dist/par$len), 0)
          } else if(nrow(siblings)==2){
              print(siblings)
              sibling_coords <- c(siblings$x1, siblings$x2)
              x1_max <- sibling_coords[which.max(abs(sibling_coords))]
              x2_max <- sibling_coords[which.max(abs(sibling_coords-x1_max))]

              # x2_max <- siblings$x2[which.max(abs(siblings$x2))]
              # x1_max <- siblings$x1[which.max(abs(siblings$x1))]
              dist <- abs(x1_max-x2_max)/2
              print(x2_max)
              print(x1_max)
              if (xi > par$x.mid){
                parent_angle <- ifelse(is.null(fixed_angle),atan(dist/par$len), fixed_angle)
              } else if (xi<par$x.mid){
                  parent_angle <- ifelse(is.null(fixed_angle),atan(-dist/par$len), -fixed_angle)
                }else{
                  dist <- par$x.mid-par$x
                  parent_angle <- atan(dist/par$len)
                }
          } else{
              if(v$lab == siblings$lab[which.min(siblings$x.mid)]){ #align leftmost child with the left outer clone border
                parent_angle <- ifelse(is.null(fixed_angle), atan(-1*abs(par$x1)/par$len), -fixed_angle)           
              } else if(v$lab == siblings$lab[which.max(siblings$x.mid)]){ #align rightmost child with the right outer clone border
                parent_angle <- ifelse(is.null(fixed_angle), atan(abs(par$x1)/par$len), fixed_angle)           
              } else{
                parent_angle <- atan((vi$x.mid - par$x.mid)/par$len)
              }
            }
        # }
   
      r <- tree$length[which(tree$parent==par$lab & tree$tip == vi$lab)]
      x.shift <- r*sin(parent_angle)
      x0 <- par$x + x.shift
      y.shift <- r*cos(parent_angle)
      y0 <- par$y + y.shift
      len0 <- par$len - y.shift
      
      if(no_ccf == FALSE){

        #make sure the node isn't outside of the parent clone
        par.coords <- data.frame(x=clones[[as.integer(which(v$lab == par$lab))]][["x"]], y= clones[[as.integer(which(v$lab == par$lab))]][["y"]])

        par$x1 <- clones[[as.integer(which(v$lab == par$lab))]][["x1"]]
        par$x2 <- clones[[as.integer(which(v$lab == par$lab))]][["x2"]]
        # browser()
        par.coords.pos <- par.coords[1:match(par$x1,par.coords$x),]
        par.coords.neg <- par.coords[match(par$x2,par.coords$x):length(par.coords$x),]
 
        match.x.pos  <- par.coords.pos$x[which.min(abs(par.coords.pos$y-y0))]
        match.x.neg  <- par.coords.neg$x[which.min(abs(par.coords.neg$y-y0))]
        while((match.x.pos > x0 & match.x.neg > x0) | (match.x.pos < x0 & match.x.neg < x0)){
          print("moving node")
          print(match.x.pos)

          print(match.x.neg)
          print(x0)
          closer <-  ifelse(match.x.pos > x0, min(match.x.pos,match.x.neg), max(match.x.pos,match.x.neg))
          further <-  ifelse(match.x.pos > x0, max(match.x.pos,match.x.neg), min(match.x.pos,match.x.neg))
          x0 <-  closer + 0.25*(further-closer)
          y.shift <- sqrt(r^2-x0^2)
          x.shift <- x0-par$x
          x0 <- par$x+x.shift
          parent_angle <- asin(y.shift/r)
          # print("iter")
          match.x.pos  <- par.coords.pos$x[which.min(abs(par.coords.pos$y-y0))]
          match.x.neg  <- par.coords.neg$x[which.min(abs(par.coords.neg$y-y0))]
        }

        len0 <- par$len - y.shift
      }
        tree$angle[which(tree$parent==par$lab & tree$tip == vi$lab)] <- parent_angle
    }
    
  v[i,]$len <- len0
  v[i,]$y <- y0
  v[i,]$x <- x0

  assign("v",v,envir=parent.frame())
  assign("tree",tree,envir=parent.frame())



  if(no_ccf){
    return(c(x.old=xi,x.new=x0,y.new=y0,len=len0,x1=x1,x2=x2))
    }
     
    clone.color <- vi$color  
    my.clone <- my.draw.clone( x=xi, x_r=x0, y_r=y0, x1=x1, x2=x2, wid=wid*vi$vaf, len=len0, col=clone.color, sig.shape, beta_in=beta_in,
                                                           wscale=wscale)
    return(c(my.clone,x.old=xi,x.new=x0,y.new=y0,len=len0,x1=x1,x2=x2,alpha=vi$alpha))
  # } 
}