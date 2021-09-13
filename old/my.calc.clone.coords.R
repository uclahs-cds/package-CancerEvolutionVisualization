my.calc.clone.coords <- function(i,wid,clones,x,y,len,sig.shape=4,tree=NULL,wscale=1,branching=TRUE, fixed_angle=NULL){
  v <- get("v",envir=parent.frame())
  # print(v)
  # print("quant")
  # print(branching)
  # get the row of v that corresponds to the clone
  vi <- v[i,]
  if (!is.na(vi$parent) && vi$parent == -1){ #if root the clone extends the full width of the plot
    x0 <- x 
    yi <- y 
    y0 <- y
    len0 <- len
    y1 = vi$y1
    y2 = vi$y2
  
  }else{ #parent not root -- not trunk clone
    par <- v[v$lab == vi$parent,] #get parent clone
    yi = vi$y.mid
    y1 = vi$y1
    y2 = vi$y2
    if (par$parent == -1){   #if direct descendant from  truncal clone 
        if(branching==FALSE){
          parent_angle <- 0
        }
        else{ #tree has branches
          # browser()
          print("par")
          print(par)
          if (yi > par$y){   #midpoint of clone is above the centre axis
            parent_angle <- ifelse(is.null(fixed_angle), atan(1*abs(par$y1)/par$len), fixed_angle)
          } else if (yi<par$y){ #midpoint of clone is below the centre axis
            parent_angle <- ifelse(is.null(fixed_angle), atan(-1*(abs(par$y1))/par$len), -fixed_angle)
          }else{
            parent_angle=0
          }

        }
      print("angle")
      print(parent_angle)
      }

  else {

        siblings <- v[which(v$parent == par$lab),]
          #get("v[v$parent == par$lab]", envir=.GlobalEnv) #

        if(nrow(siblings)>1){
          # browser()
          print(siblings)
          dist = (max(siblings$y1,siblings$y2)-min(siblings$y2,siblings$y1))/2
          print("dist")
          print(dist)
          print("len")
          print(par$len)
          if (yi > par$y.mid){
            # parent_angle <- atan(dist/par$len)
            parent_angle = ifelse(is.null(fixed_angle),atan(dist/par$len), fixed.angle)
          } else if (yi<par$y){
            parent_angle = ifelse(is.null(fixed_angle),-atan(dist/par$len), -fixed.angle)
            # parent_angle = -pi/6
            # parent_angle <- atan(-dist/par$len)
          }else{
            parent_angle=0
          }
      } else{parent_angle=0}
      
   
  }

  r = tree$length[which(tree$parent==par$lab & tree$tip == vi$lab)]
  x.shift = r*cos(parent_angle)
  x0 = par$x + x.shift
  y.shift = r*sin(parent_angle)
  y0 = par$y+y.shift
  len0 = par$len - x.shift
  tree$angle[which(tree$parent==par$lab & tree$tip == vi$lab)] <- parent_angle
 } 

     v[i,]$len <- len0
     v[i,]$x <- x0
     v[i,]$y <- y0
      print("v")
      print(v)
     assign("v",v,envir=parent.frame())
     assign("tree",tree,envir=parent.frame())
     return(c(y.old=yi,x.new=x0,y.new=y0,len=len0,y1=y1,y2=y2))
}