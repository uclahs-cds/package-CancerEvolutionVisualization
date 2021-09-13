set.position <- function(v){
  v$y.shift = 0
  max.vaf = max(v$vaf)
  scale = 0.5/max.vaf
  #debug
  for (i in 1:nrow(v)){
    vi = v[i,]
    subs = v[!is.na(v$parent) & v$parent == vi$lab,]
    if (nrow(subs) == 0){next}
    vafs = subs$vaf
    margin = (vi$vaf - sum(vafs))/length(vafs)*scale
    sp = 0
    if (margin > 0){
      margin = margin*0.75
      sp = margin*0.25
    }
    spaces = rep(sp, length(vafs))
    if (length(spaces) >= 2){
      for (j in 2:length(spaces)){
        spaces[j] = sum(vafs[1:j-1]+margin)
      }
    }else{
      # re-centering if only 1 subclone inside another
      spaces = (vi$vaf-vafs)/2
    }
    #debug
    #print(subs)
    v[!is.na(v$parent) & v$parent == vi$lab,]$y.shift = spaces
  }
  #print(v)
  return(v)
}