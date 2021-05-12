FT_center <- function(x,flip,scale){
  
  if(scale %% 2 == 1){
    v <- ceiling(scale/2)
    x <- ifelse(x > scale,v,x)
    # Then centering all values
    if(flip == T){
      x <- -1*(x-v)/v
    } else {
      x <- (x-v)/v
    }
    
  } else {
    v <- scale/2
    x <- ifelse(x > scale,v+.5,x)
    # Then centering all values
    if(flip == T){
      x <- -1*ifelse(x-v > 0,x-v,(x-v-1))/v
    } else {
      x <- ifelse(x-v > 0,x-v,(x-v-1))/v
    }
  }
  return(x)
}

# Setting refused / don't know to neutral
refused_attitude <- function(x){
  if(x %in% c(-8,-9)){
    x <- 15
  } else {
    x <- x
  }
} 


survey_na <- function(x){
  if(is.na(x)){
    x <- NA_real_
  } else if(x == -9 | x == -8){
    x <- NA_real_
  } else {
    x <- x
  }
}


# Setting technical non-response to NA 
survey_na_att <- function(x){
  if(is.na(x)){
    x <- NA_integer_
  } else if(x > 500) {
    x <- 0
  } else {
    x <- x
  }
}

sd_na <-  function(x) {
  sd(x,na.rm = T)
}

