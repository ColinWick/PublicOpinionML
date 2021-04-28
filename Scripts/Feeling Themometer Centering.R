FT_center <- function(x,flip,scale){
  
  if(scale %% 2 == 1){
    v <- ceiling(scale/2)

    # Then centering all values
    if(flip == T){
      x <- -1*(x-v)
    } else {
      x <- x-v
    }
    
  } else {
    v <- scale/2
    
    # Then centering all values
    if(flip == T){
      x <- -1*ifelse(x-v > 0,x-v,(x-v-1))
    } else {
      x <- ifelse(x-v > 0,x-v,(x-v-1))
    }
  }
  return(x)
}

DK_neutral <- function(x,flip,scale){
  if(scale %% 2 == 1){
    v <- ceiling(scale/2)
    # Centering Refused
    x <- ifelse(x == 15,v,x)
  } else {
    v <- scale/2
    x <- ifelse(x == 15,v,x)
  }
  return(x)
}

# Setting refused / don't know to neutral
refused_attitude <- function(x){
  if(x %in% c(-8,-9,99)){
    x <- 15
  } else {
    x <- x
  }
} 


survey_na <- function(x){
  if(x == -9){
    x <- NA_real_
  } else {
    x <- x
  }
}


# Setting technical non-response to NA 
survey_na_att <- function(x){
  if(x < 0){
    x <- NA_integer_
  } else if(x > 500) {
    x <- 0
  } else {
    x <- x
  }
}
