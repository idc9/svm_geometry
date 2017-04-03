library(flexclust)


#Gets the angle between two vectors a and b in degrees
angle_btwn <- function(a,b){
    
    epsilon <- 1e-10
    
    dot_prod <- sum(a*b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))
    
    # safe arc cosine
    if(abs(abs(dot_prod) - 1) < epsilon){
        return(0)
    } else{
        return((180/pi)*acos(dot_prod))
    }
    
    
    
}


unwrap_list <- function(L, name){
    unlist(unname(sapply(L, function(x) x[name])))
}


getX <- function(data){
    # returns the X matrix from a data frame
    # assumes class labels column is y
    data %>% dplyr::select(-y) %>% as.matrix() %>% unname()
}

getY <- function(data){
    # returns the y vector from a data frame
    # assumes class labels column is y
    data %>% dplyr::select(y) %>% as.matrix() %>% as.numeric() %>% unname()
}


get_diameter <- function(data){
    Xpos <- data %>% filter(y==1) %>% dplyr::select(-y) %>% as.matrix
    Xneg <- data %>% filter(y==-1) %>% dplyr::select(-y)%>% as.matrix
    
    btween_class_dists <- dist2(Xpos, Xneg)
    
    return(max(btween_class_dists))
}

