library(MASS)



guasssian_meatballs_2d <- function(n_pos, n_neg, mu_pos, mu_neg, sigma_pos, sigma_neg, seed=NA){
    if(!is.na(seed)){
        set.seed(seed)
    }
    
    # samples points from two classes of a two dimensional guassian distribution
    # returns a data frame with columns: x1, x2, y
    # n_pos, n_neg: number of points in each class
    # mu_pos, mu_neg: class means (vector length two)
    # sigma_pos, sigma_neg: number of points in each class (2x2 matrix)
    
    # generate data from negative class
    class_neg <- mvrnorm(n=n_neg, mu=mu_neg, Sigma=sigma_neg) %>% # mvrnorm comes from MASS
        as_tibble() %>%
        mutate(y=-1) %>%
        rename(x1=V1, x2=V2)
    
    # generate data from positive class
    class_pos <- mvrnorm(n=n_pos, mu=mu_pos, Sigma=sigma_pos) %>% 
        as_tibble() %>%
        mutate(y=1) %>%
        rename(x1=V1, x2=V2)
    
    # put data into one data frame
    data <- rbind(class_pos, class_neg)%>% 
        mutate(y =factor(y)) # class label should be a factor
    
    data
}


guasssian_meatballs <- function(n_pos, n_neg, offset, d, seed=NA){
    if(!is.na(seed)){
        set.seed(seed)
    }
    
    mu_pos <- rep(0, d)
    mu_pos[1] <- offset
    sigma_pos <- diag(d)
    
    mu_neg <- rep(0, d)
    mu_neg[1] <- -offset
    sigma_neg <- diag(d)
    
    
    # samples points from two classes of a two dimensional guassian distribution
    # returns a data frame with columns: x1, x2, y
    # n_pos, n_neg: number of points in each class
    # mu_pos, mu_neg: class means (vector length two)
    # sigma_pos, sigma_neg: number of points in each class (2x2 matrix)
    
    # generate data from negative class
    class_neg <- mvrnorm(n=n_neg, mu=mu_neg, Sigma=sigma_neg) %>% # mvrnorm comes from MASS
        as_tibble() %>%
        mutate(y=-1) 
    colnames(class_neg) <- gsub("V", 'x', colnames(class_neg))
    
    # generate data from positive class
    class_pos <- mvrnorm(n=n_pos, mu=mu_pos, Sigma=sigma_pos) %>% 
        as_tibble() %>%
        mutate(y=1)
    
    colnames(class_pos) <- gsub("V", 'x', colnames(class_pos))
    
    # put data into one data frame
    data <- rbind(class_pos, class_neg)%>% 
        mutate(y =factor(y)) # class label should be a factor
    
    data
}




