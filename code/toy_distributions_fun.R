library(MASS)

guasssian_meatball_distribution <- function(offset, d){
    function(n_neg, n_pos, seed=NA){
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
}


gmm_distribution2d <- function(mean_seed=NA){
    if(!is.na(mean_seed)){
        set.seed(mean_seed)
    }
    
    # grand class means
    grand_mu_pos <- c(1, 0)
    grand_mu_neg <- c(0, 1)
    
    # sample class means
    mu_pos <- mvrnorm(n=10, mu=grand_mu_pos, Sigma=diag(2))
    mu_neg <- mvrnorm(n=10, mu=grand_mu_neg, Sigma=diag(2))
    
    
    function(n_neg, n_pos, seed=NA){
        if(!is.na(seed)){
            set.seed(seed)
        }
        
        # pick which means to sample from
        m_index_pos <- sample.int(10, n_pos, replace = TRUE)
        m_index_neg <- sample.int(10, n_neg, replace = TRUE)
        
        # sample data from each class
        X_pos <- map(1:n_pos,function(i) mvrnorm(n=1, mu=mu_pos[m_index_pos[i], ], Sigma=diag(2)/5)) %>% 
            unlist %>% 
            matrix(ncol=n_pos) %>% 
            t %>%
            as_tibble() %>%
            mutate(y=1) 
        
        
        X_neg <- map(1:n_neg,function(i) mvrnorm(n=1, mu=mu_neg[m_index_neg[i], ], Sigma=diag(2)/5)) %>% 
            unlist %>% 
            matrix(ncol=n_neg) %>% 
            t %>%
            as_tibble() %>%
            mutate(y=-1) 
        
        # set column names
        colnames(X_pos) <- gsub("V", 'x', colnames(X_pos))
        colnames(X_neg) <- gsub("V", 'x', colnames(X_neg))
        
        # put data into one data frame
        data <- rbind(X_pos, X_neg)%>% 
            mutate(y =factor(y)) # class label should be a factor
        
        data
    }
}
