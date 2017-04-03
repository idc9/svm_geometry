library(cccp)
library(Rglpk)

get_mean_difference <- function(data){
    
    class_means <- data %>% 
                    group_by(y) %>% 
                    summarise_each(funs(mean))

    
    mu_pos <- class_means %>% 
                    filter(y==1) %>% 
                    dplyr::select(-y) %>%
                    as.matrix() %>%
                    t()
    
    
    mu_neg <- class_means %>% 
                    filter(y==-1) %>% 
                    dplyr::select(-y) %>%
                    as.matrix() %>%
                    t()
    
    w_md <- mu_pos - mu_neg
    
    b_md  <- -(1/2)*( t(mu_pos) %*% mu_pos - t(mu_neg) %*% mu_neg )
    
    return(list(w=unname(w_md), b=b_md))
    
}

get_mdp_direction <- function(data){
    # The Maximal Data Piling direction is definied in
    # https://pdfs.semanticscholar.org/7614/72da42b7e5cd9cfe7305d3e7454129882cdd.pdf
    
    # Parameters
    # data: data frame with column y class labels (1/-1)
    
    # Output
    # v: MDP direction
    
    # number of data points
    n <- dim(data)[1]
    
    # individual class means
    mu_pos <- data %>% filter(y==1) %>% dplyr::select(-y) %>% as.matrix() %>% colMeans()
    mu_neg <- data %>% filter(y==-1) %>% dplyr::select(-y) %>% as.matrix() %>% colMeans()
    
    # pooled mean and covariance
    X_pool <- data %>% dplyr::select(-y) %>% as.matrix()
    mu_pool <- colMeans(X_pool)
    bar_pool <-  rep(1,n) %*% t(mu_pool)
    Sigma <- (1/(n-1)) * t(X_pool - bar_pool) %*% (X_pool - bar_pool)
    
    # Moore Penrose inverse of sigma
    Sigma_mpi <- ginv(Sigma)
    
    # MDP direction
    v <- Sigma_mpi %*% (mu_pos - mu_neg)
    v <- v / norm(v, type='F')
    
    return(v)
}

are_linearly_separable<- function(data){
    # determines if two point clouds are liearly separable
    # by checking if a LP has a soln
    # borrowing code from
    # http://www.joyofdata.de/blog/testing-linear-separability-linear-programming-r-glpk/
    # assumes data is a data frame with class label column y
    
    
    X <- getX(data)
    y <- getY(data)
    
    n <- dim(X)[1]
    d <- dim(X)[2]
    
    # constraint matrix
    A <- matrix(0, nrow=n, ncol=d+1)
    A[,1:d] <- X*y
    A[,d] <- y
    
    # constraint vector
    b <- rep(-1,n)
    
    # objective function is constant
    obj <- rep(0, d + 1)
    
    # by default GLPK assums positive boundaries for the
    # variables. but we need the full set of real numbers.
    bounds <- list(lower = list(ind = 1:(d+1), val = rep(-Inf, d+1)),
                   upper = list(ind = 1:(d+1), val = rep(Inf, d+1)))
    
    
    # solve LP
    lp_soln <- Rglpk_solve_LP(obj, A, rep("<=", n), b, bounds=bounds)
    
    # status == 0 means solution found
    if(lp_soln$status == 0){
        return(TRUE)
    }else{
        return(FALSE)
    }
    
    
}

get_hard_margin_svm <- function(data){
    # fits hard margin SVM
    # if data are not linearly sep returns NA
    # assumes data is a data frame with class label column y

    if(are_linearly_separable(data)){
        X <- getX(data)
        y <- getY(data)
        
        n <- dim(X)[1]
        d <- dim(X)[2]
        
        
        # constraint matrix
        ineq_constraint_mat <- matrix(0, n, d + 1)
        ineq_constraint_mat[,1:d] <-  X
        ineq_constraint_mat[,d + 1] <- 1
        ineq_constraint_mat <- - diag(y) %*% ineq_constraint_mat
        
        ineq_constrain_vec <- - rep(1, n)
        
        # set inequality constraints
        ineq_consts <- nnoc(G=ineq_constraint_mat, h = ineq_constrain_vec)
        
        # quadratic objective matrix
        obj_mat <- diag(d + 1)
        obj_mat[d+1, d+1] <- 0
        
        # objective vector
        obj_vec <- rep(0, d + 1)
        
        
        # solve QP
        capture.output(soln <- cccp(P=obj_mat, q=obj_vec, cList = list(ineq_consts)))
        
        # SVM solutions
        w_hmsvm <- getx(soln)[1:d]
        b_hmsvm <- getx(soln)[d]
        
        return(list(w=w_hmsvm, b=b_hmsvm))
    } else{
        return(NA)
    }

   
}

get_linear_classifier_error <- function(data, w, b){
    
    n <- dim(data)[1]
    d <- dim(data)[2] - 1 
    
    # put data into matrix form
    x <- data %>% dplyr::select(-y) %>% as.matrix()
    y <- data %>% dplyr::select(y)
    
    # compute decicion function
    f <- x %*% w + rep(b, n)
    
    pred <- ifelse(f > 0, 1, -1)
    
    
    mean(pred != y)
}

get_cvnx_hull_gap <- function(data){
    # computes the distance between the convex hulls of the two classes
    # using hard margin SVM

    hm_svm <- get_hard_margin_svm(data)
    
    # if data are not separable then convex hulls intersect
    if(typeof(hm_svm) == 'list'){
        gap <- 2/norm(as.matrix(hm_svm[['w']]), type='F')
    } else{
        gap <- 0
    }
    
    gap
}

