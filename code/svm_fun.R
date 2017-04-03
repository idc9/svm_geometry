library(e1071)

set_svm_model <- function(){
    
    function(tr_data, cost){
        svm(y ~., data=tr_data,
            scale=FALSE, type='C-classification', shrinking=FALSE,
            kernel='linear',  cost=cost)
    }
}

get_folds <- function(n, k_cv, cv_seed=NA){
    # gets folds indices for cross validation
    
    # returns a list of vectors
    fold_size <- floor(n/k_cv)
    
    if(!is.na(cv_seed)){
        set.seed(cv_seed)
    }
    
    map(1:k_cv, function(x) sample.int(n=n, size=fold_size, replace=FALSE))
}


get_svm_cv_err <- function(data, svm_model, cost, k_cv, cv_seed=NA){
    # returns the SVM cross validation error
    # use teh cv_seed to make sure folds stay the same across multiple usages

    n <- dim(data)[1]
    folds <- get_folds(n, k_cv, cv_seed)
    
    
    cv_errs <- rep(0, k_cv)
    for(k in 1:k_cv){
        
        # split data into train/test sets
        data_cv_tr <- data[-folds[[k]], ]
        data_cv_tst <- data[folds[[k]], ]
        
        svmfit_cv <- svm_model(data_cv_tr, cost)
        
        cv_errs[k] <- mean(data_cv_tst[['y']] != predict(svmfit_cv, dplyr::select(data_cv_tst, -y)))
    }
    
    mean(cv_errs)
}


get_svm_parmas <- function(svmfit){
    # returns the normal vector and intercept of SVM
    # svm_model a fit svm object from the e1071 package
    # returns a list with (w, b) where w is the nv and b is the intercept
    
    # returns the normal vector and intercept (w*x + b)
    w <- colSums(c(svmfit$coefs) * svmfit$SV)
    b <- svmfit$rho
    
    return(list(w=w, b=b))
}


get_svm_data <- function(svmfit, data){
    
    cost <- svmfit$cost
    
    svm_params <- get_svm_parmas(svmfit)
    w_svm <- svm_params['w'][[1]]
    b_svm <- svm_params['b'][[1]]
    
    
    # add alpha coefficients
    data_svm <- mutate(data, alpha=0, sup_vec=FALSE)
    data_svm[svmfit$index, 'alpha'] = svmfit$coefs
    
    # identify support vectors
    data_svm[svmfit$index, 'sup_vec'] = TRUE
    
    # decision function values
    data_svm['decision_val'] <- svmfit$decision.values
    
    # identify margin vectors i.e. abs(decision value) == 1
    epsilon <- min(cost / 100, 1e-5)
    
    data_svm <- data_svm %>% 
                mutate(margin_vec = abs(abs(decision_val) - 1) < epsilon) 
                # mutate(margin_vec = near(abs(decision.values), 1)) 

    # sort(abs(abs(data_svm$decision_val) - 1))
    # identify slack vectors i.e. non-margin support vectors
    data_svm <- data_svm %>% 
                mutate(slack_vec = sup_vec & !margin_vec)
    
    data_svm
}


get_small_C_cutoff <- function(data){
    # gets the total slack bound for the small C svm limit
    n_pos <- data %>% filter(y==1) %>% dim() %>% .[1]
    n_neg <- data %>% filter(y==-1) %>% dim() %>% .[1]
    
    diam <- get_diameter(data)
    
    
    2/(max(n_pos, n_neg) * diam^2)
    
}


get_large_C_cutoff <- function(data){
    # gets the hard margin bound for the large C svm limit
    gap <- get_cvnx_hull_gap(data)
    2 / gap^2
}


