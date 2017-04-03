library(tidyverse)
library(e1071)


# source('toy_distributions.R')
# source('svm_fun.R')
# source('linear_classifiers.R')
# source('help_fun.R')




get_svm_tuning_data <- function(tr_data, tst_data, svm_model, cost_vals, cv_params){
    
    # data dimensions
    d <- dim(tr_data)[2] - 1
    n <- dim(tr_data)[1]
    
    # get mean difference fit
    md_params <- get_mean_difference(tr_data)
    w_md <- md_params['w'][[1]]
    b_md <- md_params['b'][[1]]
    
    # compute md error rates
    md_results <- list(tr_err = get_linear_classifier_error(tr_data, w_md, b_md),
                       tst_err = get_linear_classifier_error(tst_data, w_md, b_md))
    
    
    # MDP direction
    w_mdp <- get_mdp_direction(tr_data)
    
    angle_mdp_md <- angle_btwn(w_mdp, w_md)
    mdp_results <- list(w_mdp=w_mdp,
                        angle_mdp_md=angle_mdp_md)
    
    # HM-SVM direction
    if(are_linearly_separable(tr_data)){
        w_hmsvm <- get_hard_margin_svm(tr_data)[['w']]
    }else{
        w_hmsvm <- NA
    }
    
    svm_results <- list()
    for(i in 1:length(cost_vals)){
        cost <- cost_vals[i]
        
        svmfit <- svm_model(tr_data, cost=cost)
        
        # get the svm direction
        svm_params <- get_svm_parmas(svmfit)
        w_svm <- svm_params['w'][[1]]
        b_svm <- svm_params['b'][[1]]
        
        # compute the margin
        margin <- 1/norm(matrix(w_svm), type='f')
        
        # get svm data
        data_svm <- get_svm_data(svmfit, tr_data)
        
        # types of svm vectors
        svm_vectors <- data_svm %>% 
            summarise(n_sup_vec=sum(sup_vec),
                      n_margin_vec=sum(margin_vec),
                      n_slack_vec=sum(slack_vec))
        
        # svm tr, tst, cv error
        svm_tr_err <- mean(tr_data[['y']] != predict(svmfit, dplyr::select(tr_data, -y)))
        svm_tst_err <- mean(tst_data[['y']] != predict(svmfit, dplyr::select(tst_data, -y)))
        svm_cv_err <- get_svm_cv_err(tr_data, svm_model, cost, k_cv=cv_params[['k']], cv_seed=cv_params[['seed']])
        
        
        
        
        
        # if d = 2 then make the svm plots
        if(d==2){
            plt <- get_svm_plot_2d(tr_data, svmfit)
        }else{
            plt <- NA
        }
        
        
        
        # angle between svm and md/mdp/hmsvm
        angle_svm_md <- angle_btwn(w_md, w_svm)
        angle_svm_mdp <- angle_btwn(w_mdp, w_svm)
        angle_svm_hmsvm <- angle_btwn(w_hmsvm, w_svm)

        # return results as a list
        svm_results[[i]] <- list(cost=cost,
                                 angle_svm_md=angle_svm_md,
                                 angle_svm_mdp=angle_svm_mdp,
                                 angle_svm_hmsvm=angle_svm_hmsvm,
                                 tst_err=svm_tst_err,
                                 tr_err=svm_tr_err,
                                 cv_err=svm_cv_err,
                                 svm_vectors=svm_vectors,
                                 margin = margin,
                                 plt=plt)
        
    }
    
    list(svm=svm_results,
         md=md_results)
}


get_svm_vectors <- function(results){
    
    # get cost values
    cost <- unwrap_list(results[['svm']], 'cost')
    nu_cost <- length(cost)
    
    
    n_sup_vec <- unlist(map(1:21, function(i) results[['svm']][[i]][['svm_vectors']][['n_sup_vec']]))
    n_margin_vec <- unlist(map(1:21, function(i) results[['svm']][[i]][['svm_vectors']][['n_margin_vec']]))
    n_slack_vec <- unlist(map(1:21, function(i) results[['svm']][[i]][['svm_vectors']][['n_slack_vec']]))
    
    
    tibble(cost=cost,
           support=n_sup_vec,
           margin=n_margin_vec,
           slack=n_slack_vec)
}



get_svm_plot_2d <- function(data, svmfit){
    
    cost <- svmfit$cost
    
    # get md direction
    md_params <- get_mean_difference(data)
    w_md <- md_params['w'][[1]]
    b_md <- md_params['b'][[1]]
    
    
    # get svm direction
    svm_params <- get_svm_parmas(svmfit)
    w_svm <- svm_params['w'][[1]]
    b_svm <- svm_params['b'][[1]]
    
    
    # get svm data
    data_svm <- get_svm_data(svmfit, data)
    
    # types of svm vectors
    svm_vectors <- data_svm %>% 
        summarise(n_sup_vec=sum(sup_vec),
                  n_margin_vec=sum(margin_vec),
                  n_slack_vec=sum(slack_vec))
    
    # plot title
    title <- paste0('cost= ', cost,
                    ', sup vec= ', svm_vectors[['n_sup_vec']],
                    ', margin vec= ', svm_vectors[['n_margin_vec']],
                    ', slack vec= ', svm_vectors[['n_slack_vec']])
    
    # square box that contains all the data
    lim <- 1.2 * max(data_svm %>% 
                    dplyr::select(x1, x2) %>% 
                    summarise(x1=max(abs(x1)), x2=max(abs(x2))))
    
    # plot svm
    p <- ggplot(data=data_svm) +
            geom_point(aes(x=x1, y=x2, color=y, alpha=sup_vec, shape=margin_vec)) +
            geom_abline(slope=-w_svm[1]/w_svm[2], intercept = b_svm/w_svm[2]) +
            geom_abline(slope=-w_svm[1]/w_svm[2], intercept = (b_svm + 1)/w_svm[2], linetype = 2)+
            geom_abline(slope=-w_svm[1]/w_svm[2], intercept = (b_svm - 1)/w_svm[2], linetype = 2) + 
            ggtitle(title) + 
            xlim(-lim, lim) + 
            ylim(-lim, lim)
            
    
    # add mean difference
    p <- p + geom_abline(slope=-w_md[1]/w_md[2], intercept = b_md /w_md[2],  color='red', linetype = 3)
    
    
    return(p)
    
}

get_tuning_curves <- function(tr_data, tuning_data){
    # makes the error and margin curves
    
    # get cost upper and lower bouds
    C_lbd <- get_small_C_cutoff(tr_data)
    C_ubd <- get_large_C_cutoff(tr_data)
    
    
    # make results data frame
    cv_err <- unwrap_list(tuning_data[['svm']], 'cv_err')
    tr_err <- unwrap_list(tuning_data[['svm']], 'tr_err')
    tst_err <- unwrap_list(tuning_data[['svm']], 'tst_err')
    margin <- unwrap_list(tuning_data[['svm']], 'margin')
    angle_svm_mdp <- unwrap_list(tuning_data[['svm']], 'angle_svm_mdp')
    angle_svm_md <- unwrap_list(tuning_data[['svm']], 'angle_svm_md')
    
    results <- tibble(cost=cost_vals,
                      cv_err=cv_err,
                      tr_err=tr_err,
                      tst_err=tst_err,
                      margin=margin,
                      angle_svm_md=angle_svm_md,
                      angle_svm_mdp=angle_svm_mdp)
    
    
    
    # make error curves
    error_curve <- results %>% 
        rename(tr=tr_err, cv=cv_err, tst=tst_err) %>%
        dplyr::select(tr, cv, tst, cost) %>%
        gather(key=type, value=error, tr, cv, tst) %>% 
        ggplot() +
        geom_point(aes(x=log10(cost), y=error, color=type)) + 
        geom_line(aes(x=log10(cost), y=error, color=type)) +
        geom_vline(xintercept=log10(C_lbd), linetype=2) +
        geom_vline(xintercept=log10(C_ubd), linetype=2)
    
    
    # plot margin curve
    margin_curve <-  ggplot(data=results) +
        geom_point(aes(x=log10(cost), y=margin)) + 
        geom_line(aes(x=log10(cost), y=margin)) +
        geom_vline(xintercept=log10(C_lbd), linetype=2) +
        geom_vline(xintercept=log10(C_ubd), linetype=2)
    
    
    list(error = error_curve,
         margin=margin_curve)
}




