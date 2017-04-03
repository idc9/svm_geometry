make_2d_svm_tuning_plot <- function(data, cost_vals){
    ##############
    # viz options#
    ##############
    margin_alphas <- c(.3, 1)
    names(margin_alphas) <- c(TRUE, FALSE)
    
    class_support_fill <- c('blue','white', 'red', 'white')
    names(class_support_fill) <- c('1TRUE', '1FALSE', '-1TRUE', '-1FALSE')
    
    class_colors <- c('red', 'blue')
    names(class_colors) <- c(-1, 1)
    
    class_shapes <- c(21, 22)
    names(class_shapes) <- c(-1, 1)
    
    point_size <- 6
    
    
    #########################
    # fit svm and make plots#
    #########################
    
    plts <- list()
    for(i in 1:length(cost_vals)){
        cost <- cost_vals[i]
        
        svmfit <- svm_model(data, cost=cost)
         
        
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
        
        # square box that contains all the data
        lim <- 1.5 *max(data_svm %>% 
                       dplyr::select(x1, x2) %>% 
                       summarise(x1=max(abs(x1)), x2=max(abs(x2))))
        # support * class
        data_svm <- data_svm %>% 
            mutate(class_supp=str_c(y,sup_vec))
        # plot svm
        p <- ggplot(data=data_svm) +
            geom_point(aes(x=x1, y=x2, color=y, shape=y, fill=class_supp, alpha=margin_vec), size=point_size) +
            geom_abline(slope=-w_svm[1]/w_svm[2], intercept = b_svm/w_svm[2]) +
            geom_abline(slope=-w_svm[1]/w_svm[2], intercept = (b_svm + 1)/w_svm[2], linetype = 2) +
            geom_abline(slope=-w_svm[1]/w_svm[2], intercept = (b_svm - 1)/w_svm[2], linetype = 2) + 
            xlim(-lim, lim) + 
            ylim(-lim, lim) + 
            ggtitle(paste0('SVM fit for C = ', cost)) + 
            labs(x = "x", y = "y") + 
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(size = 20),
                  legend.text = element_text(size = 20),
                  legend.position = c(.85, .85),
                  legend.key = element_rect(fill = "white")) +
            scale_shape_manual(values=class_shapes) +
            scale_color_manual(values=class_colors) +
            scale_alpha_manual(values=margin_alphas) +
            scale_fill_manual(values=class_support_fill) +
            guides(alpha=FALSE, fill=FALSE)

        
        plts[[i]] <- p

    }       
    return(plts)
}


make_tuning_curves <- function(tr_data, tuning_data){
    # makes the error, margin and angle curves
    
    ################
    # plot options #
    ################
    axis_label_text_size <- 30
    point_size <- 5
    axis_text_size <- 12
    point_alpha <- .8
    
    ###############
    # set up data #
    ###############
    
    # get cost upper and lower bouds
    C_lbd <- get_small_C_cutoff(tr_data)
    C_ubd <- get_large_C_cutoff(tr_data)
    
    
    # make results data frame
    cost <- unwrap_list(tuning_data[['svm']], 'cost')
    cv_err <- unwrap_list(tuning_data[['svm']], 'cv_err')
    tr_err <- unwrap_list(tuning_data[['svm']], 'tr_err')
    tst_err <- unwrap_list(tuning_data[['svm']], 'tst_err')
    margin <- unwrap_list(tuning_data[['svm']], 'margin')
    angle_svm_mdp <- unwrap_list(tuning_data[['svm']], 'angle_svm_mdp')
    angle_svm_md <- unwrap_list(tuning_data[['svm']], 'angle_svm_md')
    angle_svm_hmsvm <-unwrap_list(tuning_data[['svm']], 'angle_svm_hmsvm')
    
    
    
    results <- tibble(cost=cost,
                      cv_err=cv_err,
                      tr_err=tr_err,
                      tst_err=tst_err,
                      margin=margin,
                      angle_svm_md=angle_svm_md,
                      angle_svm_mdp=angle_svm_mdp,
                      angle_svm_hmsvm=angle_svm_hmsvm)
    
    
    # tick marks for cost axis
    plt_costvals <- seq(from=floor(min(log10(cost))), to=ceiling(max(log10(cost))), by=1)
    plt_costvals_lables <- plt_costvals
    plt_costvals <- c(plt_costvals, log10(C_lbd) + .2, log10(C_ubd) + .2)
    plt_costvals_lables <- c(plt_costvals_lables, 'C_small', 'C_large')
    
    ##############
    # make plots #
    ##############
    # make error curves
    error_curve <- results %>% 
                    rename(train=tr_err, CV=cv_err, test=tst_err) %>%
                    dplyr::select(train, CV, test, cost) %>%
                    gather(key=type, value=error, train, CV, test) %>% 
                    ggplot() +
                    geom_point(aes(x=log10(cost), y=error, color=type, shape=type), size=point_size, alpha=point_alpha) + 
                    geom_line(aes(x=log10(cost), y=error, color=type, linetype=type)) +
                    geom_vline(xintercept=log10(C_lbd), linetype=3, alpha=.7) +
                    geom_vline(xintercept=log10(C_ubd), linetype=3, alpha=.7) +
                    labs(x = "log10(cost)", y = "error") + 
                    theme(panel.background = element_blank(),
                          axis.line = element_line(),
                          axis.title = element_text(size = axis_label_text_size),
                          axis.text = element_text(size = axis_text_size),
                          legend.background = element_blank(),
                          legend.margin	= unit(-0.1,"cm"),
                          legend.text = element_text(size = 20),
                          legend.position = c(.85, .85),
                          legend.key = element_rect(fill = "white")) +
                    scale_y_continuous(expand = c(0, 0), limits=c(0, .55))+
                    scale_x_continuous(breaks = plt_costvals,
                                       labels = plt_costvals_lables,
                                       limits = c(min(log10(cost)), max(log10(cost))))
    
    
    # plot margin curve
    margin_curve <- ggplot(data=results) +
                    geom_point(aes(x=log10(cost), y=log10(margin)), size=point_size) + 
                    geom_line(aes(x=log10(cost), y=log10(margin))) +
                    geom_vline(xintercept=log10(C_lbd), linetype=3, alpha=.7) +
                    geom_vline(xintercept=log10(C_ubd), linetype=3, alpha=.7) +
                    #xlim(min(log10(cost)), max(log10(cost))) + 
                    labs(x = "log10(cost)", y = "log margin width") + 
                    theme(panel.background = element_blank(),
                          axis.line = element_line(),
                          axis.title = element_text(size = axis_label_text_size),
                          axis.text = element_text(size = axis_text_size)) +
                    scale_y_continuous(expand = c(0, 0), limits=c(0, 1.1*max(log10(results$margin)))) +
                    scale_x_continuous(breaks=plt_costvals,
                                       labels=plt_costvals_lables,
                                       limits=c(min(log10(cost)), max(log10(cost))))
    
                
    
    # plot angle curve
    angle_curve <- results %>%
        rename(MD=angle_svm_md, HM_SVM=angle_svm_hmsvm) %>%
        dplyr::select(cost, MD, HM_SVM) %>%
        gather(key=type, value=angle, MD, HM_SVM) %>% 
        ggplot() +
        geom_point(aes(x=log10(cost), y=angle, color=type, shape=type), size=point_size) + 
        geom_line(aes(x=log10(cost), y=angle, color=type)) +
        geom_vline(xintercept=log10(C_lbd), linetype=3, alpha=.7) +
        geom_vline(xintercept=log10(C_ubd), linetype=3, alpha=.7) +
        # ggtitle('SVM vs. MD') + 
        labs(x = "log10(cost)", y = "angle (degrees)") + 
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              axis.title = element_text(size = axis_label_text_size),
              axis.text = element_text(size = axis_text_size),
              legend.text = element_text(size = 20),
              legend.margin	= unit(-0.1,"cm"),
              legend.position = c(.85, .85),
              legend.key = element_rect(fill = "white")) +
        scale_y_continuous(expand = c(0, 0), limits=c(0, 1.05*max(results$angle_svm_md,  results$angle_svm_hmsvm))) +
        scale_x_continuous(breaks=plt_costvals,
                           labels=plt_costvals_lables,
                           limits=c(min(log10(cost)), max(log10(cost))))

    
    
    
    
    list(error=error_curve,
         margin=margin_curve,
         angle=angle_curve)
}
