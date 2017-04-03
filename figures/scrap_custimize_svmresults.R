tr_data <- data_unbal
tuning_data <- tuning_data_unbal
mdp_angle <- FALSE

axis_label_text_size <- 15

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



# angle
results %>%
    rename(md=angle_svm_md, hm_svm=angle_svm_hmsvm) %>%
    dplyr::select(cost, md, hm_svm) %>%
    gather(key=type, value=angle, md, hm_svm) %>% 
    ggplot() +
    geom_point(aes(x=log10(cost), y=angle, color=type, shape=type)) + 
    geom_line(aes(x=log10(cost), y=angle, color=type, linetype=type)) +
    geom_vline(xintercept=log10(C_lbd), linetype=3, alpha=.7) +
    geom_vline(xintercept=log10(C_ubd), linetype=3, alpha=.7) +
    ggtitle('SVM vs. MD') + 
    labs(x = "log10(cost)", y = "angle") + 
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = axis_label_text_size)) +
    scale_y_continuous(expand = c(0, 0), limits=c(0, 1.05*max(results$angle_svm_md,  results$angle_svm_hmsvm))) +
    scale_x_continuous(breaks=plt_costvals,
                       labels=plt_costvals_lables,
                       limits=c(min(log10(cost)), max(log10(cost))))

results %>%
    rename(md=angle_svm_md, hm_svm=angle_svm_hmsvm) %>%
    dplyr::select(cost, md, hm_svm) %>%
    gather(key=type, value=angle, md, hm_svm) %>% 
    ggplot() +
    geom_point(aes(x=log10(cost), y=angle, color=type)) 


# margin
results %>% 
    rename(tr=tr_err, cv=cv_err, tst=tst_err) %>%
    dplyr::select(tr, cv, tst, cost) %>%
    gather(key=type, value=error, tr, cv, tst) %>% 
    ggplot() +
    geom_point(aes(x=log10(cost), y=error, color=type, shape=type),size=3) + 
    geom_line(aes(x=log10(cost), y=error, color=type, linetype=type)) +
    geom_vline(xintercept=log10(C_lbd), linetype=3, alpha=.7) +
    geom_vline(xintercept=log10(C_ubd), linetype=3, alpha=.7) +
    #geom_segment(aes(x=log10(C_lbd), y=0, xend=log10(C_lbd), yend=.01))+
    #geom_segment(aes(x=log10(C_ubd), y=0, xend=log10(C_ubd), yend=.01))+
    #ggtitle('tuning error curve') + 
    labs(x = "log10(cost)", y = "error") + 
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 15)) +
    scale_y_continuous(expand = c(0, 0), 
                       limits=c(0, .55))+
    scale_x_continuous(breaks=plt_costvals,
                       labels=plt_costvals_lables,
                       limits=c(min(log10(cost)), max(log10(cost))))

