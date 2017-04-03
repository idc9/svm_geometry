library(stringr)


data <- data_bal
cost_vals <- cost_vals_to_plot



##############
# viz options#
##############
margin_alphas <- c(.3, 1)
names(margin_alphas) <- c(TRUE, FALSE)

class_support_fill <- c('blue', 'red', 'white', 'white')
names(class_support_fill) <- c('1TRUE','-1TRUE', '1FALSE',  '-1FALSE')

class_colors <- c('red', 'blue')
names(class_colors) <- c(-1, 1)

class_shapes <- c(21, 22)
names(class_shapes) <- c(-1, 1)

point_size <- 4



#########################
# fit svm and make plots#
#########################
i <- 2


cost <- cost_vals[i]

svmfit <- svm_model(data,cost=cost)


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
ggplot(data=data_svm) +
    geom_point(aes(x=x1, y=x2, color=y, shape=y, fill=class_supp, alpha=margin_vec), size=point_size) +
    geom_abline(slope=-w_svm[1]/w_svm[2], intercept = b_svm/w_svm[2]) +
    geom_abline(slope=-w_svm[1]/w_svm[2], intercept = (b_svm + 1)/w_svm[2], linetype = 2)+
    geom_abline(slope=-w_svm[1]/w_svm[2], intercept = (b_svm - 1)/w_svm[2], linetype = 2) + 
    xlim(-lim, lim) + 
    ylim(-lim, lim) + 
    ggtitle(paste0('SVM C= ', cost)) + 
    labs(x = "x", y = "y") + 
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 20)) +
    scale_shape_manual(values=class_shapes) +
    scale_color_manual(values=class_colors) +
    scale_alpha_manual(values=margin_alphas) +
    scale_fill_manual(values=class_support_fill) +
    

guides(colour = guide_legend(override.aes = list(alpha = 1)))



ggplot(data=data_svm) +
    geom_point(aes(x=x1, y=x2, color=y, shape=y, fill=sup_vec), size=point_size) 



data_svm %>%
    add_column() %>%
    #group_by(y)
    filter(margin_vec) 
    mutate()






