
library(tidyverse)
library(stringr)


source('code/linear_classifiers.R')
source('code/svm_fun.R')
source('code/help_fun.R')
source('code/toy_distributions_fun.R')
source('code/svm_movies.R')

source('figures/presentation_fun.R')


# setup -------------------------------------------------------------------


# set data generating distribution
d <- 2
offset <- 1
dist <- guasssian_meatball_distribution(offset, d)

# set the SVM model
svm_model <- set_svm_model()

# generate data
data <- dist(n_pos=21,
             n_neg=20,
             seed=2364)





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

point_size <- 4


#########################
# fit svm and make plots#
#########################
cost <- 100

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
lim <- 1.2 *max(data_svm %>% 
                    dplyr::select(x1, x2) %>% 
                    summarise(x1=max(abs(x1)), x2=max(abs(x2))))

# support * class
data_svm <- data_svm %>% 
    mutate(class_supp=str_c(y,sup_vec))

# plot svm
p <- ggplot(data=data_svm) +
    geom_point(aes(x=x1, y=x2, color=y, shape=y, fill=class_supp, alpha=margin_vec), size=point_size) +
    geom_abline(slope=-w_svm[1]/w_svm[2], intercept = b_svm/w_svm[2]) +
    geom_abline(slope=-w_svm[1]/w_svm[2], intercept = (b_svm + 1)/w_svm[2], linetype = 2)+
    geom_abline(slope=-w_svm[1]/w_svm[2], intercept = (b_svm - 1)/w_svm[2], linetype = 2) + 
    xlim(-lim, lim) + 
    ylim(-lim, lim) + 
    # ggtitle('Soft margin SVM') + 
    labs(x = "x", y = "y") + 
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 20)) +
    scale_shape_manual(values=class_shapes) +
    scale_color_manual(values=class_colors) +
    scale_alpha_manual(values=margin_alphas) +
    scale_fill_manual(values=class_support_fill) +
    guides(alpha=FALSE, fill=FALSE) +
    annotate("text", x = -3.5, y = -4, label = "margin") +
    annotate("text", x = 3.5, y = -4, label = "margin") +
    annotate("text", x = .1, y = 4, label = "separating hyperplane") +
    annotate("text", x = 0, y = -1, label = 'normal vector') +
    geom_segment(aes(x =0, y=b_svm, xend = 2 , yend = 0),
                 arrow = arrow(length = unit(0.8,"cm")))





ggsave('figures/soft_setup.JPG', p) 


