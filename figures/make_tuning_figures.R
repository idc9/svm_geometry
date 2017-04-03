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
offset <- 2
dist <- guasssian_meatball_distribution(offset, d)

# set the SVM model
svm_model <- set_svm_model()


# cross validation
cv_params <- list(seed=2344, k=5)
cost_vals <- 10^seq(-4, 1, by=.25)


# Generate data -----------------------------------------------------------



# generate training and test data
data_unbal <- dist(n_pos=21,
                   n_neg=20,
                   seed=2345)

data_bal <- slice(data_unbal, 2:n())



tst_data <- dist(n_pos=1000,
                 n_neg=1000,
                 seed=2343) 


# get cost upper and lower bouds
C_lbd <- get_small_C_cutoff(data_unbal)
C_ubd <- get_large_C_cutoff(data_unbal)


# make data plots ------------------------------------------------------


# unbalanced movie
cost_vals_to_plot <- sort(c(1e1, 1e-2, 1.5e-3),decreasing = TRUE)
data_plots_unbal <- make_2d_svm_tuning_plot(data_unbal, cost_vals_to_plot)

# balanced movie
cost_vals_to_plot <- sort(c(1e1, 1e-2, 3e-3),decreasing = TRUE)
data_plots_bal <- make_2d_svm_tuning_plot(data_bal, cost_vals_to_plot)

# save plots
for(i in 1:3){
    
    p <- data_plots_bal[[i]]
    name <- paste0('figures/bal', i, '.png')
    ggsave(name, p) 
    
    p <- data_plots_unbal[[i]]
    name <- paste0('figures/unbal', i, '.png')
    ggsave(name, p) 
}




# tuning curves -----------------------------------------------------------


# get tuning data
tuning_data_unbal <- get_svm_tuning_data(data_unbal, tst_data, svm_model, cost_vals, cv_params)
tuning_data_bal <- get_svm_tuning_data(data_bal, tst_data, svm_model, cost_vals, cv_params)


# make error and margin curve plots
tuning_curves_bal <- make_tuning_curves(data_bal, tuning_data_bal)
tuning_curves_unbal <- make_tuning_curves(data_unbal, tuning_data_unbal)



tuning_curves_bal[['margin']]
tuning_curves_bal[['error']]
tuning_curves_bal[['angle']]

# tuning_curves_unbal[['margin']]
# tuning_curves_unbal[['error']]


# save balanced plots
ggsave(paste0('figures/bal_margin.png'),
       tuning_curves_bal[['margin']]) 

ggsave(paste0('figures/bal_error.png'),
       tuning_curves_bal[['error']]) 

ggsave(paste0('figures/bal_angle.png'),
       tuning_curves_bal[['angle']]) 

# save unbalanced plots
ggsave(paste0('figures/unbal_margin.png'),
       tuning_curves_unbal[['margin']]) 

ggsave(paste0('figures/unbal_error.png'),
       tuning_curves_unbal[['error']]) 

ggsave(paste0('figures/unbal_angle.png'),
       tuning_curves_unbal[['angle']]) 


