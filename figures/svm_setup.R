library(tidyverse)

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



# generate training and test data
data <- dist(n_pos=10,
             n_neg=10,
             seed=2345)


# cost value
cost <- 1

# fit svm
svmfit <- svm_model(data,cost=cost)



# make plot data ---------------------------------------------------------------


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




# plot options ------------------------------------------------------------
point_size <- 3

# square box that contains all the data
lim <- 1.5 *max(data_svm %>% 
                    dplyr::select(x1, x2) %>% 
                    summarise(x1=max(abs(x1)), x2=max(abs(x2))))

# make plot ---------------------------------------------------------------


# plot svm
ggplot(data=data_svm) +
    geom_point(aes(x=x1, y=x2, color=y, alpha=sup_vec, shape=margin_vec), size=point_size) +
    geom_abline(slope=-w_svm[1]/w_svm[2], intercept = b_svm/w_svm[2]) +
    geom_abline(slope=-w_svm[1]/w_svm[2], intercept = (b_svm + 1)/w_svm[2], linetype = 2)+
    geom_abline(slope=-w_svm[1]/w_svm[2], intercept = (b_svm - 1)/w_svm[2], linetype = 2)


