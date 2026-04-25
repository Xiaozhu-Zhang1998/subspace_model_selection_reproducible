rm(list = ls())
source("0_functions.R")
library(tidyverse)
library(latex2exp)
library(patchwork)
library(ggdendro)

# Figure 3 ========
# read in the results (collected from 3_realdata_l0.R)
Results = readRDS("RS_realdata.RDS") %>%
  mutate(
    q_CSS_wavg = q_CSS,
    q_CSS_savg = q_CSS,
  ) %>%
  rename(q_CSS_sps = q_CSS) %>%
  select(!ends_with(c("_Lasso", "_CSS_wavg", "_CSS_savg"))) 

mse.tab = Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("q"), -starts_with("inter"), -starts_with("stab")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "mse") %>%
  group_by(s0, method) %>%
  summarise(mse = mean(mse)) %>%
  mutate(
    Method = str_remove(method, "mse_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) 

q.tab = Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("mse"), -starts_with("inter"), -starts_with("stab")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "q") %>%
  group_by(s0, method) %>%
  summarise(q = mean(q, na.rm = T)) %>%
  mutate(
    Method = str_remove(method, "q_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) 


stab.tab = Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("mse"), -starts_with("q"), -starts_with("inter")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "stab") %>%
  group_by(s0, method) %>%
  summarise(stab = mean(stab, na.rm = T)) %>%
  mutate(
    Method = str_remove(method, "stab_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) 

pointsize = 1
p1 = mse.tab %>%
  select(s0, Method, mse) %>%
  left_join(stab.tab) %>%
  ggplot(aes(x = mse, y = stab, col = Method, shape = Method)) +
  geom_point(size = pointsize) +
  theme_bw() +
  labs(x = "Test MSE", y = "Output stability")  

p2 = mse.tab %>%
  select(s0, Method, mse) %>%
  left_join(q.tab) %>%
  ggplot(aes(x = q, y = mse, col = Method, shape = Method)) +
  geom_point(size = pointsize) +
  theme_bw() +
  labs(x = "Number of selected features", y = "Test MSE")

p3 = stab.tab %>%
  select(s0, Method, stab) %>%
  left_join(q.tab) %>%
  ggplot(aes(x = q, y = stab, col = Method, shape = Method)) +
  geom_point(size = pointsize) +
  theme_bw() +
  labs(x = "Number of selected features", y = "Output stability")

fontsize = 8
p1 + p2 + p3 +
  plot_layout(ncol = 3, guides = "collect") & 
  theme(legend.position = "bottom", legend.title=element_text(size=fontsize), legend.text = element_text(size=fontsize), 
        axis.title = element_text(size=fontsize), plot.title = element_text(size=fontsize),
        axis.text.x = element_text(size=fontsize), axis.text.y = element_text(size=fontsize),
  )




# focus on the dataset ========
# readin data
data2990 <- readRDS("data2990.RDS")
X = as.matrix(data2990$X)
y = data2990$y
gename = data2990$gename


X = X - rep(1, nrow(X)) %*% t(colMeans(X))
X = X / ( rep(1, nrow(X)) %*% t(apply(X, 2, sd)) )
y = (y - mean(y)) / sd(y)


# selection
s0 = 50
alpha = 0.7
set.seed(1234)
bags = l0_subsampling(X, y, s0, num_bags = 100)
Selection_set = all_path_FSSS(X, 50, alpha, bags)
Selection_set = Selection_set[1:50]
Stab = sapply(1:50, function(i) {
  compute_pi(Selection_set[[i]], X, bags)
})

# you may use this line to get the selection set we used:
# Selection_set_obj = readRDS("realdata_Selection_set.RDS")
# Selection_set = Selection_set_obj$Selection_set

