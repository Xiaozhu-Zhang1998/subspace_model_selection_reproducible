rm(list = ls())
source("0_functions.R")
library(tidyverse)
library(patchwork)
library(dichromat)


# l0 base procedure =====
# read in the simulation results (collected from 2_synthetic_l0.R)
Results = readRDS("RS_Syn_l0.RDS")
Results = Results %>%
  mutate(
    q_CSS_wavg = q_CSS,
    q_CSS_savg = q_CSS,
  ) %>%
  rename(q_CSS_sps = q_CSS) %>%
  select(-ends_with("_CSS_savg"), -ends_with("_CSS_wavg")) 


## Left column of Table 1 =====
mse.min = Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("q"), -starts_with("inter"), -starts_with("stab"), -starts_with("FD"), -starts_with("PW")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "mse") %>%
  group_by(s0, method) %>%
  summarise(mse = mean(mse)) %>%
  mutate(
    Method = str_remove(method, "mse_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("Lasso", "CSS (wavg)", "CSS (savg)" ))) %>%
  group_by(Method) %>%
  summarise(mse.min = min(mse)) %>%
  deframe()


# s0.cv
s0.cv = Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("q"), -starts_with("inter"), -starts_with("stab"), -starts_with("FD"), -starts_with("PW")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "mse") %>%
  group_by(s0, method) %>%
  summarise(mse = mean(mse)) %>%
  mutate(
    Method = str_remove(method, "mse_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("Lasso", "CSS (wavg)", "CSS (savg)" ))) %>%
  filter((Method == "L0" & mse == mse.min["L0"]) | (Method == "SS" & mse == mse.min["SS"]) | (Method == "CSS (sps)" & mse == mse.min["CSS (sps)"]) | (Method == "FSSS" & mse == mse.min["FSSS"]) ) %>%
  select(Method,s0) %>%
  deframe()

# mse.cv 
Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("q"), -starts_with("inter"), -starts_with("stab"), -starts_with("FD"), -starts_with("PW")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "mse") %>%
  group_by(s0, method) %>%
  summarise(sd = sd(mse), mse = mean(mse)) %>%
  mutate(
    Method = str_remove(method, "mse_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("Lasso", "CSS (wavg)", "CSS (savg)" ))) %>%
  filter((Method == "L0" & s0 == s0.cv["L0"]) | (Method == "SS" & s0 == s0.cv["SS"]) | (Method == "CSS (sps)" & s0 == s0.cv["CSS (sps)"]) | (Method == "FSSS" & s0 == s0.cv["FSSS"]) ) 


# FD.cv
Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("mse"), -starts_with("q"), -starts_with("inter"), -starts_with("stab"), -starts_with("PW")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "FD") %>%
  group_by(s0, method) %>%
  summarise(sd = sd(FD), FD = mean(FD))  %>%
  mutate(
    Method = str_remove(method, "FD_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("Lasso", "CSS (wavg)", "CSS (savg)" ))) %>%
  filter((Method == "L0" & s0 == s0.cv["L0"]) | (Method == "SS" & s0 == s0.cv["SS"]) | (Method == "CSS (sps)" & s0 == s0.cv["CSS (sps)"]) | (Method == "FSSS" & s0 == s0.cv["FSSS"]) ) 


# TP.cv 
Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("mse"), -starts_with("q"), -starts_with("inter"), -starts_with("stab"), -starts_with("FD")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "PW") %>%
  group_by(s0, method) %>%
  summarise(sd = sd(PW), PW = mean(PW)) %>%
  mutate(
    Method = str_remove(method, "PW_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("Lasso", "CSS (wavg)", "CSS (savg)" ))) %>%
  filter((Method == "L0" & s0 == s0.cv["L0"]) | (Method == "SS" & s0 == s0.cv["SS"]) | (Method == "CSS (sps)" & s0 == s0.cv["CSS (sps)"]) | (Method == "FSSS" & s0 == s0.cv["FSSS"]) ) 

# robust.cv
Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("mse"), -starts_with("q"), -starts_with("inter"), -starts_with("FD"), -starts_with("PW")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "stab") %>%
  group_by(s0, method) %>%
  summarise(sd = sd(stab), stab = mean(stab)) %>%
  mutate(
    Method = str_remove(method, "stab_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("Lasso", "CSS (wavg)", "CSS (savg)" ))) %>%
  filter((Method == "L0" & s0 == s0.cv["L0"]) | (Method == "SS" & s0 == s0.cv["SS"]) | (Method == "CSS (sps)" & s0 == s0.cv["CSS (sps)"]) | (Method == "FSSS" & s0 == s0.cv["FSSS"]) ) 



# lasso base procedure ====
# read in the simulation results (collected from 2_synthetic_lasso.R)
Results = readRDS("RS_Syn_lasso.RDS")
Results = Results %>%
  mutate(
    q_CSS_wavg = q_CSS,
    q_CSS_savg = q_CSS,
  ) %>%
  rename(q_CSS_sps = q_CSS) %>%
  select(-ends_with("_CSS_savg"), -ends_with("_CSS_wavg")) 


## Right column of Table 1 ====
mse.min = Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("q"), -starts_with("inter"), -starts_with("stab"), -starts_with("FD"), -starts_with("PW")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "mse") %>%
  group_by(s0, method) %>%
  summarise(mse = mean(mse)) %>%
  mutate(
    Method = str_remove(method, "mse_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("L0", "CSS (wavg)", "CSS (savg)" ))) %>%
  group_by(Method) %>%
  summarise(mse.min = min(mse)) %>%
  deframe()

# s0.cv
s0.cv = Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("q"), -starts_with("inter"), -starts_with("stab"), -starts_with("FD"), -starts_with("PW")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "mse") %>%
  group_by(s0, method) %>%
  summarise(mse = mean(mse)) %>%
  mutate(
    Method = str_remove(method, "mse_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("L0", "CSS (wavg)", "CSS (savg)" ))) %>%
  filter((Method == "Lasso" & mse == mse.min["Lasso"]) | (Method == "SS" & mse == mse.min["SS"]) | (Method == "CSS (sps)" & mse == mse.min["CSS (sps)"]) | (Method == "FSSS" & mse == mse.min["FSSS"]) ) %>%
  select(Method, s0) %>%
  deframe()

# mse.cv 
Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("q"), -starts_with("inter"), -starts_with("stab"), -starts_with("FD"), -starts_with("PW")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "mse") %>%
  group_by(s0, method) %>%
  summarise(sd = sd(mse), mse = mean(mse)) %>%
  mutate(
    Method = str_remove(method, "mse_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("L0", "CSS (wavg)", "CSS (savg)" ))) %>%
  filter((Method == "Lasso" & s0 == s0.cv["Lasso"]) | (Method == "SS" & s0 == s0.cv["SS"]) | (Method == "CSS (sps)" & s0 == s0.cv["CSS (sps)"]) | (Method == "FSSS" & s0 == s0.cv["FSSS"]) ) 


# FD.cv
Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("mse"), -starts_with("q"), -starts_with("inter"), -starts_with("stab"), -starts_with("PW")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "FD") %>%
  group_by(s0, method) %>%
  summarise(sd = sd(FD), FD = mean(FD))  %>%
  mutate(
    Method = str_remove(method, "FD_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("L0", "CSS (wavg)", "CSS (savg)" ))) %>%
  filter((Method == "Lasso" & s0 == s0.cv["Lasso"]) | (Method == "SS" & s0 == s0.cv["SS"]) | (Method == "CSS (sps)" & s0 == s0.cv["CSS (sps)"]) | (Method == "FSSS" & s0 == s0.cv["FSSS"]) ) 


# TP.cv 
Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("mse"), -starts_with("q"), -starts_with("inter"), -starts_with("stab"), -starts_with("FD")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "PW") %>%
  group_by(s0, method) %>%
  summarise(sd = sd(PW), PW = mean(PW)) %>%
  mutate(
    Method = str_remove(method, "PW_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("L0", "CSS (wavg)", "CSS (savg)" ))) %>%
  filter((Method == "Lasso" & s0 == s0.cv["Lasso"]) | (Method == "SS" & s0 == s0.cv["SS"]) | (Method == "CSS (sps)" & s0 == s0.cv["CSS (sps)"]) | (Method == "FSSS" & s0 == s0.cv["FSSS"]) ) 

# robust.cv
Results %>%
  select(-starts_with("alph"), -cutoff, -starts_with("mse"), -starts_with("q"), -starts_with("inter"), -starts_with("FD"), -starts_with("PW")) %>%
  pivot_longer(!c("s0", "l"), names_to = "method", values_to = "stab") %>%
  group_by(s0, method) %>%
  summarise(sd = sd(stab), stab = mean(stab)) %>%
  mutate(
    Method = str_remove(method, "stab_"),
    Method = factor(Method, levels = c("L0", "Lasso", "SS", "CSS_wavg", "CSS_savg", "CSS_sps", "FSSS"), labels = c("L0", "Lasso", "SS", "CSS (wavg)", "CSS (savg)", "CSS (sps)", "FSSS"))
  ) %>%
  filter(!(Method %in% c("L0", "CSS (wavg)", "CSS (savg)" ))) %>%
  filter((Method == "Lasso" & s0 == s0.cv["Lasso"]) | (Method == "SS" & s0 == s0.cv["SS"]) | (Method == "CSS (sps)" & s0 == s0.cv["CSS (sps)"]) | (Method == "FSSS" & s0 == s0.cv["FSSS"]) ) 




# Focus on one dataset and implement FSSS =====
library(tidyverse)
library(patchwork)
library(dichromat)

## setup =====
synData <- readRDS("synData.RDS")
X = synData$X
y = synData$y
beta = synData$beta

X = X - rep(1, nrow(X)) %*% t(colMeans(X))
X = X / ( rep(1, nrow(X)) %*% t(apply(X, 2, sd)) )
y = (y - mean(y)) / sd(y)

X = X[1:600,]
y = y[1:600]

# subsampling and selection
s0 = 35
alpha = 0.7
bags = l0_subsampling(X, y, s0, num_bags = 100)
Selection_set = all_path_FSSS(X, 100, alpha, bags)

interest_sets = list(
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(10, 11), 
  c(10, 12), 
  c(11, 12),
  c(13, 14, 15),
  c(14, 15, 16),
  c(13, 16),
  c(17, 18, 19, 20),
  c(18, 19, 20, 21),
  c(17, 18, 21),
  c(19, 21)
)

set.levels = sapply(interest_sets, function(x) {
  paste0(unlist(x), collapse = "|")
})

combidx_set = combinat::combn(1:length(interest_sets), 2)

RS = matrix(0, nrow = ncol(combidx_set), ncol = 6)
for(i in 1:ncol(combidx_set)) {
  zoom = combidx_set[,i]
  S1 = interest_sets[[zoom[1]]]
  S2 = interest_sets[[zoom[2]]]
  joint_supp = support(X, union(S1, S2), bags$base_lst)
  if(joint_supp >= alpha){
    next
  }
  rs = rep(0, 6)
  rs[1] = set.levels[zoom[1]]
  rs[2] = set.levels[zoom[2]]
  rs[3] = support(X, S1, bags$base_lst)
  rs[4] = support(X, S2, bags$base_lst)
  prod_proj = proj(X[,S1]) %*% proj(X[,S2])
  L = min(length(S1), length(S2))
  rs[5] = tr(prod_proj) / L
  L = max(length(S1), length(S2))
  rs[6] = RSpectra::svds(prod_proj, L, 0, 0)$d[L]^2
  RS[i, ]= rs
  cat("finished", i, "\n")
}


tab = RS %>%
  data.frame() %>%
  `colnames<-`( 
    c("set1", "set2", "stab1", "stab2", "tr_L", "sigma_L") ) %>%
  mutate(
    across(c(stab1, stab2, tr_L, sigma_L), as.numeric)
  )

## order by dendrogram 
d = length(set.levels)
subs.mat = matrix(0, nrow = d, ncol = d)
for(i in 1:d) {
  for(j in 1:d) {
    rs = 1 - tab$sigma_L[ tab$set1 == set.levels[i] & tab$set2 == set.levels[j] ]
    if(length(rs) == 0) {
      rs = 1 - tab$sigma_L[ tab$set2 == set.levels[i] & tab$set1 == set.levels[j]]
      if(length(rs) == 0) {
        rs = 1
      }
    }
    subs.mat[i,j] = rs[1]
  }
  cat("finished", i, "out of", d, '\n')
}
colnames(subs.mat) = set.levels
rownames(subs.mat) = set.levels
hc <- hclust(as.dist(subs.mat), method = "single")
fixed.set = hc$labels[hc$order]


## Figure 2 ====
tab1 = tab
tab1$set1 = tab$set2
tab1$set2 = tab$set1
tab1 = rbind(tab, tab1)

tritanopia_colors <- dichromat(c("white", "#D2FFFE", "#62c4f5"), type = "tritan")
combidx = combinat::combn(fixed.set, 2)
upper_tri = rbind(t(combidx), cbind(fixed.set, fixed.set)) %>%
  data.frame() %>%
  `colnames<-`(c("set1", "set2")) %>%
  arrange(factor(set1, levels = fixed.set), 
          factor(set2, levels = fixed.set)) %>% 
  left_join(tab1) %>%
  mutate(set1 = fct_inorder( set1 ), set2 = fct_inorder( set2 ),
         text = ifelse(is.na(tr_L), "", round(tr_L, 4)), 
         value = tr_L) 

lower_tri = upper_tri %>%
  mutate(set.temp = set1, set1 = set2, set2 = set.temp) %>%
  select(-set.temp) %>%
  arrange(factor(set1, levels = fixed.set), 
          factor(set2, levels = fixed.set)) %>%
  mutate(text = ifelse(is.na(sigma_L), "", round(sigma_L, 4) ),
         value = sigma_L)

all_tri = rbind(upper_tri, lower_tri) 

p1 = all_tri %>%
  ggplot(aes(x = set1, y = set2, fill = value)) +
  geom_tile(color = "white") +  # white border
  geom_tile(data = all_tri[all_tri$set1 == all_tri$set2, ], color = "lightgrey", fill = "#525354") + # diagonal tile
  # geom_tile(data = all_tri[all_tri$nabla_value >= 0.5 & all_tri$subs_u > 0.8, ] %>% drop_na(), color = "red", lwd = 0.5) + # red border
  geom_text(aes(label = text), color = "black", size = 2.5) +  # Add values
  scale_fill_gradientn(colors = tritanopia_colors, na.value = "white") +
  annotate("text", x = c("3|6|9", "18|19|20|21"), y = c("18|19|20|21", "3|6|9"), label = "Stable together", size = 3) +
  labs(fill = "Upper tri: normalized similarity\nLower tri: cosine squared of the largest principal angle", x = "Subsets of interest", y = "\n\n\n") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8),  # Rotate x-axis labels
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8), legend.title = element_text(size = 9, angle = 90), legend.position = "right",
        axis.title = element_text(size = 9)
  )
p1 


