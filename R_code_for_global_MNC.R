setwd("")

#####test for normality and homogeneity of variance#####
library(car)
total.dat <- read.csv("agro_natural_analysis_data_0-20.csv")        #read database in .csv files
###Shapiro-Wilk test
shapiro_F_necromass_C_B_necromass_C <- tapply(total.dat$F_necromass_C_B_necromass_C, total.dat$Habitats, shapiro.test)
shapiro_F_necromass_C_B_necromass_C
shapiro_F_necromass_C_B_necromass_C$'Agroecosystem'$p.value
shapiro_F_necromass_C_B_necromass_C$'Natural_ecosystem'$p.value 

shapiro_F_necromass_C_SOC <- tapply(total.dat$F_necromass_C_SOC, total.dat$Habitats, shapiro.test)
shapiro_F_necromass_C_SOC
shapiro_F_necromass_C_SOC$'Agroecosystem'$p.value
shapiro_F_necromass_C_SOC$'Natural_ecosystem'$p.value 

shapiro_B_necromass_C_SOC <- tapply(total.dat$B_necromass_C_SOC, total.dat$Habitats, shapiro.test)
shapiro_B_necromass_C_SOC
shapiro_B_necromass_C_SOC$'Agroecosystem'$p.value
shapiro_B_necromass_C_SOC$'Natural_ecosystem'$p.value

###leveneTest
leveneTest(F_necromass_C_B_necromass_C~Habitats, data = total.dat)
leveneTest(F_necromass_C_SOC~Habitats, data = total.dat)
leveneTest(B_necromass_C_SOC~Habitats, data = total.dat)

#####Figure 1#####
library(ggplot2)
library(ggnewscale)
No_plob_map_world <- read.csv('point.csv', header = 1,row.names = 1)
world <- map_data("world")
ggplot() +
  geom_map(data = world, map = world, aes(x=long, y=lat, map_id = region),color = "#d3d3d3", fill = "#d3d3d3", size = 0.2) +theme_void()+
  geom_hline(yintercept = 0, col = "gray90")+
  geom_point( data = No_plob_map_world,aes(x=long, y=lat,color =Habitats, fill =Habitats, shape=Habitats),  
              size = 2, alpha = 1) +
  scale_color_manual(values = c('#006064','#dd2c00')) + 
  scale_fill_manual(values = c('#006064','#dd2c00'))+
  new_scale_color()+  new_scale_fill() +
  scale_shape_manual(values = c(21,24))
  
#####Figure 2#####
#nonparametric test——Wilcoxon rank sum test
wilcox_F_necromass_C_B_necromass_C <- wilcox.test(F_necromass_C_B_necromass_C~Habitats, total.dat, paired = FALSE, alternative = 'two.sided')
wilcox_F_necromass_C_B_necromass_C
wilcox_F_necromass_C_B_necromass_C$p.value

wilcox_F_necromass_C_SOC <- wilcox.test(F_necromass_C_SOC~Habitats, total.dat, paired = FALSE, alternative = 'two.sided')
wilcox_F_necromass_C_SOC
wilcox_F_necromass_C_SOC$p.value

wilcox_B_necromass_C_SOC <- wilcox.test(B_necromass_C_SOC~Habitats, total.dat, paired = FALSE, alternative = 'two.sided')
wilcox_B_necromass_C_SOC
wilcox_B_necromass_C_SOC$p.value

library(ggplot2)
library(ggsci)
library(ggpubr)
library(tidyverse)
library(cowplot)

total.dat %>% ggboxplot( x="Habitats", y="F_necromass_C_B_necromass_C", color = "Habitats", bxp.errorbar = T, 
                          outlier.shape = NA, palette = c("#d4a345" ,"#467e46") ,alpha = 0.7,add = "jitter", shape=1 ,size = 1.5)+
  theme_pubr(base_size = 20)+theme_test(base_size = 20)+
  theme(legend.position = "none")+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.background = element_rect(fill = "white")) +labs(x = "Habitats", y = "F_necromass_C_B_necromass_C")+
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0))+
  theme(axis.text = element_text(face = "bold")) +labs(x = NULL)+ylim(c(0,15)) 

total.dat %>% ggboxplot( x="Habitats", y="F_necromass_C_SOC", color = "Habitats", bxp.errorbar = T, 
                         outlier.shape = NA, palette = c("#d4a345" ,"#467e46") ,alpha = 0.7,add = "jitter", shape=1 ,size = 1.5)+
  theme_pubr(base_size = 20)+theme_test(base_size = 20)+
  theme(legend.position = "none")+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.background = element_rect(fill = "white")) +labs(x = "Habitats", y = "F_necromass_C_SOC")+
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0))+
  theme(axis.text = element_text(face = "bold")) +labs(x = NULL)+ylim(c(0,100)) 


total.dat %>% ggboxplot( x="Habitats", y="B_necromass_C_SOC", color = "Habitats", bxp.errorbar = T, 
                         outlier.shape = NA, palette = c("#d4a345" ,"#467e46") ,alpha = 0.7,add = "jitter", shape=1 ,size = 1.5)+
  theme_pubr(base_size = 20)+theme_test(base_size = 20)+
  theme(legend.position = "none")+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.background = element_rect(fill = "white")) +labs(x = "Habitats", y = "B_necromass_C_SOC")+
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0))+
  theme(axis.text = element_text(face = "bold")) +labs(x = NULL)+ylim(c(0,100))

#####Figure 3#####
agro_FB_necromass.dat <- read.csv("agro_FB_necromass_0-20.csv")         
natural_FB_necromass.dat <- read.csv("natural_FB_necromass_0-20.csv") 
agro_B_necromass.dat <- read.csv("agro_B_necromass_0-20.csv") 
agro_F_necromass.dat <- read.csv("agro_F_necromass_0-20.csv") 
natural_B_necromass.dat <- read.csv("natural_B_necromass_0-20.csv") 
natural_F_necromass.dat <- read.csv("natural_F_necromass_0-20.csv")

library(vegan)
library(tidyverse)
mod1 <- varpart(agro_FB_necromass.dat[,1], agro_FB_necromass.dat[,2:4], agro_FB_necromass.dat[,5:6], agro_FB_necromass.dat[,7:12], agro_FB_necromass.dat[,13:17])
mod1
plot(mod1, bg=2:5)

mod2 <- varpart(natural_FB_necromass.dat[,1], natural_FB_necromass.dat[,2:4], natural_FB_necromass.dat[,5:6], natural_FB_necromass.dat[,7:12], natural_FB_necromass.dat[,13:17], data=natural_FB_necromass.dat)
mod2
plot(mod2, bg=2:5)

mod5 <- varpart(agro_F_necromass.dat[,1], agro_F_necromass.dat[,2:4], agro_F_necromass.dat[,5:6], agro_F_necromass.dat[,7:12], agro_F_necromass.dat[,13:17], data=agro_F_necromass.dat)
mod5
plot(mod5, bg=2:5)

mod6 <- varpart(natural_F_necromass.dat[,1], natural_F_necromass.dat[,2:4], natural_F_necromass.dat[,5:6], natural_F_necromass.dat[,7:12], natural_F_necromass.dat[,13:17], data=natural_F_necromass.dat)
mod6
plot(mod6, bg=2:5)

mod7 <- varpart(agro_B_necromass.dat[,1], agro_B_necromass.dat[,2:4], agro_B_necromass.dat[,5:6], agro_B_necromass.dat[,7:12], agro_B_necromass.dat[,13:17], data=agro_B_necromass.dat)
mod7
plot(mod7, bg=2:5)

mod8 <- varpart(natural_B_necromass.dat[,1], natural_B_necromass.dat[,2:4], natural_B_necromass.dat[,5:6], natural_B_necromass.dat[,7:12], natural_B_necromass.dat[,13:17], data=natural_B_necromass.dat)
mod8
plot(mod8, bg=2:5)

#####Figure 4#####
library(gbm)

#FB
#agro
agro_FB_normalized_1.dat <- scale(agro_FB_necromass.dat)
colnames(agro_FB_normalized_1.dat) <- c("F_necromass_C_B_necromass_C","Longitude","Elevation","MAT","MAP","Clay","pH","SOC","SOC_TN","BGBC","NPP","MBC","MBC_MBN") 
agro_FB_normalized_1.dat <- as.data.frame(agro_FB_normalized_1.dat)

set.seed(123)
agro_FB_split<- sample(nrow(agro_FB_normalized_1.dat), nrow(agro_FB_normalized_1.dat)*0.8)
agro_FB_train <- agro_FB_normalized_1.dat[agro_FB_split, ]
agro_FB_test <- agro_FB_normalized_1.dat[-agro_FB_split, ]

set.seed(123)
brt_agro_FB <- gbm(F_necromass_C_B_necromass_C ~., data = agro_FB_train, distribution = "gaussian", n.trees = 9999, interaction.depth = 9, shrinkage = 0.001, bag.fraction = 0.5, cv.folds = 10)
print(brt_agro_FB)
summary(brt_agro_FB)

perf_agro_FB <- gbm.perf(brt_agro_FB,method = "cv")
perf_agro_FB
agro_FB_test$F_necromass_C_B_necromass_C_pred_1 <- predict(brt_agro_FB, agro_FB_test)
plot(agro_FB_test$F_necromass_C_B_necromass_C_pred_1, agro_FB_test$F_necromass_C_B_necromass_C, main = 'Test dataset', xlab = 'Predicted data', ylab = 'Observed data')
abline(a=0,b=1)
fitline_agro_FB <- lm(F_necromass_C_B_necromass_C~F_necromass_C_B_necromass_C_pred_1, agro_FB_test)
abline(fitline_agro_FB,lty=2)
summary(fitline_agro_FB)

#FB
#natural
natural_FB_normalized_1.dat <- scale(natural_FB_necromass.dat) 
colnames(natural_FB_normalized_1.dat) <- c("F_necromass_C_B_necromass_C","Longitude","Elevation","MAT","MAP","Clay","pH","SOC","SOC_TN","BGBC","NPP","MBC","MBC_MBN") 
natural_FB_normalized_1.dat <- as.data.frame(natural_FB_normalized_1.dat)

set.seed(123)
natural_FB_split<- sample(nrow(natural_FB_normalized_1.dat), nrow(natural_FB_normalized_1.dat)*0.8)
natural_FB_train <- natural_FB_normalized_1.dat[natural_FB_split, ]
natural_FB_test <- natural_FB_normalized_1.dat[-natural_FB_split, ]

set.seed(123)
brt_natural_FB <- gbm(F_necromass_C_B_necromass_C ~., data = natural_FB_train, distribution = "gaussian", n.trees = 9999, interaction.depth = 9, shrinkage = 0.001, bag.fraction = 0.5, cv.folds = 10)
print(brt_natural_FB)
summary(brt_natural_FB)

perf_natural_FB <- gbm.perf(brt_natural_FB,method = "cv")
perf_natural_FB
natural_FB_test$F_necromass_C_B_necromass_C_pred_1 <- predict(brt_natural_FB, natural_FB_test)
plot(natural_FB_test$F_necromass_C_B_necromass_C_pred_1, natural_FB_test$F_necromass_C_B_necromass_C, main = 'Test dataset', xlab = 'Predicted data', ylab = 'Observed data')
abline(a=0,b=1)
fitline_natural_FB <- lm(F_necromass_C_B_necromass_C~F_necromass_C_B_necromass_C_pred_1, natural_FB_test)
abline(fitline_natural_FB,lty=2)
summary(fitline_natural_FB)

#F
#agro
agro_F_normalized_1.dat <- scale(agro_F_necromass.dat) 
colnames(agro_F_normalized_1.dat) <- c("F_necromass_C_SOC","Longitude","Elevation","MAT","MAP","Clay","pH","SOC","SOC_TN","BGBC","NPP","MBC","MBC_MBN")    
agro_F_normalized_1.dat <- as.data.frame(agro_F_normalized_1.dat)

set.seed(123)
agro_F_split<- sample(nrow(agro_F_normalized_1.dat), nrow(agro_F_normalized_1.dat)*0.8)
agro_F_train <- agro_F_normalized_1.dat[agro_F_split, ]
agro_F_test <- agro_F_normalized_1.dat[-agro_F_split, ]

set.seed(123)
brt_agro_F <- gbm(F_necromass_C_SOC ~., data = agro_F_train, distribution = "gaussian", n.trees = 9999, interaction.depth = 9, shrinkage = 0.001, bag.fraction = 0.5, cv.folds = 10)
print(brt_agro_F)
summary(brt_agro_F)

perf_agro_F <- gbm.perf(brt_agro_F,method = "cv")
perf_agro_F
agro_F_test$F_necromass_C_SOC_pred_1 <- predict(brt_agro_F, agro_F_test)
plot(agro_F_test$F_necromass_C_SOC_pred_1, agro_F_test$F_necromass_C_SOC, main = 'Test dataset', xlab = 'Predicted data', ylab = 'Observed data')
abline(a=0,b=1)
fitline_agro_F <- lm(F_necromass_C_SOC~F_necromass_C_SOC_pred_1, agro_F_test)
abline(fitline_agro_F,lty=2)
summary(fitline_agro_F)

#F
#natural
natural_F_normalized_1.dat <- scale(natural_F_necromass.dat)
colnames(natural_F_normalized_1.dat) <- c("F_necromass_C_SOC","Longitude","Elevation","MAT","MAP","Clay","pH","SOC","SOC_TN","BGBC","NPP","MBC","MBC_MBN") 
natural_F_normalized_1.dat <- as.data.frame(natural_F_normalized_1.dat)

set.seed(123)
natural_F_split<- sample(nrow(natural_F_normalized_1.dat), nrow(natural_F_normalized_1.dat)*0.8)
natural_F_train <- natural_F_normalized_1.dat[natural_F_split, ]
natural_F_test <- natural_F_normalized_1.dat[-natural_F_split, ]

set.seed(123)
brt_natural_F <- gbm(F_necromass_C_SOC ~., data = natural_F_train, distribution = "gaussian", n.trees = 9999, interaction.depth = 9, shrinkage = 0.001, bag.fraction = 0.5, cv.folds = 10)
print(brt_natural_F)
summary(brt_natural_F)

perf_natural_F <- gbm.perf(brt_natural_F,method = "cv")
perf_natural_F
natural_F_test$F_necromass_C_SOC_pred_1 <- predict(brt_natural_F, natural_F_test)
plot(natural_F_test$F_necromass_C_SOC_pred_1, natural_F_test$F_necromass_C_SOC, main = 'Test dataset', xlab = 'Predicted data', ylab = 'Observed data')
abline(a=0,b=1)
fitline_natural_F <- lm(F_necromass_C_SOC~F_necromass_C_SOC_pred_1, natural_F_test)
abline(fitline_natural_F,lty=2)
summary(fitline_natural_F)

#B
#agro
agro_B_normalized_1.dat <- scale(agro_B_necromass.dat)
colnames(agro_B_normalized_1.dat) <- c("B_necromass_C_SOC","Longitude","Elevation","MAT","MAP","Clay","pH","SOC","SOC_TN","BGBC","NPP","MBC","MBC_MBN")
agro_B_normalized_1.dat <- as.data.frame(agro_B_normalized_1.dat)

set.seed(123)
agro_B_split<- sample(nrow(agro_B_normalized_1.dat), nrow(agro_B_normalized_1.dat)*0.8)
agro_B_train <- agro_B_normalized_1.dat[agro_B_split, ]
agro_B_test <- agro_B_normalized_1.dat[-agro_B_split, ]

set.seed(123)
brt_agro_B <- gbm(B_necromass_C_SOC ~., data = agro_B_train, distribution = "gaussian", n.trees = 9999, interaction.depth = 9, shrinkage = 0.001, bag.fraction = 0.5, cv.folds = 10)
print(brt_agro_B)
summary(brt_agro_B)

perf_agro_B <- gbm.perf(brt_agro_B,method = "cv")
perf_agro_B
agro_B_test$B_necromass_C_SOC_pred_1 <- predict(brt_agro_B, agro_B_test)
plot(agro_B_test$B_necromass_C_SOC_pred_1, agro_B_test$B_necromass_C_SOC, main = 'Test dataset', xlab = 'Predicted data', ylab = 'Observed data')
abline(a=0,b=1)
fitline_agro_B <- lm(B_necromass_C_SOC~B_necromass_C_SOC_pred_1, agro_B_test)
abline(fitline_agro_B,lty=2)
summary(fitline_agro_B)

#B
#natural
natural_B_normalized_1.dat <- scale(natural_B_necromass.dat) 
colnames(natural_B_normalized_1.dat) <- c("B_necromass_C_SOC","Longitude","Elevation","MAT","MAP","Clay","pH","SOC","SOC_TN","BGBC","NPP","MBC","MBC_MBN")   
natural_B_normalized_1.dat <- as.data.frame(natural_B_normalized_1.dat)

set.seed(123)
natural_B_split<- sample(nrow(natural_B_normalized_1.dat), nrow(natural_B_normalized_1.dat)*0.8)
natural_B_train <- natural_B_normalized_1.dat[natural_B_split, ]
natural_B_test <- natural_B_normalized_1.dat[-natural_B_split, ]

set.seed(123)
brt_natural_B <- gbm(B_necromass_C_SOC ~., data = natural_B_train, distribution = "gaussian", n.trees = 9999, interaction.depth = 9, shrinkage = 0.001, bag.fraction = 0.5, cv.folds = 10)
print(brt_natural_B)
summary(brt_natural_B)

perf_natural_B <- gbm.perf(brt_natural_B,method = "cv")
perf_natural_B
natural_B_test$B_necromass_C_SOC_pred_1 <- predict(brt_natural_B, natural_B_test)
plot(natural_B_test$B_necromass_C_SOC_pred_1, natural_B_test$B_necromass_C_SOC, main = 'Test dataset', xlab = 'Predicted data', ylab = 'Observed data')
abline(a=0,b=1)
fitline_natural_B <- lm(B_necromass_C_SOC~B_necromass_C_SOC_pred_1, natural_B_test)
abline(fitline_natural_B,lty=2)
summary(fitline_natural_B)

#####Figure 5, S6#####
library(gbm)
library(readr)
library(dplyr)
library(caret)

##FNC
target_FNC <- read.csv("gbm.dat.FNC.csv")
target_FNC$Ecosystems <- as.factor(target_FNC$Ecosystems)

data_agro_FNC <- subset(target_FNC, Ecosystems == 'agro')
selected_data_agro_FNC <- select(data_agro_FNC, -c(F_necromass_C_SOC, Ecosystems))
selected_data_agro_FNC_1 <- select(data_agro_FNC, -c(Latitude, Ecosystems))

data_natural_FNC <- subset(target_FNC, Ecosystems == 'natural')
selected_data_natural_FNC <- select(data_natural_FNC, -c(F_necromass_C_SOC, Ecosystems))
selected_data_natural_FNC_1 <- select(data_natural_FNC, -c(Latitude, Ecosystems))

set.seed(123)  
gbm_model_FNC_agro <- gbm(F_necromass_C_SOC ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", n.trees = 999, interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, n.minobsinnode = 1, data = data_agro_FNC)
gbm_model_FNC_natural <- gbm(F_necromass_C_SOC ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", n.trees = 999, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, n.minobsinnode = 1, data = data_natural_FNC)

#agro
set.seed(123)
predictions_FNC_agro <- predict(gbm_model_FNC_agro, newdata = selected_data_agro_FNC, n.trees = 999)
agro_predictions_FNC <- data.frame(selected_data_agro_FNC[, c(1:2)], predictions_FNC_agro)

set.seed(123) 
bootstrap_iterations = 999 
results_agro_FNC <- replicate(bootstrap_iterations, {
  sampled_data_agro_FNC <- selected_data_agro_FNC_1 %>% sample_n(n(), replace = TRUE)
  gbm_model_FNC_agro <- gbm(F_necromass_C_SOC ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", 
                            n.trees = 999, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, n.minobsinnode = 1, 
                            data = data_agro_FNC)
  predictions_FNC_agro <- predict(gbm_model_FNC_agro, newdata = selected_data_agro_FNC, n.trees = 999)
  actuals_FNC_agro <- selected_data_agro_FNC_1$F_necromass_C_SOC
  r_squared_FNC_agro <- cor(predictions_FNC_agro, actuals_FNC_agro)^2
  rmse_FNC_agro <- sqrt(mean((predictions_FNC_agro - actuals_FNC_agro)^2))
  return(c(R_Squared = r_squared_FNC_agro, RMSE = rmse_FNC_agro))
}, simplify = "array")
average_results_FNC_agro <- apply(results_agro_FNC, 1, mean)
sd_results_FNC_agro <- apply(results_agro_FNC, 1, sd)

predicted_agro_FNC <- agro_predictions_FNC$predictions_FNC_agro
observed_agro_FNC <- data_agro_FNC$F_necromass_C_SOC
fitted_agro_FNC <- data.frame(predicted_agro_FNC, observed_agro_FNC)
plot(fitted_agro_FNC$predicted_agro_FNC, fitted_agro_FNC$observed_agro_FNC, 
     xlab = "Predicted FNC/SOC (%)", ylab = "Observed FNC/SOC (%)", 
     main = "Observed vs. Predicted FNC/SOC in agroecosystems",
     col = "grey", pch = 20)
model_agro_FNC <- lm(observed_agro_FNC ~ predicted_agro_FNC, data = fitted_agro_FNC)
abline(model_agro_FNC, col="red", lty = 1, lwd=3)
abline(a = 0, b = 1, col = "blue", lty = 2, lwd=3)
summary_model_agro_FNC <- summary(model_agro_FNC)
r_squared <- summary_model_agro_FNC$r.squared
text(x = 15, y = 95, labels = paste("R^2 =", round(r_squared, digits = 3)), col = "black", cex = 1.2)

#natural
set.seed(123)
predictions_FNC_natural <- predict(gbm_model_FNC_natural, newdata = selected_data_natural_FNC, n.trees = 999)
natural_predictions_FNC <- data.frame(selected_data_natural_FNC[, c(1:2)], predictions_FNC_natural)

set.seed(123)
bootstrap_iterations = 999 
results_natural_FNC <- replicate(bootstrap_iterations, {
  sampled_data_natural_FNC <- selected_data_natural_FNC_1 %>% sample_n(n(), replace = TRUE)
  gbm_model_FNC_natural <- gbm(F_necromass_C_SOC ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", 
                               n.trees = 999, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, n.minobsinnode = 1, 
                               data = data_natural_FNC)
  predictions_FNC_natural <- predict(gbm_model_FNC_natural, newdata = selected_data_natural_FNC, n.trees = 999)
  actuals_FNC_natural <- selected_data_natural_FNC_1$F_necromass_C_SOC
  r_squared_FNC_natural <- cor(predictions_FNC_natural, actuals_FNC_natural)^2
  rmse_FNC_natural <- sqrt(mean((predictions_FNC_natural - actuals_FNC_natural)^2))
  return(c(R_Squared = r_squared_FNC_natural, RMSE = rmse_FNC_natural))
}, simplify = "array")
average_results_FNC_natural <- apply(results_natural_FNC, 1, mean)
sd_results_FNC_natural <- apply(results_natural_FNC, 1, sd)

predicted_natural_FNC <- natural_predictions_FNC$predictions_FNC_natural
observed_natural_FNC <- data_natural_FNC$F_necromass_C_SOC
fitted_natural_FNC <- data.frame(predicted_natural_FNC, observed_natural_FNC)
plot(fitted_natural_FNC$predicted_natural_FNC, fitted_natural_FNC$observed_natural_FNC, 
     xlab = "Predicted FNC/SOC (%)", ylab = "Observed FNC/SOC (%)", 
     main = "Observed vs. Predicted FNC/SOC in naturalecosystems",
     col = "grey", pch = 20)
model_natural_FNC <- lm(observed_natural_FNC ~ predicted_natural_FNC, data = fitted_natural_FNC)
abline(model_natural_FNC, col="red", lty = 1, lwd=3)
abline(a = 0, b = 1, col = "blue", lty = 2, lwd=3)
summary_model_natural_FNC <- summary(model_natural_FNC)
r_squared <- summary_model_natural_FNC$r.squared
text(x = 27, y = 95, labels = paste("R^2 =", round(r_squared, digits = 3)), col = "black", cex = 1.2)

set.seed(123)
predictions_FNC_agro <- predict(gbm_model_FNC_agro, newdata = features, n.trees = 999)
agro_predictions_FNC <- data.frame(features[, c(1:2)], predictions_FNC_agro)

predictions_FNC_natural <- predict(gbm_model_FNC_natural, newdata = features, n.trees = 999)
natural_predictions_FNC <- data.frame(features[, c(1:2)], predictions_FNC_natural)

# plot global F_necromass_C in agro- and natural ecosystems
library(tidyverse)
library(ggplot2)

#agro
ggplot() +
  geom_raster(data=agro_predictions_FNC, 
              aes(x=Longitude, y=Latitude,fill=predictions_FNC_agro)) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       na.value = "grey80",
                       name = expression(paste("FNC/SOC (%)")),
                       limits=c(0, 75),
                       breaks=c(0, 25, 50, 75),
                       label=c("0", "25", "50", "75")) +
  labs(title = expression(paste("FNC/SOC in agroecosystems")),
       x=" ",   y=" ",
       fill = "predictions_FNC_agro") +
  scale_y_continuous(expand = c(0,0), limits = c(-65,85),
                     breaks = c(-60, -40, -20, 0, 20, 40,
                                60, 80),
                     label = c("60°S", "40°S", "20°S", 
                               "0", "20°N", "40°N", 
                               "60°N", "80°N")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = c(-120, -60,  0,  60, 120),
                     label = c("120°W", "60°W",  
                               "0", "60°E", "120°E"))+
  theme(text = element_text(size = 16),
        legend.title = element_text(size=12),
        legend.key.size = unit(.7, "cm"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = c(0.13,0.3),
        plot.margin = unit(c(0,2,1,1), "mm"),
        legend.background = element_rect(fill=NA))

#natural ecosystems
ggplot() +
  geom_raster(data=natural_predictions_FNC, 
              aes(x=Longitude, y=Latitude,fill=predictions_FNC_natural)) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       na.value = "grey80",
                       name = expression(paste("FNC/SOC (%)")),
                       limits=c(0, 75),
                       breaks=c(0, 25, 50, 75),
                       label=c("0", "25", "50", "75")) +
  labs(title = expression(paste("FNC/SOC in natural ecosystems")),
       x=" ",   y=" ",
       fill = "predictions_FNC_natural") +
  scale_y_continuous(expand = c(0,0), limits = c(-65,85),
                     breaks = c(-60, -40, -20, 0, 20, 40,
                                60, 80),
                     label = c("60°S", "40°S", "20°S", 
                               "0", "20°N", "40°N", 
                               "60°N", "80°N")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = c(-120, -60,  0,  60, 120),
                     label = c("120°W", "60°W",  
                               "0", "60°E", "120°E"))+
  theme(text = element_text(size = 16),
        legend.title = element_text(size=12),
        legend.key.size = unit(.7, "cm"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = c(0.13,0.3),
        plot.margin = unit(c(0,2,1,1), "mm"),
        legend.background = element_rect(fill=NA))

##BNC
target_BNC <- read.csv("gbm.dat.BNC.csv")
target_BNC$Ecosystems <- as.factor(target_BNC$Ecosystems)

data_agro_BNC <- subset(target_BNC, Ecosystems == 'agro')
selected_data_agro_BNC <- select(data_agro_BNC, -c(B_necromass_C_SOC, Ecosystems))
selected_data_agro_BNC_1 <- select(data_agro_BNC, -c(Latitude, Ecosystems))

data_natural_BNC <- subset(target_BNC, Ecosystems == 'natural')
selected_data_natural_BNC <- select(data_natural_BNC, -c(B_necromass_C_SOC, Ecosystems))
selected_data_natural_BNC_1 <- select(data_natural_BNC, -c(Latitude, Ecosystems))

set.seed(123) 
gbm_model_BNC_agro <- gbm(B_necromass_C_SOC ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", n.trees = 999, interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, n.minobsinnode = 1, data = data_agro_BNC)
gbm_model_BNC_natural <- gbm(B_necromass_C_SOC ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", n.trees = 999, interaction.depth = 3, shrinkage = 0.005, cv.folds = 5, n.minobsinnode = 1, data = data_natural_BNC)

#agro
set.seed(123)
predictions_BNC_agro <- predict(gbm_model_BNC_agro, newdata = selected_data_agro_BNC, n.trees = 999)
agro_predictions_BNC <- data.frame(selected_data_agro_BNC[, c(1:2)], predictions_BNC_agro)

set.seed(123) 
bootstrap_iterations = 999 
results_agro_BNC <- replicate(bootstrap_iterations, {
  sampled_data_agro_BNC <- selected_data_agro_BNC_1 %>% sample_n(n(), replace = TRUE)
  gbm_model_BNC_agro <- gbm(B_necromass_C_SOC ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", 
                            n.trees = 999, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, n.minobsinnode = 1, 
                            data = data_agro_BNC)
  predictions_BNC_agro <- predict(gbm_model_BNC_agro, newdata = selected_data_agro_BNC, n.trees = 999)
  actuals_BNC_agro <- selected_data_agro_BNC_1$B_necromass_C_SOC
  r_squared_BNC_agro <- cor(predictions_BNC_agro, actuals_BNC_agro)^2
  rmse_BNC_agro <- sqrt(mean((predictions_BNC_agro - actuals_BNC_agro)^2))
  return(c(R_Squared = r_squared_BNC_agro, RMSE = rmse_BNC_agro))
}, simplify = "array")
average_results_BNC_agro <- apply(results_agro_BNC, 1, mean)
sd_results_BNC_agro <- apply(results_agro_BNC, 1, sd)

predicted_agro_BNC <- agro_predictions_BNC$predictions_BNC_agro
observed_agro_BNC <- data_agro_BNC$B_necromass_C_SOC
fitted_agro_BNC <- data.frame(predicted_agro_BNC, observed_agro_BNC)
plot(fitted_agro_BNC$predicted_agro_BNC, fitted_agro_BNC$observed_agro_BNC, 
     xlab = "Predicted BNC/SOC (%)", ylab = "Observed BNC/SOC (%)", 
     main = "Observed vs. Predicted BNC/SOC in agroecosystems",
     col = "grey", pch = 20)
model_agro_BNC <- lm(observed_agro_BNC ~ predicted_agro_BNC, data = fitted_agro_BNC)
abline(model_agro_BNC, col="red", lty = 1, lwd=3)
abline(a = 0, b = 1, col = "blue", lty = 2, lwd=3)
summary_model_agro_BNC <- summary(model_agro_BNC)
r_squared <- summary_model_agro_BNC$r.squared
text(x = 15, y = 60, labels = paste("R^2 =", round(r_squared, digits = 3)), col = "black", cex = 1.2)

#natural
set.seed(123)
predictions_BNC_natural <- predict(gbm_model_BNC_natural, newdata = selected_data_natural_BNC, n.trees = 999)
natural_predictions_BNC <- data.frame(selected_data_natural_BNC[, c(1:2)], predictions_BNC_natural)

set.seed(123)
bootstrap_iterations = 999 
results_natural_BNC <- replicate(bootstrap_iterations, {
  sampled_data_natural_BNC <- selected_data_natural_BNC_1 %>% sample_n(n(), replace = TRUE)
  gbm_model_BNC_natural <- gbm(B_necromass_C_SOC ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", 
                               n.trees = 999, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, n.minobsinnode = 1, 
                               data = data_natural_BNC)
  predictions_BNC_natural <- predict(gbm_model_BNC_natural, newdata = selected_data_natural_BNC, n.trees = 999)
  actuals_BNC_natural <- selected_data_natural_BNC_1$B_necromass_C_SOC
  r_squared_BNC_natural <- cor(predictions_BNC_natural, actuals_BNC_natural)^2
  rmse_BNC_natural <- sqrt(mean((predictions_BNC_natural - actuals_BNC_natural)^2))
  return(c(R_Squared = r_squared_BNC_natural, RMSE = rmse_BNC_natural))
}, simplify = "array")
average_results_BNC_natural <- apply(results_natural_BNC, 1, mean)
sd_results_BNC_natural <- apply(results_natural_BNC, 1, sd)

predicted_natural_BNC <- natural_predictions_BNC$predictions_BNC_natural
observed_natural_BNC <- data_natural_BNC$B_necromass_C_SOC
fitted_natural_BNC <- data.frame(predicted_natural_BNC, observed_natural_BNC)
plot(fitted_natural_BNC$predicted_natural_BNC, fitted_natural_BNC$observed_natural_BNC, 
     xlab = "Predicted BNC/SOC (%)", ylab = "Observed BNC/SOC (%)", 
     main = "Observed vs. Predicted BNC/SOC in natural ecosystems",
     col = "grey", pch = 20)
model_natural_BNC <- lm(observed_natural_BNC ~ predicted_natural_BNC, data = fitted_natural_BNC)
abline(model_natural_BNC, col="red", lty = 1, lwd=3)
abline(a = 0, b = 1, col = "blue", lty = 2, lwd=3)
summary_model_natural_BNC <- summary(model_natural_BNC)
r_squared <- summary_model_natural_BNC$r.squared
text(x =11, y = 45, labels = paste("R^2 =", round(r_squared, digits = 3)), col = "black", cex = 1.2)

set.seed(123)
predictions_BNC_agro <- predict(gbm_model_BNC_agro, newdata = features, n.trees = 999)
agro_predictions_BNC <- data.frame(features[, c(1:2)], predictions_BNC_agro)

predictions_BNC_natural <- predict(gbm_model_BNC_natural, newdata = features, n.trees = 999)
natural_predictions_BNC <- data.frame(features[, c(1:2)], predictions_BNC_natural)

# plot global F_necromass_C in agro- and natural ecosystems
#agro
ggplot() +
  geom_raster(data=agro_predictions_BNC, 
              aes(x=Longitude, y=Latitude,fill=predictions_BNC_agro)) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       na.value = "grey80",
                       name = expression(paste("BNC/SOC (%)")),
                       limits=c(0, 40),
                       breaks=c(0, 20, 40),
                       label=c("0", "20", "40")) +
  labs(title = expression(paste("BNC/SOC in agroecosystems")),
       x=" ",   y=" ",
       fill = "predictions_BNC_agro") +
  scale_y_continuous(expand = c(0,0), limits = c(-65,85),
                     breaks = c(-60, -40, -20, 0, 20, 40,
                                60, 80),
                     label = c("60°S", "40°S", "20°S", 
                               "0", "20°N", "40°N", 
                               "60°N", "80°N")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = c(-120, -60,  0,  60, 120),
                     label = c("120°W", "60°W",  
                               "0", "60°E", "120°E"))+
  theme(text = element_text(size = 16),
        legend.title = element_text(size=12),
        legend.key.size = unit(.7, "cm"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = c(0.13,0.3),
        plot.margin = unit(c(0,2,1,1), "mm"),
        legend.background = element_rect(fill=NA))

#natural ecosystems
ggplot() +
  geom_raster(data=natural_predictions_BNC, 
              aes(x=Longitude, y=Latitude,fill=predictions_BNC_natural)) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       na.value = "grey80",
                       name = expression(paste("BNC/SOC (%)")),
                       limits=c(0, 40),
                       breaks=c(0, 20, 40),
                       label=c("0", "20", "40")) +
  labs(title = expression(paste("BNC/SOC in natural ecosystems")),
       x=" ",   y=" ",
       fill = "predictions_BNC_natural") +
  scale_y_continuous(expand = c(0,0), limits = c(-65,85),
                     breaks = c(-60, -40, -20, 0, 20, 40,
                                60, 80),
                     label = c("60°S", "40°S", "20°S", 
                               "0", "20°N", "40°N", 
                               "60°N", "80°N")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = c(-120, -60,  0,  60, 120),
                     label = c("120°W", "60°W",  
                               "0", "60°E", "120°E"))+
  theme(text = element_text(size = 16),
        legend.title = element_text(size=12),
        legend.key.size = unit(.7, "cm"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = c(0.13,0.3),
        plot.margin = unit(c(0,2,1,1), "mm"),
        legend.background = element_rect(fill=NA))

##FNC_BNC
target_FNC_BNC <- read.csv("gbm.dat.FNC.BNC.csv")
target_FNC_BNC$Ecosystems <- as.factor(target_FNC_BNC$Ecosystems)

data_agro_FNC_BNC <- subset(target_FNC_BNC, Ecosystems == 'agro')
selected_data_agro_FNC_BNC <- select(data_agro_FNC_BNC, -c(F_necromass_C_B_necromass_C, Ecosystems))
selected_data_agro_FNC_BNC_1 <- select(data_agro_FNC_BNC, -c(Latitude, Ecosystems))

data_natural_FNC_BNC <- subset(target_FNC_BNC, Ecosystems == 'natural')
selected_data_natural_FNC_BNC <- select(data_natural_FNC_BNC, -c(F_necromass_C_B_necromass_C, Ecosystems))
selected_data_natural_FNC_BNC_1 <- select(data_natural_FNC_BNC, -c(Latitude, Ecosystems))

set.seed(123)  
gbm_model_FNC_BNC_agro <- gbm(F_necromass_C_B_necromass_C ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", n.trees = 999, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, n.minobsinnode = 1, data = data_agro_FNC_BNC)
gbm_model_FNC_BNC_natural <- gbm(F_necromass_C_B_necromass_C ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", n.trees = 999, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, n.minobsinnode = 1, data = data_natural_FNC_BNC)

#agro
set.seed(123)
predictions_FNC_BNC_agro <- predict(gbm_model_FNC_BNC_agro, newdata = selected_data_agro_FNC_BNC, n.trees = 999)
agro_predictions_FNC_BNC <- data.frame(selected_data_agro_FNC_BNC[, c(1:2)], predictions_FNC_BNC_agro)

set.seed(123) 
bootstrap_iterations = 999 
results_agro_FNC_BNC <- replicate(bootstrap_iterations, {
  sampled_data_agro_FNC_BNC <- selected_data_agro_FNC_BNC_1 %>% sample_n(n(), replace = TRUE)
  gbm_model_FNC_BNC_agro <- gbm(F_necromass_C_B_necromass_C ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", 
                                n.trees = 999, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, n.minobsinnode = 1, 
                                data = data_agro_FNC_BNC)
  predictions_FNC_BNC_agro <- predict(gbm_model_FNC_BNC_agro, newdata = selected_data_agro_FNC_BNC, n.trees = 999)
  actuals_FNC_BNC_agro <- selected_data_agro_FNC_BNC_1$F_necromass_C_B_necromass_C
  r_squared_FNC_BNC_agro <- cor(predictions_FNC_BNC_agro, actuals_FNC_BNC_agro)^2
  rmse_FNC_BNC_agro <- sqrt(mean((predictions_FNC_BNC_agro - actuals_FNC_BNC_agro)^2))
  return(c(R_Squared = r_squared_FNC_BNC_agro, RMSE = rmse_FNC_BNC_agro))
}, simplify = "array")
average_results_FNC_BNC_agro <- apply(results_agro_FNC_BNC, 1, mean)
sd_results_FNC_BNC_agro <- apply(results_agro_FNC_BNC, 1, sd)

predicted_agro_FNC_BNC <- agro_predictions_FNC_BNC$predictions_FNC_BNC_agro
observed_agro_FNC_BNC <- data_agro_FNC_BNC$F_necromass_C_B_necromass_C
fitted_agro_FNC_BNC <- data.frame(predicted_agro_FNC_BNC, observed_agro_FNC_BNC)
plot(fitted_agro_FNC_BNC$predicted_agro_FNC_BNC, fitted_agro_FNC_BNC$observed_agro_FNC_BNC, 
     xlab = "Predicted FNC/BNC", ylab = "Observed FNC/BNC", 
     main = "Observed vs. Predicted FNC/BNC in agroecosystems",
     col = "grey", pch = 20)
model_agro_FNC_BNC <- lm(observed_agro_FNC_BNC ~ predicted_agro_FNC_BNC, data = fitted_agro_FNC_BNC)
abline(model_agro_FNC_BNC, col="red", lty = 1, lwd=3)
abline(a = 0, b = 1, col = "blue", lty = 2, lwd=3)
summary_model_agro_FNC_BNC <- summary(model_agro_FNC_BNC)
r_squared <- summary_model_agro_FNC_BNC$r.squared
text(x = 2, y = 11, labels = paste("R^2 =", round(r_squared, digits = 3)), col = "black", cex = 1.2)

#natural
set.seed(123)
predictions_FNC_BNC_natural <- predict(gbm_model_FNC_BNC_natural, newdata = selected_data_natural_FNC_BNC, n.trees = 999)
natural_predictions_FNC_BNC <- data.frame(selected_data_natural_FNC_BNC[, c(1:2)], predictions_FNC_BNC_natural)

set.seed(123)
bootstrap_iterations = 999 
results_natural_FNC_BNC <- replicate(bootstrap_iterations, {
  sampled_data_natural_FNC_BNC <- selected_data_natural_FNC_BNC_1 %>% sample_n(n(), replace = TRUE)
  gbm_model_FNC_BNC_natural <- gbm(F_necromass_C_B_necromass_C ~ . -Ecosystems -Longitude -Latitude, distribution = "gaussian", 
                                   n.trees = 999, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, n.minobsinnode = 1, 
                                   data = data_natural_FNC_BNC)
  predictions_FNC_BNC_natural <- predict(gbm_model_FNC_BNC_natural, newdata = selected_data_natural_FNC_BNC, n.trees = 999)
  actuals_FNC_BNC_natural <- selected_data_natural_FNC_BNC_1$F_necromass_C_B_necromass_C
  r_squared_FNC_BNC_natural <- cor(predictions_FNC_BNC_natural, actuals_FNC_BNC_natural)^2
  rmse_FNC_BNC_natural <- sqrt(mean((predictions_FNC_BNC_natural - actuals_FNC_BNC_natural)^2))
  return(c(R_Squared = r_squared_FNC_BNC_natural, RMSE = rmse_FNC_BNC_natural))
}, simplify = "array")
average_results_FNC_BNC_natural <- apply(results_natural_FNC_BNC, 1, mean)
sd_results_FNC_BNC_natural <- apply(results_natural_FNC_BNC, 1, sd)

predicted_natural_FNC_BNC <- natural_predictions_FNC_BNC$predictions_FNC_BNC_natural
observed_natural_FNC_BNC <- data_natural_FNC_BNC$F_necromass_C_B_necromass_C
fitted_natural_FNC_BNC <- data.frame(predicted_natural_FNC_BNC, observed_natural_FNC_BNC)
plot(fitted_natural_FNC_BNC$predicted_natural_FNC_BNC, fitted_natural_FNC_BNC$observed_natural_FNC_BNC, 
     xlab = "Predicted FNC/BNC", ylab = "Observed FNC/BNC", 
     main = "Observed vs. Predicted FNC/BNC in natural ecosystems",
     col = "grey", pch = 20)
model_natural_FNC_BNC <- lm(observed_natural_FNC_BNC ~ predicted_natural_FNC_BNC, data = fitted_natural_FNC_BNC)
abline(model_natural_FNC_BNC, col="red", lty = 1, lwd=3)
abline(a = 0, b = 1, col = "blue", lty = 2, lwd=3)
summary_model_natural_FNC_BNC <- summary(model_natural_FNC_BNC)
r_squared <- summary_model_natural_FNC_BNC$r.squared
text(x =3, y = 15, labels = paste("R^2 =", round(r_squared, digits = 3)), col = "black", cex = 1.2)

set.seed(123)
predictions_FNC_BNC_agro <- predict(gbm_model_FNC_BNC_agro, newdata = features, n.trees = 999)
agro_predictions_FNC_BNC <- data.frame(features[, c(1:2)], predictions_FNC_BNC_agro)

predictions_FNC_BNC_natural <- predict(gbm_model_FNC_BNC_natural, newdata = features, n.trees = 999)
natural_predictions_FNC_BNC <- data.frame(features[, c(1:2)], predictions_FNC_BNC_natural)

# plot global F_necromass_C in agro- and natural ecosystems
#agro
ggplot() +
  geom_raster(data=agro_predictions_FNC_BNC, 
              aes(x=Longitude, y=Latitude,fill=predictions_FNC_BNC_agro)) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       na.value = "grey80",
                       name = expression(paste("FNC/BNC")),
                       limits=c(0, 10),
                       breaks=c(0, 5, 10),
                       label=c("0", "5", "10")) +
  labs(title = expression(paste("FNC/BNC in agroecosystems")),
       x=" ",   y=" ",
       fill = "predictions_FNC_BNC_agro") +
  scale_y_continuous(expand = c(0,0), limits = c(-65,85),
                     breaks = c(-60, -40, -20, 0, 20, 40,
                                60, 80),
                     label = c("60°S", "40°S", "20°S", 
                               "0", "20°N", "40°N", 
                               "60°N", "80°N")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = c(-120, -60,  0,  60, 120),
                     label = c("120°W", "60°W",  
                               "0", "60°E", "120°E"))+
  theme(text = element_text(size = 16),
        legend.title = element_text(size=12),
        legend.key.size = unit(.7, "cm"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = c(0.13,0.3),
        plot.margin = unit(c(0,2,1,1), "mm"),
        legend.background = element_rect(fill=NA))

#natural ecosystems
ggplot() +
  geom_raster(data=natural_predictions_FNC_BNC, 
              aes(x=Longitude, y=Latitude,fill=predictions_FNC_BNC_natural)) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       na.value = "grey80",
                       name = expression(paste("FNC/BNC")),
                       limits=c(0, 10),
                       breaks=c(0, 5, 10),
                       label=c("0", "5", "10")) +
  labs(title = expression(paste("FNC/BNC in natural ecosystems")),
       x=" ",   y=" ",
       fill = "predictions_FNC_BNC_natural") +
  scale_y_continuous(expand = c(0,0), limits = c(-65,85),
                     breaks = c(-60, -40, -20, 0, 20, 40,
                                60, 80),
                     label = c("60°S", "40°S", "20°S", 
                               "0", "20°N", "40°N", 
                               "60°N", "80°N")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = c(-120, -60,  0,  60, 120),
                     label = c("120°W", "60°W",  
                               "0", "60°E", "120°E"))+
  theme(text = element_text(size = 16),
        legend.title = element_text(size=12),
        legend.key.size = unit(.7, "cm"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = c(0.13,0.3),
        plot.margin = unit(c(0,2,1,1), "mm"),
        legend.background = element_rect(fill=NA))

#####Figure S1#####
natural_NNA.dat <- read.csv("natural-NNA.csv") 
natural_YNA.dat <- read.csv("natural-YNA.csv")  

###Shapiro-Wilk test
shapiro_F_necromass_C_B_necromass_C <- tapply(natural_NNA.dat$F_necromass_C_B_necromass_C, natural_NNA.dat$Ecosystem, shapiro.test)
shapiro_F_necromass_C_B_necromass_C
shapiro_F_necromass_C_B_necromass_C$'Forest'$p.value
shapiro_F_necromass_C_B_necromass_C$'Grassland'$p.value 

shapiro_F_necromass_C_SOC <- tapply(natural_NNA.dat$F_necromass_C_SOC, natural_NNA.dat$Ecosystem, shapiro.test)
shapiro_F_necromass_C_SOC
shapiro_F_necromass_C_SOC$'Forest'$p.value
shapiro_F_necromass_C_SOC$'Grassland'$p.value

shapiro_B_necromass_C_SOC <- tapply(natural_NNA.dat$B_necromass_C_SOC, natural_NNA.dat$Ecosystem, shapiro.test)
shapiro_B_necromass_C_SOC
shapiro_B_necromass_C_SOC$'Forest'$p.value
shapiro_B_necromass_C_SOC$'Grassland'$p.value 

#nonparametric test——Wilcoxon rank sum test
wilcox_F_necromass_C_B_necromass_C <- wilcox.test(F_necromass_C_B_necromass_C~Ecosystem, natural_NNA.dat, paired = FALSE, alternative = 'two.sided')
wilcox_F_necromass_C_B_necromass_C
wilcox_F_necromass_C_B_necromass_C$p.value

wilcox_F_necromass_C_SOC <- wilcox.test(F_necromass_C_SOC~Ecosystem, natural_NNA.dat, paired = FALSE, alternative = 'two.sided')
wilcox_F_necromass_C_SOC
wilcox_F_necromass_C_SOC$p.value

wilcox_B_necromass_C_SOC <- wilcox.test(B_necromass_C_SOC~Ecosystem, natural_NNA.dat, paired = FALSE, alternative = 'two.sided')
wilcox_B_necromass_C_SOC
wilcox_B_necromass_C_SOC$p.value

library(ggplot2)
library(ggsci)
library(ggpubr)
library(tidyverse)
library(cowplot)

natural_NNA.dat %>% ggboxplot( x="Ecosystem", y="F_necromass_C_B_necromass_C", color = "Ecosystem", bxp.errorbar = T, 
                               outlier.shape = NA, palette = c("#1b9e77" ,"#66a61e") ,alpha = 0.7,add = "jitter", shape=1 ,size = 1.5)+
  theme_pubr(base_size = 20)+theme_test(base_size = 20)+
  theme(legend.position = "none")+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.background = element_rect(fill = "white")) +labs(x = "Ecosystem", y = "F_necromass_C_B_necromass_C")+
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0))+
  theme(axis.text = element_text(face = "bold")) +labs(x = NULL)+ylim(c(0,15))

natural_NNA.dat %>% ggboxplot( x="Ecosystem", y="F_necromass_C_SOC", color = "Ecosystem", bxp.errorbar = T, 
                               outlier.shape = NA, palette = c("#1b9e77" ,"#66a61e") ,alpha = 0.7,add = "jitter", shape=1 ,size = 1.5)+
  theme_pubr(base_size = 20)+theme_test(base_size = 20)+
  theme(legend.position = "none")+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.background = element_rect(fill = "white")) +labs(x = "Ecosystem", y = "F_necromass_C_SOC")+
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0))+
  theme(axis.text = element_text(face = "bold")) +labs(x = NULL)+ylim(c(0,100))


natural_NNA.dat %>% ggboxplot( x="Ecosystem", y="B_necromass_C_SOC", color = "Ecosystem", bxp.errorbar = T, 
                               outlier.shape = NA, palette = c("#1b9e77" ,"#66a61e") ,alpha = 0.7,add ="jitter", shape=1 ,size = 1.5)+
  theme_pubr(base_size = 20)+theme_test(base_size = 20)+
  theme(legend.position = "none")+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.background = element_rect(fill = "white")) +labs(x = "Ecosystem", y = "B_necromass_C_SOC")+
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0))+
  theme(axis.text = element_text(face = "bold")) +labs(x = NULL)+ylim(c(0,100))

#####Figure S2, S3 and S4#####
FB_necromass.dat <- read.csv("FB_necromass_0-20.csv")                  
B_necromass.dat <- read.csv("B_necromass_0-20.csv") 
F_necromass.dat <- read.csv("F_necromass_0-20.csv") 

library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(patchwork)

#FB
ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = Longitude, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = Longitude, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = Longitude,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p1
p1

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = Elevation, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = Elevation, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = Elevation,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p2
p2

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = MAT, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = MAT, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = MAT,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p3
p3

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = MAP, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = MAP, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = MAP,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p4
p4

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = Clay, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = Clay, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = Clay,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p5
p5

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = pH, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = pH, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = pH,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p6
p6

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = SOC, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = SOC, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = SOC,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p7
p7

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = SOC_TN, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = SOC_TN, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = SOC_TN,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p8
p8

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = BGBC, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = BGBC, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = BGBC,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p9
p9

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = NPP, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = NPP, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = NPP,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p10
p10

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = MBC, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = MBC, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = MBC,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p11
p11

ggplot(FB_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = MBC_MBN, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = MBC_MBN, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = MBC_MBN,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p12
p12

(p1+p2+p3)/(p4+p5+p6)/(p7+p8+p9)/(p10+p11+p12)

#F
ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = Longitude, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = Longitude, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = Longitude,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p1
p1

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = Elevation, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = Elevation, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = Elevation,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p2
p2

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = MAT, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = MAT, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = MAT,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p3
p3

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = MAP, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = MAP, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = MAP,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p4
p4

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = Clay, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = Clay, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = Clay,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p5
p5

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = pH, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = pH, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = pH,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p6
p6

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = SOC, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = SOC, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = SOC,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p7
p7

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = SOC_TN, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = SOC_TN, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = SOC_TN,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p8
p8

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = BGBC, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = BGBC, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = BGBC,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p9
p9

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = NPP, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = NPP, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = NPP,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p10
p10

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = MBC, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = MBC, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = MBC,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p11
p11

ggplot(F_necromass.dat,aes(group=habs))+
  geom_point(aes(y = F_necromass_C_SOC, x = MBC_MBN, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = MBC_MBN, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = MBC_MBN,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p12
p12

(p1+p2+p3)/(p4+p5+p6)/(p7+p8+p9)/(p10+p11+p12)

#B
ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = Longitude, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = Longitude, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = Longitude,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p1
p1

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = Elevation, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = Elevation, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = Elevation,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p2
p2

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = MAT, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = MAT, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = MAT,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p3
p3

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = MAP, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = MAP, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = MAP,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p4
p4

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = Clay, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = Clay, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = Clay,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p5
p5

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = pH, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = pH, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = pH,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p6
p6

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = SOC, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = SOC, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = SOC,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p7
p7

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = SOC_TN, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = SOC_TN, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = SOC_TN,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p8
p8

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = BGBC, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = BGBC, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = BGBC,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p9
p9

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = NPP, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = NPP, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = NPP,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p10
p10

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = MBC, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = MBC, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = MBC,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p11
p11

ggplot(B_necromass.dat,aes(group=habs))+
  geom_point(aes(y = B_necromass_C_SOC, x = MBC_MBN, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = MBC_MBN, color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = MBC_MBN,color=habs, label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~x, label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p12
p12

(p1+p2+p3)/(p4+p5+p6)/(p7+p8+p9)/(p10+p11+p12)

#####Figure S5#####
FB_necromass.dat <- read.csv("FB_latitude_0-20.csv")                 
B_necromass.dat <- read.csv("B_latitude_0-20.csv") 
F_necromass.dat <- read.csv("F_latitude_0-20.csv")

library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(patchwork)

ggplot(FB_necromass.dat,aes(group=habs))+#labs(x = NULL)+
  geom_point(aes(y = F_necromass_C_B_necromass_C, x = Latitude, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_B_necromass_C, x = Latitude,color=habs) , level=0.95, size = 1,
             method = 'lm' , formula=y~poly(x,2))+
  stat_poly_eq(aes(y = F_necromass_C_B_necromass_C, x = Latitude, color=habs,label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
             formula =y~poly(x,2), label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p1
p1

ggplot(F_necromass.dat,aes(group=habs))+#labs(x = NULL)+
  geom_point(aes(y = F_necromass_C_SOC, x = Latitude, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_necromass_C_SOC, x = Latitude,color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~poly(x,2))+
  stat_poly_eq(aes(y = F_necromass_C_SOC, x = Latitude, color=habs,label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~poly(x,2), label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992FF", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992FF", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p2
p2

ggplot(B_necromass.dat,aes(group=habs))+#labs(x = NULL)+
  geom_point(aes(y = B_necromass_C_SOC, x = Latitude, fill = habs, color = habs) ,size = 3,alpha = 0.5 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_necromass_C_SOC, x = Latitude,color=habs) , level=0.95, size = 1,
              method = 'lm' , formula=y~poly(x,2))+
  stat_poly_eq(aes(y = B_necromass_C_SOC, x = Latitude, color=habs,label = paste( ..rr.label.., sep = "~", stat(p.value.label))), 
               formula =y~poly(x,2), label.x = "left",parse = T,family = "serif",size = 5) +
  scale_color_manual(values = c("#3B4992BB", "#CC3333"))+
  scale_fill_manual(values = c("#3B4992BB", "#CC3333"))+
  theme_pubr(border = T)+ 
  theme(legend.position = "right")+
  theme(strip.text.x = element_blank()) ->p3
p3

#####Figure S7#####
library(Hmisc)
library(reshape2)
library(dplyr)
library(ggplot2)
agro_Microbial_spearman_r <- cor(agro_Microbial_necromass.dat[,2:17], method = 'spearman', use="pairwise.complete.obs")  
agro_Microbial_spearman_p <- rcorr(as.matrix(agro_Microbial_spearman_r), type = 'spearman')   
agro_Microbial_spearman_r[!lower.tri(agro_Microbial_spearman_r, diag = F)] <- 0    
agro_Microbial_spearman_length_r <- melt(agro_Microbial_spearman_r)    
agro_Microbial_spearman_length_p <- melt(agro_Microbial_spearman_p$P)     
agro_Microbial_spearman.dat = cbind(agro_Microbial_spearman_length_r,agro_Microbial_spearman_length_p[,3])   
colnames(agro_Microbial_spearman.dat) <- c("Var1","Var2","r","p")    
agro_Microbial_spearman.dat %>% filter(r!=0) -> agro_Microbial_spearman.dat2    
agro_Microbial_spearman.dat2 %>% mutate(sig = case_when(p <= 0.001 ~'***' , (p > 0.001 & p <= 0.01) ~'**', (p > 0.01 & p <= 0.05) ~'*')) -> agro_Microbial_spearman.dat_sig     
agro_Microbial_spearman.dat_sig -> agro_Microbial_spearman.dat_sig_con_dis     
agro_Microbial_spearman.dat_sig_con_dis$Var2 <- factor(agro_Microbial_spearman.dat_sig_con_dis$Var2, levels = rev(colnames(agro_Microbial_necromass.dat[,2:17])))  
agro_Microbial_spearman.dat_sig_con_dis$Var1 <- factor(agro_Microbial_spearman.dat_sig_con_dis$Var1, levels = rev(colnames(agro_Microbial_necromass.dat[,2:17])))  


natural_Microbial_spearman_r <- cor(natural_Microbial_necromass.dat[,2:17], method = 'spearman', use="pairwise.complete.obs")   
natural_Microbial_spearman_p <- rcorr(as.matrix(natural_Microbial_spearman_r), type = 'spearman')     
natural_Microbial_spearman_r[!lower.tri(natural_Microbial_spearman_r, diag = F)] <- 0     
natural_Microbial_spearman_length_r <- melt(natural_Microbial_spearman_r)    
natural_Microbial_spearman_length_p <- melt(natural_Microbial_spearman_p$P)     
natural_Microbial_spearman.dat = cbind(natural_Microbial_spearman_length_r,natural_Microbial_spearman_length_p[,3])   
colnames(natural_Microbial_spearman.dat) <- c("Var1","Var2","r","p")    
natural_Microbial_spearman.dat %>% filter(r!=0) -> natural_Microbial_spearman.dat2    
natural_Microbial_spearman.dat2 %>% mutate(sig = case_when(p <= 0.001 ~'***' , (p > 0.001 & p <= 0.01) ~'**', (p > 0.01 & p <= 0.05) ~'*')) -> natural_Microbial_spearman.dat_sig     
natural_Microbial_spearman.dat_sig -> natural_Microbial_spearman.dat_sig_con_dis     
natural_Microbial_spearman.dat_sig_con_dis$Var2 <- factor(natural_Microbial_spearman.dat_sig_con_dis$Var2, levels = rev(colnames(natural_Microbial_necromass.dat[,2:17])))  
natural_Microbial_spearman.dat_sig_con_dis$Var1 <- factor(natural_Microbial_spearman.dat_sig_con_dis$Var1, levels = rev(colnames(natural_Microbial_necromass.dat[,2:17])))  

rbind(natural_Microbial_spearman.dat_sig_con_dis %>% mutate(gr="natural"),agro_Microbial_spearman.dat_sig_con_dis %>% mutate(gr='agro')) -> combine_ag_na 
combine_ag_na %>% ggplot() +   
  geom_point(aes(x= Var2,y = Var1,size= abs(r), color = r), alpha =0.9)+
  scale_shape_manual(values = 21) +
  scale_size_continuous( range=c(3,9),limits = c(0,1))+ 
  scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)+  
  theme_bw(base_size = 10)+    
  theme(axis.text.x = element_text(colour = "black", angle = 30 ,hjust = 1), axis.text.y = element_text(colour = "black", angle = 0 ,hjust = 1, vjust = 0))+
  geom_text(aes(x=Var2, y = Var1,label = sig), family = 'serif' ,color = 'black')+  
  coord_flip()+ 
  theme(legend.position = "right")+   
  facet_wrap(.~gr)

#####Figure S8#####
library(car)

#FB
agro_FB_normalized.dat <- scale(agro_FB_necromass.dat)  
colnames(agro_FB_normalized.dat) <- c("F_necromass_C_B_necromass_C","Longitude","Latitude","Elevation","MAT","MAP","Clay","pH","SOC","TN","SOC_TN","Soil_temperature","BGBC","NPP","MBC","MBN","MBC_MBN")    
agro_FB_normalized.dat <- as.data.frame(agro_FB_normalized.dat)   

fit_agro_FB <- lm(F_necromass_C_B_necromass_C ~ Longitude+Latitude+Elevation+MAT+MAP+Clay+pH+SOC+TN+SOC_TN+Soil_temperature+BGBC+NPP+MBC+MBN+MBC_MBN, data = agro_FB_normalized.dat)
vif(fit_agro_FB)      

fit_agro_FB <- lm(F_necromass_C_B_necromass_C ~ Longitude+Elevation+MAT+MAP+Clay+pH+SOC+SOC_TN+BGBC+NPP+MBC+MBC_MBN, data = agro_FB_normalized.dat)
vif(fit_agro_FB)

natural_FB_normalized.dat <- scale(natural_FB_necromass.dat)  
colnames(natural_FB_normalized.dat) <- c("F_necromass_C_B_necromass_C","Longitude","Latitude","Elevation","MAT","MAP","Clay","pH","SOC","TN","SOC_TN","Soil_temperature","BGBC","NPP","MBC","MBN","MBC_MBN")    
natural_FB_normalized.dat <- as.data.frame(natural_FB_normalized.dat)   

fit_natural_FB <- lm(F_necromass_C_B_necromass_C ~ Longitude+Latitude+Elevation+MAT+MAP+Clay+pH+SOC+TN+SOC_TN+Soil_temperature+BGBC+NPP+MBC+MBN+MBC_MBN, data = natural_FB_normalized.dat)
vif(fit_natural_FB)

fit_natural_FB <- lm(F_necromass_C_B_necromass_C ~ Longitude+Elevation+MAT+MAP+Clay+pH+SOC+SOC_TN+BGBC+NPP+MBC+MBC_MBN, data = natural_FB_normalized.dat)
vif(fit_natural_FB)

#F
agro_F_normalized.dat <- scale(agro_F_necromass.dat)  
colnames(agro_F_normalized.dat) <- c("F_necromass_C_SOC","Longitude","Latitude","Elevation","MAT","MAP","Clay","pH","SOC","TN","SOC_TN","Soil_temperature","BGBC","NPP","MBC","MBN","MBC_MBN")    
agro_F_normalized.dat <- as.data.frame(agro_F_normalized.dat)   

fit_agro_F <- lm(F_necromass_C_SOC ~ Longitude+Latitude+Elevation+MAT+MAP+Clay+pH+SOC+TN+SOC_TN+Soil_temperature+BGBC+NPP+MBC+MBN+MBC_MBN, data = agro_F_normalized.dat)
vif(fit_agro_F)      

fit_agro_F <- lm(F_necromass_C_SOC ~ Longitude+Elevation+MAT+MAP+Clay+pH+SOC+SOC_TN+BGBC+NPP+MBC+MBC_MBN, data = agro_F_normalized.dat)
vif(fit_agro_F)

natural_F_normalized.dat <- scale(natural_F_necromass.dat)  
colnames(natural_F_normalized.dat) <- c("F_necromass_C_SOC","Longitude","Latitude","Elevation","MAT","MAP","Clay","pH","SOC","TN","SOC_TN","Soil_temperature","BGBC","NPP","MBC","MBN","MBC_MBN")    
natural_F_normalized.dat <- as.data.frame(natural_F_normalized.dat)   

fit_natural_F <- lm(F_necromass_C_SOC ~ Longitude+Latitude+Elevation+MAT+MAP+Clay+pH+SOC+TN+SOC_TN+Soil_temperature+BGBC+NPP+MBC+MBN+MBC_MBN, data = natural_F_normalized.dat)
vif(fit_natural_F) 

fit_natural_F <- lm(F_necromass_C_SOC ~ Longitude+Elevation+MAT+MAP+Clay+pH+SOC+SOC_TN+BGBC+NPP+MBC+MBC_MBN, data = natural_F_normalized.dat)
vif(fit_natural_F)

#B
agro_B_normalized.dat <- scale(agro_B_necromass.dat)  
colnames(agro_B_normalized.dat) <- c("B_necromass_C_SOC","Longitude","Latitude","Elevation","MAT","MAP","Clay","pH","SOC","TN","SOC_TN","Soil_temperature","BGBC","NPP","MBC","MBN","MBC_MBN")    
agro_B_normalized.dat <- as.data.frame(agro_B_normalized.dat)   

fit_agro_B <- lm(B_necromass_C_SOC ~ Longitude+Latitude+Elevation+MAT+MAP+Clay+pH+SOC+TN+SOC_TN+Soil_temperature+BGBC+NPP+MBC+MBN+MBC_MBN, data = agro_B_normalized.dat)
vif(fit_agro_B)      

fit_agro_B <- lm(B_necromass_C_SOC ~ Longitude+Elevation+MAT+MAP+Clay+pH+SOC+SOC_TN+BGBC+NPP+MBC+MBC_MBN, data = agro_B_normalized.dat)
vif(fit_agro_B)

natural_B_normalized.dat <- scale(natural_B_necromass.dat)  
colnames(natural_B_normalized.dat) <- c("B_necromass_C_SOC","Longitude","Latitude","Elevation","MAT","MAP","Clay","pH","SOC","TN","SOC_TN","Soil_temperature","BGBC","NPP","MBC","MBN","MBC_MBN")    
natural_B_normalized.dat <- as.data.frame(natural_B_normalized.dat)   

fit_natural_B <- lm(B_necromass_C_SOC ~ Longitude+Latitude+Elevation+MAT+MAP+Clay+pH+SOC+TN+SOC_TN+Soil_temperature+BGBC+NPP+MBC+MBN+MBC_MBN, data = natural_B_normalized.dat)
vif(fit_natural_B) 

fit_natural_B <- lm(B_necromass_C_SOC ~ Longitude+Elevation+MAT+MAP+Clay+pH+SOC+SOC_TN+BGBC+NPP+MBC+MBC_MBN, data = natural_B_normalized.dat)
vif(fit_natural_B) 


#plot for VIF
VIF_agro_FB.dat <- read.csv("VIF_agro_FB.csv")               
VIF_natural_FB.dat <- read.csv("VIF_natural_FB.csv")    
VIF_agro_F.dat <- read.csv("VIF_agro_F.csv") 
VIF_natural_F.dat <- read.csv("VIF_natural_F.csv")
VIF_agro_B.dat <- read.csv("VIF_agro_B.csv") 
VIF_natural_B.dat <- read.csv("VIF_natural_B.csv")


library(ggplot2)
library(ggpubr)

#FB
#agro
VIF_agro_FB.dat$kind<-factor(VIF_agro_FB.dat$kind)  
ggdotchart(VIF_agro_FB.dat, x = "variable", y ="VIF",
           color = "kind",                                
           palette = c("#c7b1d5", "#f28d61", "#8fa0cb","#76c2a4"), 
           size = 15,
           dot.size = 20,
           sorting = "descending",                        
           add = "segments",                             
           ggtheme = theme_pubr(),                        
           xlab="")+
  ylim(c(0,4))+
  geom_hline(yintercept = 3.3, linetype="dashed" ,color = 'red', size =2)

#natural
VIF_natural_FB.dat$kind<-factor(VIF_natural_FB.dat$kind)  
ggdotchart(VIF_natural_FB.dat, x = "variable", y ="VIF",
           color = "kind",                                
           palette = c("#c7b1d5", "#f28d61", "#8fa0cb","#76c2a4"), 
           size = 15,
           dot.size = 20,
           sorting = "descending",                        
           add = "segments",                             
           ggtheme = theme_pubr(),                        
           xlab="")+
  ylim(c(0,4))+
  geom_hline(yintercept = 3.3, linetype="dashed" ,color = 'red', size =2)


#F
#agro
VIF_agro_F.dat$kind<-factor(VIF_agro_F.dat$kind)  
ggdotchart(VIF_agro_F.dat, x = "variable", y ="VIF",
           color = "kind",                                
           palette = c("#c7b1d5", "#f28d61", "#8fa0cb","#76c2a4"), 
           size = 15,
           dot.size = 20,
           sorting = "descending",                        
           add = "segments",                             
           ggtheme = theme_pubr(),                        
           xlab="")+
  ylim(c(0,4))+
  geom_hline(yintercept = 3.3, linetype="dashed" ,color = 'red', size =2)

#natural
VIF_natural_F.dat$kind<-factor(VIF_natural_F.dat$kind)  
ggdotchart(VIF_natural_F.dat, x = "variable", y ="VIF",
           color = "kind",                                
           palette = c("#c7b1d5", "#f28d61", "#8fa0cb","#76c2a4"), 
           size = 15,
           dot.size = 20,
           sorting = "descending",                        
           add = "segments",                             
           ggtheme = theme_pubr(),                        
           xlab="")+
  ylim(c(0,4))+
  geom_hline(yintercept = 3.3, linetype="dashed" ,color = 'red', size =2)

#B
#agro
VIF_agro_B.dat$kind<-factor(VIF_agro_B.dat$kind)  量
ggdotchart(VIF_agro_B.dat, x = "variable", y ="VIF",
           color = "kind",                                
           palette = c("#c7b1d5", "#f28d61", "#8fa0cb","#76c2a4"), 
           size = 15,
           dot.size = 20,
           sorting = "descending",                        
           add = "segments",                             
           ggtheme = theme_pubr(),                        
           xlab="")+
  ylim(c(0,4))+
  geom_hline(yintercept = 3.3, linetype="dashed" ,color = 'red', size =2)

#natural
VIF_natural_B.dat$kind<-factor(VIF_natural_B.dat$kind)  
ggdotchart(VIF_natural_B.dat, x = "variable", y ="VIF",
           color = "kind",                                
           palette = c("#c7b1d5", "#f28d61", "#8fa0cb","#76c2a4"), 
           size = 15,
           dot.size = 20,
           sorting = "descending",                        
           add = "segments",                             
           ggtheme = theme_pubr(),                        
           xlab="")+
  ylim(c(0,4))+
  geom_hline(yintercept = 3.3, linetype="dashed" ,color = 'red', size =2)