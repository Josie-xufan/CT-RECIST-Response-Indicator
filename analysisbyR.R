#Figure5 water full plot--------------------------------------------------------------------------
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(dplyr)
library(knitr)
library(rio)
library("readxl")

best_volume_cutoff = -0.6867

# waterplot for chemotherapy
data <- read_excel("./data/D_V_35patients.xlsx", sheet = "chemo")
data[data$MPR_label!='nonMPR',]$MPR_label="MPRs"
data[data$MPR_label=='nonMPR',]$MPR_label="Non-MPRs"
data = data[order(data$tumor_decrease, decreasing = T),]

col <- ifelse(data$MPR_label == "MPRs", "#BC5A42","#009296")
svg(file= "./figures/figure5/chemo_MPR_decrease.svg",width=8, height=8)
barplot(data$tumor_decrease, 
        col=col, 
        border=col, 
        space=0.5,
        names.arg = 1:15,
        ylab="The decrease of the residual viable tumor(%)",
        xlab='15 Patients following Neoadjuvant chemotherapy',
        cex.axis=1.5,
        cex.lab=1,
        axis.lty = 1,
        ylim = c(-105,10),
        legend.text = c("MPR", "Non-MPR"),
        args.legend = list(x=6.5, 
                           y=-75,
                           title="Pathological Response", 
                           fill=c("#BC5A42","#009296"), 
                           border=NA, 
                           cex=0.8,
                           inset = c(0,0)))+
  abline(h=-90, col = "black", lwd=2)+
  text(x=seq(1,29.5,by=1.5)+0.2, y=(data$tumor_decrease)/2-2, labels = data$tumor_decrease, pos=3,srt = 90)+
  text(x=5.5, y=-94, labels = "MPR: ¡Ü10% residual viable tumor")
dev.off()


col <- ifelse(data$delta_diameter_3D < -0.3, "#BC5A42", "#009296")
svg(file= "./figures/figure5/chemo_diameter.svg",width=8, height=8) 
barplot(data$temp, 
        col=col, 
        border=col, 
        space=0.5,
        xaxt="n",
        yaxt="n",
        cex.axis=1.5,
        cex.lab=1.5
)+text(x=seq(1,29.5,by=1.5)+0.3, y=(data$temp)/2, 
       labels = round(data$delta_diameter_3D, 4), pos=3, srt = 90)
dev.off()

col <- ifelse(data$delta_volume < -0.3, "#BC5A42","#009296")
svg(file= "./figures/figure5/chemo_volume.svg",width=8, height=8)
barplot(data$temp, 
        col=col, 
        border=col, 
        space=0.5,
        xaxt="n",
        yaxt="n",
        cex.axis=1.5,
        cex.lab=1.5
)+text(x=seq(1,29.5,by=1.5)+0.3, y=(data$temp)/2, 
       labels = round(data$delta_volume, 4), pos=3, srt = 90)
dev.off()

col <- ifelse(data$delta_volume < best_volume_cutoff, "#BC5A42","#009296")
svg(file= "./figures/figure5/chemo_volume_bestcutoff.svg",width=8, height=8)
barplot(data$temp, 
        col=col, 
        border=col, 
        space=0.5,
        xaxt="n",
        yaxt="n",
        cex.axis=1.5,
        cex.lab=1.5
)+text(x=seq(1,29.5,by=1.5)+0.3, y=(data$temp)/2, 
       labels = round(data$delta_volume, 4), pos=3, srt = 90)
dev.off()


# waterplot for immunotherapy
data <- read_excel("./data/D_V_35patients.xlsx", sheet = "immun")
data[data$MPR_label!='nonMPR',]$MPR_label="MPRs"
data[data$MPR_label=='nonMPR',]$MPR_label="Non-MPRs"
data = data[order(data$tumor_decrease, decreasing = T),]

col <- ifelse(data$MPR_label == "MPRs", "#BC5A42","#009296")
svg(file= "./figures/figure5/immun_MPR_decrease.svg",width=8, height=8) #
barplot(data$tumor_decrease, 
        col=col, 
        border=col, 
        space=0.5,
        names.arg = 1:20,
        ylab="The decrease of the residual viable tumor(%)",
        xlab='20 Patients following Neoadjuvant immunotherapy',
        cex.axis=1.5,
        cex.lab=1,
        axis.lty = 1,
        ylim = c(-105,10),
        legend.text = c("MPR", "Non-MPR"),
        args.legend = list(x=6.5, 
                           y=-75,
                           title="Pathological Response", 
                           fill=c("#BC5A42","#009296"), 
                           border=NA, 
                           cex=0.8,
                           inset = c(0,0)))+
  abline(h=-90, col = "black", lwd=2)+
  text(x=seq(1,29.5,by=1.5)+0.2, y=(data$tumor_decrease)/2-2, labels = data$tumor_decrease, pos=3,srt = 90)+
  text(x=5.5, y=-94, labels = "MPR: ¡Ü10% residual viable tumor")
dev.off()


col <- ifelse(data$delta_diameter_3D < -0.3, "#BC5A42", "#009296")
svg(file= "./figures/figure5/immun_diameter.svg",width=8, height=8) 
barplot(data$temp, 
        col=col, 
        border=col, 
        space=0.5,
        xaxt="n",
        yaxt="n",
        cex.axis=1.5,
        cex.lab=1.5
)+text(x=seq(1,29.5,by=1.5)+0.3, y=(data$temp)/2, 
       labels = round(data$delta_diameter_3D, 4), pos=3, srt = 90)
dev.off()

col <- ifelse(data$delta_volume < -0.3, "#BC5A42","#009296")
svg(file= "./figures/figure5/immun_volume.svg",width=8, height=8)
barplot(data$temp, 
        col=col, 
        border=col, 
        space=0.5,
        xaxt="n",
        yaxt="n",
        cex.axis=1.5,
        cex.lab=1.5
)+text(x=seq(1,29.5,by=1.5)+0.3, y=(data$temp)/2, 
       labels = round(data$delta_volume, 4), pos=3, srt = 90)
dev.off()

col <- ifelse(data$delta_volume < best_volume_cutoff, "#BC5A42","#009296")
svg(file= "./figures/figure5/immun_volume_bestcutoff.svg",width=8, height=8)
barplot(data$temp, 
        col=col, 
        border=col, 
        space=0.5,
        xaxt="n",
        yaxt="n",
        cex.axis=1.5,
        cex.lab=1.5
)+text(x=seq(1,29.5,by=1.5)+0.3, y=(data$temp)/2, 
       labels = round(data$delta_volume, 4), pos=3, srt = 90)
dev.off()

# Figure6 survival analysis--------------------------------------------------------------------------
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library("openxlsx")
library(survival)
library(survminer)
library(rio)
library(TSHRC)
library(dplyr)

data_root = './data'
survival_data_path = paste(data_root, "survival_record.xlsx", sep = "/")
D_V_results_path = paste(data_root, "D_V_all.csv", sep = "/")

survival_data = read.xlsx(survival_data_path, sheet = 'Sheet1')
survival_data$status = 1-survival_data$final_result
D_V_results = read.csv(D_V_results_path)


for(i in 1: dim(survival_data)[1]){
  p_id = survival_data$patient.reindex[i]
  t1 = strsplit(survival_data[survival_data$patient.reindex==p_id, ]$patient_id_initial, "_")[[1]][3]
  if(length(strsplit(survival_data[survival_data$patient.reindex==p_id, ]$patient_id_initial, "_")[[1]])==4){
    t1 = paste(t1, strsplit(survival_data[survival_data$patient.reindex==p_id, ]$patient_id_initial, "_")[[1]][4], sep="_")
  }
  
  t2 = strsplit(survival_data[survival_data$patient.reindex==p_id, ]$`early_treatment_cycle`, "_")[[1]][3]
  if(length(strsplit(survival_data[survival_data$patient.reindex==p_id, ]$`early_treatment_cycle`, "_")[[1]])==4){
    t2 = paste(t2, strsplit(survival_data[survival_data$patient.reindex==p_id, ]$`early_treatment_cycle`, "_")[[1]][4], sep="_")
  }
  
  t1_D = subset(D_V_results, D_V_results$patient_id==p_id & D_V_results$time==t1)$original_shape_Maximum3DDiameter
  t1_V = subset(D_V_results, D_V_results$patient_id==p_id & D_V_results$time==t1)$original_shape_MeshVolume
  t2_D = subset(D_V_results, D_V_results$patient_id==p_id & D_V_results$time==t2)$original_shape_Maximum3DDiameter
  t2_V = subset(D_V_results, D_V_results$patient_id==p_id & D_V_results$time==t2)$original_shape_MeshVolume
  delta_D = (t2_D-t1_D)/t1_D
  delta_V = (t2_V-t1_V)/t1_V
  
  survival_data[survival_data$patient.reindex==p_id,]$delta_D = delta_D
  survival_data[survival_data$patient.reindex==p_id,]$delta_V = delta_V
  
}
survival_data = select(survival_data, time, delta_D, delta_V, status)
survival_data[survival_data$delta_D>= -0.3,]$delta_D=1
survival_data[survival_data$delta_D< -0.3,]$delta_D=0
survival_data[survival_data$delta_V>= -0.3,]$delta_V=1
survival_data[survival_data$delta_V< -0.3,]$delta_V=0 
colnames(survival_data)[2] = '¦¤D'
colnames(survival_data)[3] = '¦¤V'
svg(file= "./figures/figure6.svg",width=16, height=8)

attach(survival_data)
fit <- survfit(Surv(time,status) ~ ¦¤D, data = survival_data)
pv <- twostage(survival_data$time,survival_data$status, survival_data$¦¤D, nboot = 100)
p_surv_D<-ggsurvplot(fit, data = survival_data, pval = TRUE)+
  labs(x="Time", y="Proportion of survival",
       title = "RECIST(Diameter)")

attach(survival_data)
Surv(time,status)
fit <- survfit(Surv(time,status) ~ ¦¤V, data = survival_data)
pv <- twostage(survival_data$time,survival_data$status, survival_data$¦¤V, nboot = 100)
p_surv_V<-ggsurvplot(fit, data = survival_data, pval = TRUE)+
  labs(x="Time", y="Proportion of survival",
       title = "RECIST(Volume)")

p<-ggarrange(p_surv_D$plot, p_surv_V$plot, nrow = 1, ncol = 2) #labels = c('A','B'), 
annotate_figure(p, top = text_grob("Kaplan-Meier survival curve",color = "black", size = 14))
dev.off()

# Table3 regression analysis--------------------------------------------------------------------------
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(rio)
library("pscl")
library(ggplot2)

# chemotherapy
data <- read_excel("./data/D_V_35patients.xlsx", sheet = "chemo")
data[data$MPR_label!='nonMPR',]$MPR_label='1' #"MPR"
data[data$MPR_label=='nonMPR',]$MPR_label='0' #"Non-MPR"
data$MPR_label=as.numeric(data$MPR_label)
data = data[order(data$tumor_decrease, decreasing = T),]

cor_volume = cor(data$delta_volume,data$tumor_decrease,method = "pearson")
cor_3d_diameter = cor(data$delta_diameter_3D,data$tumor_decrease,method = "pearson")


model_delta_volume<-lm(formula = residual_tumor ~ delta_volume, data = data)
summary(model_delta_volume)
model = model_delta_volume
cor_value = cor_volume
l_delta_volume <- list(b = as.numeric(format(coef(model)[1], digits = 4)),
                       a = as.numeric(format(coef(model)[2], digits = 4)),
                       r2 = format(summary(model)$r.squared, digits = 4),
                       p = format(summary(model)$coefficients[2,4], digits = 4),
                       c = format(cor_value, digits = 4))

model_delta_diameter_3D<-lm(formula = residual_tumor ~ delta_diameter_3D, data = data)
summary(model_delta_diameter_3D)
model = model_delta_diameter_3D
cor_value = cor_3d_diameter
l_delta_diameter_3D <- list(b = as.numeric(format(coef(model)[1], digits = 4)),
                            a = as.numeric(format(coef(model)[2], digits = 4)),
                            r2 = format(summary(model)$r.squared, digits = 4),
                            p = format(summary(model)$coefficients[2,4], digits = 4),
                            c = format(cor_value, digits = 4))

# immunotherapy
data <- read_excel("./data/D_V_35patients.xlsx", sheet = "immun")
data[data$MPR_label!='nonMPR',]$MPR_label='1' #"MPR"
data[data$MPR_label=='nonMPR',]$MPR_label='0' #"Non-MPR"
data$MPR_label=as.numeric(data$MPR_label)
data = data[order(data$tumor_decrease, decreasing = T),]

cor_volume = cor(data$delta_volume,data$tumor_decrease,method = "pearson")
cor_3d_diameter = cor(data$delta_diameter_3D,data$tumor_decrease,method = "pearson")


model_delta_volume<-lm(formula = residual_tumor ~ delta_volume, data = data)
summary(model_delta_volume)
model = model_delta_volume
cor_value = cor_volume
l_delta_volume <- list(b = as.numeric(format(coef(model)[1], digits = 4)),
                       a = as.numeric(format(coef(model)[2], digits = 4)),
                       r2 = format(summary(model)$r.squared, digits = 4),
                       p = format(summary(model)$coefficients[2,4], digits = 4),
                       c = format(cor_value, digits = 4))

model_delta_diameter_3D<-lm(formula = residual_tumor ~ delta_diameter_3D, data = data)
summary(model_delta_diameter_3D)
model = model_delta_diameter_3D
cor_value = cor_3d_diameter
l_delta_diameter_3D <- list(b = as.numeric(format(coef(model)[1], digits = 4)),
                            a = as.numeric(format(coef(model)[2], digits = 4)),
                            r2 = format(summary(model)$r.squared, digits = 4),
                            p = format(summary(model)$coefficients[2,4], digits = 4),
                            c = format(cor_value, digits = 4))

# combine
data <- read_excel("./data/D_V_35patients.xlsx", sheet = "combine")
data[data$MPR_label!='nonMPR',]$MPR_label='1' #"MPR"
data[data$MPR_label=='nonMPR',]$MPR_label='0' #"Non-MPR"
data$MPR_label=as.numeric(data$MPR_label)
data = data[order(data$tumor_decrease, decreasing = T),]

cor_volume = cor(data$delta_volume,data$tumor_decrease,method = "pearson")
cor_3d_diameter = cor(data$delta_diameter_3D,data$tumor_decrease,method = "pearson")


model_delta_volume<-lm(formula = residual_tumor ~ delta_volume, data = data)
summary(model_delta_volume)
model = model_delta_volume
cor_value = cor_volume
l_delta_volume <- list(b = as.numeric(format(coef(model)[1], digits = 4)),
                       a = as.numeric(format(coef(model)[2], digits = 4)),
                       r2 = format(summary(model)$r.squared, digits = 4),
                       p = format(summary(model)$coefficients[2,4], digits = 4),
                       c = format(cor_value, digits = 4))

model_delta_diameter_3D<-lm(formula = residual_tumor ~ delta_diameter_3D, data = data)
summary(model_delta_diameter_3D)
model = model_delta_diameter_3D
cor_value = cor_3d_diameter
l_delta_diameter_3D <- list(b = as.numeric(format(coef(model)[1], digits = 4)),
                            a = as.numeric(format(coef(model)[2], digits = 4)),
                            r2 = format(summary(model)$r.squared, digits = 4),
                            p = format(summary(model)$coefficients[2,4], digits = 4),
                            c = format(cor_value, digits = 4))

