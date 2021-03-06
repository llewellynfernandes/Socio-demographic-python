data <-read.csv(file.choose(),header=TRUE)
attach(data)
 summary(data)
library(dplyr)
###Percentile norms
M <- filter(data, Gender == "M")
F <- filter(data, Gender == "F")
attach(F)
count(F)
spm_F<-data.frame(spm1,spm2,spm3,spm4,spm5,spm6,spm7,spm8,spm9,spm10,spm11,spm12,spm13,spm14,spm15,spm16,spm17,spm18,spm19,spm20,spm21,spm22,spm23,spm24,spm25,spm26,spm27,spm28,spm29,spm30,spm31,spm32,spm33,spm34,spm35,spm36,spm37,spm38,spm39,spm40,spm41,spm42,spm43,spm44,spm45,spm46,spm47,spm48,spm49,spm50,spm51,spm52,spm53,spm54,spm55,spm56,spm57,spm58,spm59,spm60)
F_total <- rowSums(spm_F)
F_norms <-quantile(F_total, probs = seq(0, 1, 0.05), na.rm = T,names = TRUE, type = 7)
F_norms
M <- filter(data, Age == "21")
MA_21 <- filter(data, Age == "21")
attach(MA_21)
spm_21 <- data.frame(spm1,spm2,spm3,spm4,spm5,spm6,spm7,spm8,spm9,spm10,spm11,spm12,spm13,spm14,spm15,spm16,spm17,spm18,spm19,spm20,spm21,spm22,spm23,spm24,spm25,spm26,spm27,spm28,spm29,spm30,spm31,spm32,spm33,spm34,spm35,spm36,spm37,spm38,spm39,spm40,spm41,spm42,spm43,spm44,spm45,spm46,spm47,spm48,spm49,spm50,spm51,spm52,spm53,spm54,spm55,spm56,spm57,spm58,spm59,spm60)
total21 <-rowSums(spm_21)
norms21 <-quantile(total21, probs = seq(0,1,0.05), na.ram = T, names = TRUE, type = 7)
norms21
MA_21 <- filter(data, Age > 21)
MA_F_21 <- filter(data, Age == "21", Gender == "F")
MA_age <- filter(data, Age <23 & Age > 20)
attach(MA_age)
count(MA_age)
??filter