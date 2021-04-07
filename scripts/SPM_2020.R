data <-read.csv(file.choose(),header=T)
attach(data)
summary(data)

##Libraries required
library(psych)
library(psychometric)
library(dplyr)


###SPM scoring


spm <- data.frame(spm1,spm2,spm3,spm4,spm5,spm6,spm7,spm8,spm9,spm10,spm11,spm12,
spm13,spm14,spm15,spm16,spm17,spm18,spm19,spm20,spm21,spm22,spm23,spm24,
spm25,spm26,spm27,spm28,spm29,spm30,spm31,spm32,spm33,spm34,spm35,spm36,
spm37,spm38,spm39,spm40,spm41,spm42,spm43,spm44,spm45,spm46,spm47,spm48,
spm49,spm50,spm51,spm52,spm53,spm54,spm55,spm56,spm57,spm58,spm59,spm60)


total_spm <- rowSums(spm)

###Correlation (Validity)

cor.test(total_spm, totalar) #SPM and DAT-AR

###Reliability


##KR20

kr20 <- function(df) {
k <- ncol(df)
k1 <- (k-1)
x <- (k/k1)
p <- colMeans(df)
q <- 1 - p
pq <- (p*q)
sumpq <- sum(pq)
total <- rowSums(df)
var <- var(total)
y <- (sumpq/var)
z <- (1-y)
reliability <- (x*z)
return(reliability)
}

kr20(spm)


##Item Analysis
#item total correlation, item difficulty, item discrimination

item.exam(spm, spm, discrim = TRUE)  


###Factor Analysis


##Principal components

fit <- principal(spm, nfactors = 60, residuals = FALSE, rotate = "none", n.obs = 60)


#deleted items spm1 spm2 spm4 spm5 spm6 spm13 spm14 spm15 spm16 spm25 spm27

spm_fa<-data.frame(spm3,spm7,spm8,spm9,spm10,spm11,spm12,
spm17,spm18,spm19,spm20,spm21,spm22,spm23,spm24,
spm26,spm28,spm29,spm30,spm31,spm32,spm33,spm34,spm35,spm36,
spm37,spm38,spm39,spm40,spm41,spm42,spm43,spm44,spm45,spm46,spm47,spm48,
spm49,spm50,spm51,spm52,spm53,spm54,spm55,spm56,spm57,spm58,spm59,spm60)

fit1 <- principal(spm_fa, nfactors = 49, residuals = FALSE, rotate = "none", n.obs = 60)
fit1

#Scree Plot

eigen <- fit1$values
pc_number <- 1:49
plot(pc_number, eigen, pch = 20, cex = 3, cex.lab = 2, 
cex.axis = 2, main = "The Scree Plot", type = "b")

#Parallel Analysis 

fa.parallel(spm_fa,n.obs=60, fa = "pc", n.iter = 1000)


#PCA with 4 components

fit2 <- principal(spm_fa, nfactors = 4, residuals = FALSE, rotate = "none", n.obs = 60)
fit2

#PCA - varimax rotation
fit3 <- principal(spm_fa, nfactors = 4, rotate = "varimax")
fit3


###Percentile norms


#Female

F <- filter(data, Gender == "F")
attach(F)
count(F)

spm_F<-data.frame(spm1,spm2,spm3,spm4,spm5,spm6,spm7,spm8,spm9,spm10,spm11,spm12,
spm13,spm14,spm15,spm16,spm17,spm18,spm19,spm20,spm21,spm22,spm23,spm24,
spm25,spm26,spm27,spm28,spm29,spm30,spm31,spm32,spm33,spm34,spm35,spm36,
spm37,spm38,spm39,spm40,spm41,spm42,spm43,spm44,spm45,spm46,spm47,spm48,
spm49,spm50,spm51,spm52,spm53,spm54,spm55,spm56,spm57,spm58,spm59,spm60)


F_total <- rowSums(spm_F)

F_norms <- quantile(F_total, probs = seq(0, 1, 0.05), na.rm = T,names = TRUE, type = 7)

F_norms

#Male
M <- filter(data, Gender == "M")
attach(M)
nrow(M)

spm_M<-data.frame(spm1,spm2,spm3,spm4,spm5,spm6,spm7,spm8,spm9,spm10,spm11,spm12,
spm13,spm14,spm15,spm16,spm17,spm18,spm19,spm20,spm21,spm22,spm23,spm24,
spm25,spm26,spm27,spm28,spm29,spm30,spm31,spm32,spm33,spm34,spm35,spm36,
spm37,spm38,spm39,spm40,spm41,spm42,spm43,spm44,spm45,spm46,spm47,spm48,
spm49,spm50,spm51,spm52,spm53,spm54,spm55,spm56,spm57,spm58,spm59,spm60)

M_total <- rowSums(spm_M)

M_norms<-quantile(M_total, probs = seq(0, 1, 0.05), na.rm = T,names = TRUE, type = 7)

M_norms

#Age

MA_21 <- filter(data, Age == "21")
attach(MA_21)

spm_21<-data.frame(spm1,spm2,spm3,spm4,spm5,spm6,spm7,spm8,spm9,spm10,spm11,spm12,
spm13,spm14,spm15,spm16,spm17,spm18,spm19,spm20,spm21,spm22,spm23,spm24,
spm25,spm26,spm27,spm28,spm29,spm30,spm31,spm32,spm33,spm34,spm35,spm36,
spm37,spm38,spm39,spm40,spm41,spm42,spm43,spm44,spm45,spm46,spm47,spm48,
spm49,spm50,spm51,spm52,spm53,spm54,spm55,spm56,spm57,spm58,spm59,spm60)

total21 <- rowSums(spm_21)

norms21 <- quantile(total21, probs = seq(0, 1, 0.05), na.rm = T,names = TRUE, type = 7)

norms21

#Age greater than/lesser than

MA_21 <- filter(data, Age > 21)
attach(MA_21)

spm_21<-data.frame(spm1,spm2,spm3,spm4,spm5,spm6,spm7,spm8,spm9,spm10,spm11,spm12,
spm13,spm14,spm15,spm16,spm17,spm18,spm19,spm20,spm21,spm22,spm23,spm24,
spm25,spm26,spm27,spm28,spm29,spm30,spm31,spm32,spm33,spm34,spm35,spm36,
spm37,spm38,spm39,spm40,spm41,spm42,spm43,spm44,spm45,spm46,spm47,spm48,
spm49,spm50,spm51,spm52,spm53,spm54,spm55,spm56,spm57,spm58,spm59,spm60)

total21 <- rowSums(spm_21)

norms21 <-quantile(total21, probs = seq(0, 1, 0.05), na.rm = T,names = TRUE, type = 7)

norms21


###Multiple filters

#using comma(,) as an operatior

MA_F_21 <- filter(data, Age == "21", Gender == "F")
attach(MA_F_21)

spm_F_21<-data.frame(spm1,spm2,spm3,spm4,spm5,spm6,spm7,spm8,spm9,spm10,spm11,spm12,
spm13,spm14,spm15,spm16,spm17,spm18,spm19,spm20,spm21,spm22,spm23,spm24,
spm25,spm26,spm27,spm28,spm29,spm30,spm31,spm32,spm33,spm34,spm35,spm36,
spm37,spm38,spm39,spm40,spm41,spm42,spm43,spm44,spm45,spm46,spm47,spm48,
spm49,spm50,spm51,spm52,spm53,spm54,spm55,spm56,spm57,spm58,spm59,spm60)

total_F_21 <- rowSums(spm_F_21)

norms_F_21 <- quantile(total_F_21, probs = seq(0, 1, 0.05), na.rm = T,names = TRUE, type = 7)

norms_F_21


#using ampersand(&) as an operator

MA_age <- filter(data, Age < 23 & Age > 20)
attach(MA_age)

MA_age<-data.frame(spm1,spm2,spm3,spm4,spm5,spm6,spm7,spm8,spm9,spm10,spm11,spm12,
spm13,spm14,spm15,spm16,spm17,spm18,spm19,spm20,spm21,spm22,spm23,spm24,
spm25,spm26,spm27,spm28,spm29,spm30,spm31,spm32,spm33,spm34,spm35,spm36,
spm37,spm38,spm39,spm40,spm41,spm42,spm43,spm44,spm45,spm46,spm47,spm48,
spm49,spm50,spm51,spm52,spm53,spm54,spm55,spm56,spm57,spm58,spm59,spm60)

total_MA_age <- rowSums(MA_age)

norms_MA_age <- quantile(total_MA_age, probs = seq(0, 1, 0.05), na.rm = T,names = TRUE, type = 7)

norms_MA_age