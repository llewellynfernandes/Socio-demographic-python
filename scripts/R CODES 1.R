data <- read.csv(file.choose())
attach(data)
names(data) 

################################
         NEO-FFI scoring
################################
# N
neof1 <- (6-neof1)
neof16 <- (6-neof16)
neof31 <- (6-neof31)
neof46 <- (6-neof46)

N_neof_items <-data.frame(neof1, neof6, neof11, neof16, neof21, neof26, neof31, neof36, neof41, neof46, neof51, neof56)

N_neof <- rowSums(N_neof_items)

psych::alpha(N_neof_items)

neof12 <- (6-neof12)
neof27 <- (6-neof27)
neof42 <- (6-neof42)
neof57 <- (6-neof57)

N_neof_items <-data.frame(neof2, neof7, neof12, neof17, neof22, neof27, neof32, neof37, neof42, neof47, neof52, neof57)
N_neof <- rowSums(N_neof_items)
psych::alpha(N_neof_items)

neof3 <-(6-neof3)
neof8 <-(6-neof8)
neof18 <-(6-neof18)
neof23 <-(6-neof23)
neof33 <-(6-neof33)
neof38 <-(6-neof38)
neof48 <-(6-neof48)

N_neof_items <-data.frame(neof3, neof8, neof13, neof18, neof23, neof28, neof33, neof38, neof43, neof48, neof53, neof58)
N_neof <- rowSums(N_neof_items)
psych::alpha(N_neof_items)

neof9 <-(6-neof9)
neof14 <-(6-neof14)
neof24 <-(6-neof24)
neof29 <-(6-neof29)
neof39 <-(6-neof39)
neof44 <-(6-neof44)
neof54 <-(6-neof54)
neof59 <-(6-neof59)

N_neof_items <-data.frame(neof4, neof9, neof14, neof19, neof24, neof29, neof34, neof39, neof44, neof49, neof54, neof59)
N_neof <- rowSums(N_neof_items)
psych::alpha(N_neof_items)

neof15 <-(6-neof15)
neof30 <-(6-neof30)
neof45 <-(6-neof45)
neof55 <-(6-neof55)

N_neof_items <-data.frame(neof5, neof10, neof15, neof20, neof25, neof30, neof35, neof40, neof45, neof50, neof55, neof60)
N_neof <- rowSums(N_neof_items)
psych::alpha(N_neof_items)

neof <-data.frame(neof1,neof2,neof3,neof4,neof5,neof6,neof7,neof8,neof9,neof10,neof11,neof12,neof13,neof14,neof15,neof16,neof17,neof18,neof19,neof20,neof21,neof22,neof23,neof24,neof25,neof26,neof27,neof28,neof29,neof30,neof31,neof32,neof33,neof34,neof35,neof36,neof37,neof38,neof39,neof40,neof41,neof42,neof43,neof44,neof45,neof46,neof47,neof48,neof49,neof50,neof51,neof52,neof53,neof54,neof55,neof56,neof57,neof58,neof59,neof60)
total_neof <- rowSums(neof)
total_neof
muffi <-data.frame(muffi1,muffi2,muffi3,muffi4,muffi5,muffi6,muffi7,muffi8,muffi9,muffi10,muffi11,muffi12,muffi13,muffi14,muffi15,muffi16,muffi17,muffi18,muffi19,muffi20,muffi21,muffi22,muffi23,muffi24,muffi25,muffi26,muffi27,muffi28,muffi29,muffi30,muffi31,muffi32,muffi33,muffi34,muffi35,muffi36,muffi37,muffi38,muffi39,muffi40,muffi41,muffi42,muffi43,muffi44,muffi45,muffi46,muffi47,muffi48,muffi49,muffi50)
total_muffi <- rowSums(muffi)
total_muffi
cor.test(total_neof,total_muffi)

bfi <-data.frame(bfi1,bfi2,bfi3,bfi4,bfi5,bfi6,bfi7,bfi8,bfi9,bfi10,bfi11,bfi12,bfi13,bfi14,bfi15,bfi16,bfi17,bfi18,bfi19,bfi20,bfi21,bfi22,bfi23,bfi24,bfi25,bfi26,bfi27,bfi28,bfi29,bfi30,bfi31,bfi32,bfi33,bfi34,bfi35,bfi36,bfi37,bfi38,bfi39,bfi40,bfi41,bfi42,bfi43,bfi44)
total_bfi <- rowSums(bfi)
total_bfi
cor.test(total_neof,total_bfi)





