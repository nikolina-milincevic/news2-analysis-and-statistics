### ANALIZA NEWS2 SCORE

# pretpostavljamo da uzorak dolazi iz slucajnog uzorka
# nemamo razloga sumnjati u to
# podatci su prikupljani tijekom odredjenog perioda
# u kojem su nasumicno (kako) odabrani pacijenti

# ucitaj podatke na klik i nazovi file dataset
attach(dataset)
summary(dataset)


spol <- as.factor(SPOL)
summary(spol)

# testiram hipotezu H_0: udio muskaraca = 1/2
# naspram alternativne H_1: udio muskaraca != 1/2
binom.test(length(spol[which(spol==1)]), length(spol), 1/2,
           alternative = "two.sided")


mjesto <- as.factor(MJESTO)
summary(mjesto)

# testiram hipotezu H_0: udio ljudi iz grada = 1/2
# naspram alternativne H_1: udio ljudi iz grada > 1/2
binom.test(length(mjesto[which(mjesto==1)]), length(mjesto), 1/2,
           alternative = "greater")



smrt <- as.factor(SMRT)
summary(smrt)


rizik_uk <- as.factor(RIZIK_UKUPNI)
summary(rizik_uk)

otpust <- as.factor(OTPUST)
summary(otpust)

rizik_p <- as.factor(RIZIK_POJEDINACNI)
summary(rizik_p)



summary(rizik_uk[which(otpust == 1)])
summary(rizik_uk[which(otpust == 2)])
# kako je rasporedjen rizik s obzirom na to da su pacijenti pusteni kuci (1)
# ili su upuceni u drugu ambulantu (2) sto je indikator toga da su 
# zapravo bili low risk
summary(rizik_uk[which(otpust %in% c(1,2))])
levels(rizik_uk) <- c("Niska", "Srednja", "Visoka")
barplot(table(rizik_uk[which(otpust %in% c(1,2))]), 
     main = "Pacijenti koji su pušteni kući ili upućeni u drugu ambulantu",
    xlab = "Kategorije rizičnosti prema NEWS2 skali", ylab = "Frekvencija", 
    ylim = c(0, 120))

barplot(table(rizik_uk[which(otpust %in% c(3))]), 
        main = "Pacijenti koji su hospitalizirani na odjelu minimalne njege",
        xlab = "Kategorije rizičnosti prema NEWS2 skali", ylab = "Frekvencija",
        ylim = c(0, 120))

barplot(table(rizik_uk[which(otpust %in% c(4, 5))]), 
        main = "Pacijenti koji su primljeni u jedinicu intenzivne skrbi ili su umrli",
        xlab = "Kategorije rizičnosti prema NEWS2 skali", ylab = "Frekvencija",
        ylim = c(0, 120))

barplot(table(otpust[which(rizik_uk %in% c("Visoka"))]), 
        main = ">> Gdje su pacijenti s visokim NEWS2 <<",
        xlab = "Otpust", ylab = "Frekvencija",
        ylim = c(0, 30))

barplot(table(otpust[which(rizik_uk %in% c("Srednja"))]), 
        main = ">> Gdje su pacijenti sa srednjim NEWS2 <<",
        xlab = "Otpust", ylab = "Frekvencija",
        ylim = c(0, 30))

barplot(table(otpust[which(rizik_uk %in% c("Niska"))]), 
        main = ">> Gdje su pacijenti s niskim NEWS2 <<",
        xlab = "Otpust", ylab = "Frekvencija",
        ylim = c(0, 100))



barplot(table(smrt[which(rizik_uk %in% c("Niska"))]), 
        main = ">> Smrt pacijenata s niskim NEWS2 <<",
        xlab = "Otpust", ylab = "Frekvencija",
        ylim = c(0, 200))

barplot(table(smrt[which(rizik_uk %in% c("Srednja"))]), 
        main = ">> Smrt pacijenata sa srednjim NEWS2 <<",
        xlab = "Otpust", ylab = "Frekvencija",
        ylim = c(0, 200))

barplot(table(smrt[which(rizik_uk %in% c("Visoka"))]), 
        main = ">> Smrt pacijenata s visokim NEWS2 <<",
        xlab = "Otpust", ylab = "Frekvencija",
        ylim = c(0, 200))

stvarni_rizik <- c()
for (i in 1:length(otpust)) {
  if (otpust[i] == 1 || otpust[i] == 2) {
    stvarni_rizik[i] <- "Niska"
  }
  else {
    if (otpust[i] == 3 || otpust[i] == 6) {
      stvarni_rizik[i] <- "Srednja"
    }
    else {
      stvarni_rizik[i] <- "Visoka"
    }
  }
}
stvarni_rizik <- as.factor(stvarni_rizik)
summary(stvarni_rizik)
length(otpust[which(otpust == 4)])

table(stvarni_rizik[which(rizik_uk == "Niska")])
table(stvarni_rizik[which(rizik_uk == "Srednja")])
table(stvarni_rizik[which(rizik_uk == "Visoka")])


# ZA LOW: (low = 1, risk = 0)

# ovo su oni koji su stvarno nisko rizicni i ukupni rizik ih je
# ispravno tako kategorizirao, oni su 1 i stavljeni su 1
a11 <- table(stvarni_rizik[which(rizik_uk == "Niska")])[[1]]

# ovo su oni su stvarno nisko rizicni, ali ih je ukupni rizik
# pogresno kategorizirao, oni su 1 a stavljeni su u 0
a10 <- table(stvarni_rizik[which(rizik_uk == "Srednja")])[[1]] +
    table(stvarni_rizik[which(rizik_uk == "Visoka")])[[1]]

# rizicni a procijenjeni kao da nisu
a01 <- table(stvarni_rizik[which(rizik_uk == "Niska")])[[2]] + 
      table(stvarni_rizik[which(rizik_uk == "Niska")])[[3]]

# rizicni i procijenjeni kao rizicni
a00 <- table(stvarni_rizik[which(rizik_uk == "Srednja")])[[2]] + 
    table(stvarni_rizik[which(rizik_uk == "Visoka")])[[2]] + 
    table(stvarni_rizik[which(rizik_uk == "Srednja")])[[3]] + 
    table(stvarni_rizik[which(rizik_uk == "Visoka")])[[3]] 

# koliko ih ima ukupno je na primjer length(otpust) 

low <- matrix(c(a00, a01, a10, a11), nrow = 2, byrow=TRUE)
low
 
# U ovom dijelu provjeravam koliko dobro test prediktira 
# NERIZICNE PACIJENTE

# osjetljivost (sensitivity) testa je udio bolesnih koje je test
# ispravno prepoznao kao pozitivne tj bolesne
# u nasem slucaju je to udio low risk pacijenata koje je news2 score
# ispravno prepoznao kao low risk pacijente, a to je:
(osjetljivost_low <- a11/(a11 + a10))

# specificnost (specificity) testa je udio zdravih koje je test
# ispravno prepoznao kao negativne, tj zdrave
# u nasem slucaju je to udio rizicnih koje je news2 score
# ispravno prepoznao kao rizicne pacijente
(specificnost_low <- a00/(a00 + a01))


(odds_ratio_low <- (a11*a00) / (a10*a01))
# interpretacija? 
# sanse da je pacijent nisko rizican ako pripada kategoriji nisko rizicnih
# su toliko puta vece nego ako bi pripadao kategoriji onih koji nisu nisko rizicni



# ZA MEDIUM: (medium = 1, nemedium je 0)

# ovo su oni koji su stvarno ili nerizicni ili visoko rizicni, 
# i ukupni rizik ih je tako kategorizirao, oni su 0 i stavljeni su 0
b00 <- table(stvarni_rizik[which(rizik_uk == "Niska")])[[1]] +
  table(stvarni_rizik[which(rizik_uk == "Niska")])[[3]] +
  table(stvarni_rizik[which(rizik_uk == "Visoka")])[[1]] +
  table(stvarni_rizik[which(rizik_uk == "Visoka")])[[3]]

# ovo su oni koji su stvarno ili nerizicni ili visoko rizicni, 
# ali ih je ukupni rizik pogresno kategorizirao i procijenio kao medium risk,
# oni su 0 a stavljeni su u 1
b01 <- table(stvarni_rizik[which(rizik_uk == "Srednja")])[[1]] +
  table(stvarni_rizik[which(rizik_uk == "Srednja")])[[3]]

# srednje rizicni a procijenjeni kao da nisu
b10 <- table(stvarni_rizik[which(rizik_uk == "Niska")])[[2]] + 
  table(stvarni_rizik[which(rizik_uk == "Visoka")])[[2]]

# srednje rizicni i procijenjeni kao srednje rizicni
b11 <- table(stvarni_rizik[which(rizik_uk == "Srednja")])[[2]] 

# koliko ih ima ukupno je na primjer length(otpust) 

medium <- matrix(c(b00, b01, b10, b11), nrow = 2, byrow=TRUE)
medium

# U ovom dijelu provjeravam koliko dobro test prediktira 
# SREDNJE RIZICNE PACIJENTE

# osjetljivost (sensitivity) testa je udio bolesnih koje je test
# ispravno prepoznao kao pozitivne tj bolesne
# u nasem slucaju je to udio medium risk pacijenata koje je news2 score
# ispravno prepoznao kao medium risk pacijente, a to je:
(osjetljivost_medium <- b11/(b11 + b10))

# specificnost (specificity) testa je udio zdravih koje je test
# ispravno prepoznao kao negativne, tj zdrave
# u nasem slucaju je to udio nerizicnih ili visoko rizicnih koje je news2 score
# ispravno prepoznao kao nerizicne ili visoko rizicne pacijente
(specificnost_medium <- b00/(b00 + b01))


(odds_ratio_medium <- (b11*b00) / (b10*b01))







# ZA High: (high = 1, nehigh je 0)

# ovo su oni koji su stvarno ili nerizicni ili srednje rizicni, 
# i ukupni rizik ih je tako kategorizirao, oni su 0 i stavljeni su 0
c00 <- table(stvarni_rizik[which(rizik_uk == "Niska")])[[1]] +
  table(stvarni_rizik[which(rizik_uk == "Niska")])[[2]] +
  table(stvarni_rizik[which(rizik_uk == "Srednja")])[[1]] +
  table(stvarni_rizik[which(rizik_uk == "Srednja")])[[2]]

# ovo su oni koji su stvarno ili nerizicni ili srednje rizicni, 
# ali ih je ukupni rizik pogresno kategorizirao i procijenio kao high risk,
# oni su 0 a stavljeni su u 1
c01 <- table(stvarni_rizik[which(rizik_uk == "Visoka")])[[1]] +
  table(stvarni_rizik[which(rizik_uk == "Visoka")])[[2]]

# visoko rizicni a procijenjeni kao da nisu
c10 <- table(stvarni_rizik[which(rizik_uk == "Niska")])[[3]] + 
  table(stvarni_rizik[which(rizik_uk == "Srednja")])[[3]]

# visoko rizicni i procijenjeni kao visoko rizicni
c11 <- table(stvarni_rizik[which(rizik_uk == "Visoka")])[[3]] 

# koliko ih ima ukupno je na primjer length(otpust) 

high <- matrix(c(c00, c01, c10, c11), nrow = 2, byrow=TRUE)
high



# U ovom dijelu provjeravam koliko dobro test prediktira 
# VISOKO RIZICNE PACIJENTE

# osjetljivost (sensitivity) testa je udio bolesnih koje je test
# ispravno prepoznao kao pozitivne tj bolesne
# u nasem slucaju je to udio high risk pacijenata koje je news2 score
# ispravno prepoznao kao high risk pacijente, a to je:
(osjetljivost_high <- c11/(c11 + c10))

# specificnost (specificity) testa je udio zdravih koje je test
# ispravno prepoznao kao negativne, tj zdrave
# u nasem slucaju je to udio nerizicnih ili srednje rizicnih koje je news2 score
# ispravno prepoznao kao nerizicne ili srednje rizicne pacijente
(specificnost_high <- c00/(c00 + c01))

(odds_ratio_high <- (c11*c00) / (c01*c10))



# H_0: stvarni rizik i procijenjeni ukupni rizik su nezavisni
# H_1: nisu nezavisni
chisq.test(stvarni_rizik, rizik_uk)





# izuzmimo pacijente kojima je razlog dolaska 
# 5 = mentalni poremecaji i poremecaji ponasanja
# 6 = bolesti zivcanog sustava

razlog <- as.factor(`RAZLOG/dg`)
summary(razlog)

'%!in%' <- function(x,y)!('%in%'(x,y))

rizik_uk <- as.factor(RIZIK_UKUPNI[which(razlog %!in% c(5, 6))])
levels(rizik_uk) <- c("Niska", "Srednja", "Visoka")
summary(rizik_uk)

stvarni_rizik <- stvarni_rizik[which(razlog %!in% c(5,6))]
summary(stvarni_rizik)
# prodji cijeli kod s ovim vrijednostima




# uzmimo sad u obzir i rizik_p 

summary(rizik_p)
table(stvarni_rizik)
table(stvarni_rizik[which(rizik_p == 0)])
table(stvarni_rizik[which(rizik_p == 1)])

novi_rizik <- c()
for (i in 1:length(rizik_p)) {
  if (rizik_p[i] == 1) {
    novi_rizik[i] <- 3
  }
  else{
    novi_rizik[i] <- rizik_uk[i]
  }
}
novi_rizik <- as.factor(novi_rizik)
summary(novi_rizik)
rizik_uk

length(rizik_uk[which(rizik_p == 1)])
length(rizik_uk[which(rizik_p == 0)])

# testiram H0: novi rizik i stvarni su nezavisni
chisq.test(novi_rizik, stvarni_rizik)


#####
# koliko dobro ova nova varijabla raspoznaje rizicnost?
# ova varijabla uzima u obzir je li u jednoj od kategorija skor velik
# odgovara na pitanje ima li pacijent pojedini skor nekog parametra >=3
# a sveukupni rizik nizak

# sada visok rizik = visok rizik kao ranije ili ima rizik_p = 1



# koliko dobro raspoznaje rizicne pacijente
# nerizican = 0, rizican = 1
d00 <- table(stvarni_rizik[which(novi_rizik == 1)])[[1]] 

d01 <- table(stvarni_rizik[which(novi_rizik == 3)])[[1]] 

d10 <- table(stvarni_rizik[which(novi_rizik == 1)])[[2]] + 
  table(stvarni_rizik[which(novi_rizik == 1)])[[3]]

d11 <- table(stvarni_rizik[which(novi_rizik == 3)])[[2]] +
  table(stvarni_rizik[which(novi_rizik == 3)])[[3]]

novi <- matrix(c(d00, d01, d10, d11), nrow = 2, byrow = TRUE)
novi
(osjetljivost_novi <- d11/(d11 + d10))
(specificnost_novi <- d00/(d00 + d01))

(odds_ratio_novi <- (d11*d00) / (d01*d10))








# idem sada pogledati ukupne bodove da bih odredila rizicnost, 
# umjesto koristenja varijable rizik_uk

rizik_s_bodovima <- c()
for (i in 1:length(UKUPNI_BODOVI_1)) {
  if (UKUPNI_BODOVI_1[i] >= 7) {
    rizik_s_bodovima[i] <- "Visoka"
  }
  else {
    rizik_s_bodovima[i] <- "Niska"
  }
}
table(rizik_s_bodovima)

# koliko dobro prediktira rizik ( = 1):
# stvarnu nerizicnost gledam kao nerizican ili srednje rizican 
# (hospitaliziran u odjelu min njege)

e00 <- table(stvarni_rizik[which(rizik_s_bodovima == "Niska")])[[1]] + 
  table(stvarni_rizik[which(rizik_s_bodovima == "Niska")])[[2]] 
e01 <- table(stvarni_rizik[which(rizik_s_bodovima == "Visoka")])[[1]] + 
  table(stvarni_rizik[which(rizik_s_bodovima == "Visoka")])[[2]]
e10 <- table(stvarni_rizik[which(rizik_s_bodovima == "Niska")])[[3]] 
e11 <- table(stvarni_rizik[which(rizik_s_bodovima == "Visoka")])[[3]]
(matrix(c(e00, e01, e10, e11), nrow = 2, byrow = TRUE))
(osjetljivost_s_bodovima <- e11/(e11 + e10))
(specificnost_s_bodovima <- e00/(e00 + e01))

(odds_ratio_s_bodovima <- (e11*e00) / (e01*e10))


vektor1 <- c()
for (i in 1:length(otpust)) {
  if (stvarni_rizik[i] == "Niska" | stvarni_rizik[i] == "Srednja") {
    vektor1[i] <- 0
  }
  else {
    vektor1[i] <- 1
  }
}

vektor2 <- c()
for (i in 1:length(otpust)) {
  if (rizik_s_bodovima[i] == "Niska") {
    vektor2[i] <- 0
  }
  else {
    vektor2[i] <- 1
  }
}

# Install and load the pROC package
install.packages("pROC")
library(pROC)



# GODINE
godine <- 2024 - DOB
summary(godine)
boxplot(godine)
IQR(godine)

godine_umrli <- godine[which(smrt==1)]
godine_neumrli <- godine[which(smrt==0)]

plot(density(godine_umrli), col="lightblue", lwd=2, main="",
     xlab = "Dob", ylab = "Gustoća")
lines(density(godine_neumrli), col="pink", lwd=2)
legend(30, 0.035, legend=c("Umrli", "Preživjeli"),
       col=c("lightblue", "pink"), lty=1:1, cex=0.8, lwd=2:2)

hist(godine_umrli, col="lightblue", lwd=2, main="", probability = TRUE,
     xlab = "", ylab = "")
hist(godine_neumrli, col="pink", lwd=2, main="", probability = TRUE,
     xlab = "", ylab = "")

# H0: variances are equal
# H1: variances differ
var.test(godine_umrli, godine_neumrli)
# H0: umrli su mladji od neumrlih
# H1: umrli su stariji od neumrlih
t.test(godine_umrli, godine_neumrli, alternative="greater", var.equal = FALSE)


# RAZLOG DOLASKA
summary(razlog)
plot(razlog)
summary(razlog[which(smrt==0)])


# ICU
icu <- as.factor(ICU)
summary(icu)
summary(icu[which(smrt==0)])
summary(icu[which(smrt==1)])
table(icu[which(smrt==0)])/length(icu[which(smrt==0)])
table(icu[which(smrt==1)])/length(icu[which(smrt==1)])
table(icu, smrt)
(metode <- matrix(c(198, 22, 41, 17), 
                  byrow = TRUE, nrow = 2))
rownames(metode) <- c("no icu", "icu")
colnames(metode) <- c("no smrt", "smrt")
metode

chisq.test(metode)


prop.test(x=c(17, 22), n=c(17+41, 22+198), 
          p = NULL, alternative = "greater", correct = TRUE)



# 2 DANA

summary(`2 DANA`)
table(`2 DANA`)


# 30 DANA
table(`30 DANA`)



################################# FUNKCIJE

library(pROC)
calculate_auc <- function(factual, fpredicted) {
  roc_curve <- roc(factual, fpredicted)
  
  # Print the AUC value
  auc_value <- auc(roc_curve)
  print(auc_value)
  
  # Plot the ROC curve
  plot(roc_curve, main = paste("ROC krivulja (AUC =", round(auc_value, 2), ")"),
       xlab = "Specifičnost", ylab = "Osjetljivost")
}
calculate_auc(smrt, UKUPNI_BODOVI_1)

conf_matrix <- function(factual, fpredicted01){
  tp <- length(factual[which(fpredicted01==1 & factual==1)])
  tn <- length(factual[which(fpredicted01==0 & factual==0)])
  fp <- length(factual[which(fpredicted01==1 & factual==0)])
  fn <- length(factual[which(fpredicted01==0 & factual==1)])
  sum_of_all <- tp+tn+fp+fn
  
  sensitivity <- tp/(tp+fn)  #tpr
  specificity <- tn/(tn+fp)   #tnr
  pos_predictive_power <- tp/(tp+fp)     # precision
  neg_predictive_power <- tn/(tn+fn)
  odds_ratio <- (tp*tn)/(fp*fn)
  f1_score <- 2*tp/(2*tp+fp+fn)
  accuracy <- (tp+tn)/sum_of_all
  mcc <- (tp*tn-fp*fn)/(sqrt( (tp+fp)*(tp+fn)*(tn+fp)*(tn+fn) ))
  
  ci_lower <- exp( log(odds_ratio) - 1.96*sqrt(1/tp+1/tn+1/fp+1/fn) )
  ci_upper <- exp( log(odds_ratio) + 1.96*sqrt(1/tp+1/tn+1/fp+1/fn) )
  ci <- c(ci_lower, ci_upper)
  
  pos_likelihood_ratio <- sensitivity/(1-specificity)
  neg_likelihood_ratio <- (1-sensitivity)/specificity
  
  
  significance <- "yes"
  if(ci_lower<=1 & 1<=ci_upper){
    significance <- "no"
  }
  
  list_of_results <- list("tp"=tp, "tn"=tn, "fp"=fp, "fn"=fn,
                          "sensitivity"=sensitivity, 
                          "specificity"=specificity,
                          "ppp"=pos_predictive_power,
                          "npp"=neg_predictive_power,
                          "odds_ratio"=odds_ratio,
                          "ci"=ci,
                          "significance"=significance,
                          "f1_score"=f1_score,
                          "accuracy"=accuracy,
                          "mcc"=mcc)
  return(list_of_results)
}
conf_matrix(vektor1, vektor2)

######################## About

# the Matthews correlation coefficient mcc is the only one that is in [-1, 1]
# +1 value corresponds to perfect classification; 
# value close to 0 corresponds to a prediction made by chance; 
# -1 corresponds to a perfectly opposite prediction, 
#     where all the negative samples were predicted as positive and vice versa

# Odds ratios are used to compare the relative odds of the occurrence 
# of the outcome of interest (e.g. disease or disorder), 
# given exposure to the variable of interest
# Confidence interval for odds ratio is calculated for 95% ci
# that is why we have 1.96 as a coefficient in the formula
# which is qnorm(1-alpha/2), for alpha=0.05
# so for other levels of confidence, change the value

# A positive likelihood ratio, or LR+, is the 
# “probability that a positive test would be expected in a patient 
# divided by the probability that a positive test would be expected 
# in a patient without a disease.”
# A negative likelihood ratio or LR-, is 
# “the probability of a patient testing negative who has a disease 
# divided by the probability of a patient testing negative who 
# does not have a disease.”






######################## ROC and AUC


#### maksimum news2 u prva tri mjerenja
uk_1 <- UKUPNI_BODOVI_1
uk_2 <- UKUPNI_BODOVI_2
uk_3 <- UKUPNI_BODOVI_3
for (i in 1:length(uk_1)) {
  if (is.na(uk_2[i]) == TRUE) {
    uk_2[i] <- uk_1[i]
  }
  if (is.na(uk_3[i]) == TRUE) {
    uk_3[i] <- uk_1[i]
  }
}

news2_3mjerenja <- c()
for (i in 1:length(uk_1)) {
  news2_3mjerenja[i] <- max(uk_1[i], uk_2[i], uk_3[i])
}
table(news2_3mjerenja)
hist(news2_3mjerenja[which(smrt==0)])
density(news2_3mjerenja[which(smrt==1)])

plot(density(news2_3mjerenja[which(smrt==1)]), col="lightblue", lwd=2, main="",
     xlab = "Vrijednost NEWS2", ylab = "Gustoća", ylim=c(0, 0.2))
lines(density(news2_3mjerenja[which(smrt==0)]), col="pink", lwd=2)
legend(-4, 0.2, legend=c("Umrli", "Preživjeli"),
       col=c("lightblue", "pink"), lty=1:1, cex=0.8, lwd=2:2)

var.test(news2_3mjerenja[which(smrt==1)], news2_3mjerenja[which(smrt==0)])
t.test(news2_3mjerenja[which(smrt==1)], news2_3mjerenja[which(smrt==0)],
       alternative = "greater", var.equal = TRUE)




calculate_auc(smrt, news2_3mjerenja)

prag5 <- c(rep(0, length(news2_3mjerenja)))
for (i in 1:length(news2_3mjerenja)) {
  if (news2_3mjerenja[i] >= 5) {
    prag5[i] <- 1
  }
}
table(prag5)
prag7 <- c(rep(0, length(news2_3mjerenja)))
for (i in 1:length(news2_3mjerenja)) {
  if (news2_3mjerenja[i] >= 7) {
    prag7[i] <- 1
  }
}
table(prag7)

conf_matrix(smrt, prag5)
conf_matrix(smrt, prag7)


smrt_24h <- c(rep(0, length(otpust)))
for (i in 1:length(otpust)) {
  if (otpust[i] == 5){
    smrt_24h[i] <- 1
  }
}
table(smrt_24h)
calculate_auc(smrt_24h, news2_3mjerenja)
conf_matrix(smrt_24h, prag5)
conf_matrix(smrt_24h, prag7)


check_smrt <- c(rep(0, length(otpust)))
for (i in 1:length(otpust)) {
  if (otpust[i] == 5) {
    check_smrt[i] <- 1
  }
  if (is.na(`2 DANA`[i]) == FALSE & `2 DANA`[i] == 5) {
    check_smrt[i] <- 1
  }
  if (is.na(`30 DANA`[i]) == FALSE &`30 DANA`[i] == 5) {
    check_smrt[i] <- 1
  }
}
table(check_smrt)
# pacijent pod rednim br 3 ima jednu nelogicnost
# umro je nakon 2 dana a smrt = 0
# pa treba sve ponoviti samo s var check_smrt
# IPAK NIJE UMRLA - ISPRAVI VAR 2 DANA NA POZ 3 U 0!!!

`2 DANA`[3]
SMRT[3]
dvadana <- `2 DANA`
OTPUST[3]
dvadana[3] <- 3

calculate_auc(check_smrt, news2_3mjerenja)
conf_matrix(check_smrt, prag5)
conf_matrix(check_smrt, prag7)


table(icu)
calculate_auc(icu, news2_3mjerenja)
conf_matrix(icu, prag5)
conf_matrix(icu, prag7)


otpust4ili5 <- c(rep(0, length(otpust)))
for (i in 1:length(otpust)) {
  if (otpust[i] %in% c(4, 5)) {
    otpust4ili5[i] <- 1
  }
}
table(otpust4ili5)
calculate_auc(otpust4ili5, news2_3mjerenja)
conf_matrix(otpust4ili5, prag5)
conf_matrix(otpust4ili5, prag7)


smrt_icu <- rep(0, length(icu))
for (i in 1:length(icu)) {
  if (icu[i] == 1 || smrt[i] == 1) {
    smrt_icu[i] <- 1
  }
}
table(smrt_icu)
calculate_auc(smrt_icu, news2_3mjerenja)
conf_matrix(smrt_icu, prag5)
conf_matrix(smrt_icu, prag7)



poz_ishod <- rep(0, length(otpust))
for (i in 1:length(otpust)) {
  if (otpust[i] %in% c(1, 2)) {
    poz_ishod[i] <- 1
  }
}

table(poz_ishod)
calculate_auc(poz_ishod, news2_3mjerenja)
conf_matrix(poz_ishod, 1-prag5)
conf_matrix(poz_ishod, 1-prag7)




icu_24 <- rep(0, length(icu))
for (i in 1:length(icu)) {
  if (otpust[i] == 4) {
    icu_24[i] <- 1
  }
}
table(icu_24)
calculate_auc(icu_24, news2_3mjerenja)
conf_matrix(icu_24, prag5)
conf_matrix(icu_24, prag7)



icu_2dana <- rep(0, length(icu))
for (i in 1:length(icu)) {
  if (icu[i] == 1 || (is.na(dvadana[i]) == FALSE & dvadana[i] == 4)) {
    icu_2dana[i] <- 1
  }
}
table(icu_2dana)
calculate_auc(icu_2dana, news2_3mjerenja)
conf_matrix(icu_2dana, prag5)
conf_matrix(icu_2dana, prag7)



smrt_2dana <- rep(0, length(icu))
for (i in 1:length(icu)) {
  if (smrt_24h[i] == 1 || (is.na(dvadana[i]) == FALSE & dvadana[i] == 5)) {
    smrt_2dana[i] <- 1
  }
}
table(smrt_2dana)
calculate_auc(smrt_2dana, news2_3mjerenja)
conf_matrix(smrt_2dana, prag5)
conf_matrix(smrt_2dana, prag7)



smrt_icu_2dana <- rep(0, length(icu))
for (i in 1:length(icu)) {
  if (smrt_2dana[i] == 1 || icu_2dana[i] == 1) {
    smrt_icu_2dana[i] <- 1
  }
}
table(smrt_icu_2dana)
calculate_auc(smrt_icu_2dana, news2_3mjerenja)
conf_matrix(smrt_icu_2dana, prag5)
conf_matrix(smrt_icu_2dana, prag7)








#### IZUZMIMO kategoriju 6 (razlog = 6), tj neuroloske pacijente

neuro_smrt <- smrt[which(`RAZLOG/dg`!=6)]
table(neuro_smrt)
neuro_news <- news2_3mjerenja[which(`RAZLOG/dg`!=6)]
neuro_icu <- icu[which(`RAZLOG/dg`!=6)]
neuro_smrt_icu <- smrt_icu[which(`RAZLOG/dg`!=6)]
neuro_smrt24h <- smrt_24h[which(`RAZLOG/dg`!=6)]
neuro_icu24h <- icu_24[which(`RAZLOG/dg`!=6)]
neuro_smrt_icu_24h <- otpust4ili5[which(`RAZLOG/dg`!=6)]
neuro_smrt_2dana <- smrt_2dana[which(`RAZLOG/dg`!=6)]
neuro_icu_2dana <- icu_2dana[which(`RAZLOG/dg`!=6)]
neuro_smrt_icu_2dana <- smrt_icu_2dana[which(`RAZLOG/dg`!=6)]





neuro_prag5 <- c(rep(0, length(neuro_news)))
for (i in 1:length(neuro_news)) {
  if (neuro_news[i] >= 5) {
    neuro_prag5[i] <- 1
  }
}
table(neuro_prag5)
neuro_prag7 <- c(rep(0, length(neuro_news)))
for (i in 1:length(neuro_news)) {
  if (neuro_news[i] >= 7) {
    neuro_prag7[i] <- 1
  }
}
table(neuro_prag7)

calculate_auc(neuro_smrt, neuro_news)
conf_matrix(neuro_smrt, neuro_prag5)
conf_matrix(neuro_smrt, neuro_prag7)

calculate_auc(neuro_icu, neuro_news)
conf_matrix(neuro_icu, neuro_prag5)
conf_matrix(neuro_icu, neuro_prag7)

calculate_auc(neuro_smrt_icu, neuro_news)
conf_matrix(neuro_smrt_icu, neuro_prag5)
conf_matrix(neuro_smrt_icu, neuro_prag7)

calculate_auc(neuro_smrt24h, neuro_news)
conf_matrix(neuro_smrt24h, neuro_prag5)
conf_matrix(neuro_smrt24h, neuro_prag7)

calculate_auc(neuro_icu24h, neuro_news)
conf_matrix(neuro_icu24h, neuro_prag5)
conf_matrix(neuro_icu24h, neuro_prag7)

calculate_auc(neuro_smrt_icu_24h, neuro_news)
conf_matrix(neuro_smrt_icu_24h, neuro_prag5)
conf_matrix(neuro_smrt_icu_24h, neuro_prag7)

calculate_auc(neuro_smrt_icu_2dana, neuro_news)
conf_matrix(neuro_smrt_icu_2dana, neuro_prag5)
conf_matrix(neuro_smrt_icu_2dana, neuro_prag7)

calculate_auc(neuro_smrt_2dana, neuro_news)
conf_matrix(neuro_smrt_2dana, neuro_prag5)
conf_matrix(neuro_smrt_2dana, neuro_prag7)

calculate_auc(neuro_icu_2dana, neuro_news)
conf_matrix(neuro_icu_2dana, neuro_prag5)
conf_matrix(neuro_icu_2dana, neuro_prag7)





### IZUZMIMO SADA SAMO KARDIOLOSKE PACIJENTE

kardio_smrt <- smrt[which(`RAZLOG/dg`!=9)]
table(kardio_smrt)
kardio_news <- news2_3mjerenja[which(`RAZLOG/dg`!=9)]
kardio_icu <- icu[which(`RAZLOG/dg`!=9)]
kardio_smrt_icu <- smrt_icu[which(`RAZLOG/dg`!=9)]
kardio_smrt24h <- smrt_24h[which(`RAZLOG/dg`!=9)]
kardio_icu24h <- icu_24[which(`RAZLOG/dg`!=9)]
kardio_smrt_icu_24h <- otpust4ili5[which(`RAZLOG/dg`!=9)]
kardio_smrt_2dana <- smrt_2dana[which(`RAZLOG/dg`!=9)]
kardio_icu_2dana <- icu_2dana[which(`RAZLOG/dg`!=9)]
kardio_smrt_icu_2dana <- smrt_icu_2dana[which(`RAZLOG/dg`!=9)]





kardio_prag5 <- c(rep(0, length(kardio_news)))
for (i in 1:length(kardio_news)) {
  if (kardio_news[i] >= 5) {
    kardio_prag5[i] <- 1
  }
}
table(kardio_prag5)
kardio_prag7 <- c(rep(0, length(kardio_news)))
for (i in 1:length(kardio_news)) {
  if (kardio_news[i] >= 7) {
    kardio_prag7[i] <- 1
  }
}
table(kardio_prag7)

calculate_auc(kardio_smrt, kardio_news)
conf_matrix(kardio_smrt, kardio_prag5)
conf_matrix(kardio_smrt, kardio_prag7)

calculate_auc(kardio_icu, kardio_news)
conf_matrix(kardio_icu, kardio_prag5)
conf_matrix(kardio_icu, kardio_prag7)

calculate_auc(kardio_smrt_icu, kardio_news)
conf_matrix(kardio_smrt_icu, kardio_prag5)
conf_matrix(kardio_smrt_icu, kardio_prag7)

calculate_auc(kardio_smrt24h, kardio_news)
conf_matrix(kardio_smrt24h, kardio_prag5)
conf_matrix(kardio_smrt24h, kardio_prag7)

calculate_auc(kardio_icu24h, kardio_news)
conf_matrix(kardio_icu24h, kardio_prag5)
conf_matrix(kardio_icu24h, kardio_prag7)

calculate_auc(kardio_smrt_icu_24h, kardio_news)
conf_matrix(kardio_smrt_icu_24h, kardio_prag5)
conf_matrix(kardio_smrt_icu_24h, kardio_prag7)

calculate_auc(kardio_smrt_icu_2dana, kardio_news)
conf_matrix(kardio_smrt_icu_2dana, kardio_prag5)
conf_matrix(kardio_smrt_icu_2dana, kardio_prag7)

calculate_auc(kardio_smrt_2dana, kardio_news)
conf_matrix(kardio_smrt_2dana, kardio_prag5)
conf_matrix(kardio_smrt_2dana, kardio_prag7)

calculate_auc(kardio_icu_2dana, kardio_news)
conf_matrix(kardio_icu_2dana, kardio_prag5)
conf_matrix(kardio_icu_2dana, kardio_prag7)

