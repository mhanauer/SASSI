---
title: "SASSI"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
SASSI Load Data
```{r}
library(caret)
library(prettyR)
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/SASSI/Data")
honest = read.csv("A3 Honest Admin.csv", header = TRUE)
honest = as.data.frame(honest)
fake = read.csv("A3 Fake-Good Admin.csv", header = TRUE)
```
Make sure everyone matches up
```{r}
dim(honest)
dim(fake)
colnames(honest)[1] = c("ID")
colnames(fake)[1] = c("ID")
```
Demographics
```{r}
head(honest)
demos = honest[c("AGE", "GENDER", "RACE", "STUDENT", "GRADE", "EMPLOY", "B1", "B2", "B3", "B4", "B5")]
apply(demos, 2, function(x){describe.factor(x)})
mean(honest$AGE, na.rm = TRUE)
sd(honest$AGE, na.rm= TRUE)
```
Table 2
Sensitivty
```{r}
### Get numbers
describe.factor(fake$THEfinalRULE)
describe.factor(fake$FVAonlyRULE)

###Create diagnosis
truth = rep(1, dim(fake)[1])
truth = as.factor(truth)

### Now get sens for both, which is just the percentage 
fake_all =  as.factor(fake$THEfinalRULE)
sensitivity(fake_all, truth, boot = TRUE)

fake_face = as.factor(fake$FVAonlyRULE)
sensitivity(fake_face, truth)
R.Version()
```
T-tests
```{r}

###FVA5R
mean(honest$FVA5R, na.rm = TRUE)
sd(honest$FVA5R, na.rm = TRUE)

mean(fake$FVA5R, na.rm = TRUE)
sd(fake$FVA5R, na.rm = TRUE)

t.test(honest$FVA5R, fake$FVA5R, paired = TRUE)
###FVOD5R
mean(honest$FVOD5R, na.rm = TRUE)
sd(honest$FVOD5R, na.rm = TRUE)

mean(fake$FVOD5R, na.rm = TRUE)
sd(fake$FVOD5R, na.rm = TRUE)

t.test(honest$FVOD5R, fake$FVOD5R, paired = TRUE)

###FRISK
mean(honest$FRISK, na.rm = TRUE)
sd(honest$FRISK, na.rm = TRUE)

mean(fake$FRISK, na.rm = TRUE)
sd(fake$FRISK, na.rm = TRUE)

t.test(honest$FRISK, fake$FRISK, paired = TRUE)

###ATT
honest$ATT
mean(honest$ATT, na.rm = TRUE)
sd(honest$ATT, na.rm = TRUE)

mean(fake$ATT, na.rm = TRUE)
sd(fake$ATT, na.rm = TRUE)

t.test(honest$ATT, fake$ATT, paired = TRUE)

###SYM
honest$SYM
mean(honest$SYM, na.rm = TRUE)
sd(honest$SYM, na.rm = TRUE)

mean(fake$SYM, na.rm = TRUE)
sd(fake$SYM, na.rm = TRUE)

t.test(honest$SYM, fake$SYM, paired = TRUE)

###OAT
mean(honest$OAT, na.rm = TRUE)
sd(honest$OAT, na.rm = TRUE)

mean(fake$OAT, na.rm = TRUE)
sd(fake$OAT, na.rm = TRUE)

t.test(honest$OAT, fake$OAT, paired = TRUE)


###SAT
honest$SAT
mean(honest$SAT, na.rm = TRUE)
sd(honest$SAT, na.rm = TRUE)

mean(fake$SAT, na.rm = TRUE)
sd(fake$SAT, na.rm = TRUE)

t.test(honest$SAT, fake$SAT, paired = TRUE)

###SAM
honest$SAM
mean(honest$SAM, na.rm = TRUE)
sd(honest$SAM, na.rm = TRUE)

mean(fake$SAM, na.rm = TRUE)
sd(fake$SAM, na.rm = TRUE)

t.test(honest$SAM, fake$SAM, paired = TRUE)

###COR
honest$COR
mean(honest$COR, na.rm = TRUE)
sd(honest$COR, na.rm = TRUE)

mean(fake$COR, na.rm = TRUE)
sd(fake$COR, na.rm = TRUE)

t.test(honest$COR, fake$COR, paired = TRUE)
```
Identify those with high VAL (above 4) and low prob on fake good for total score
```{r}
fake_val = fake[c("VAL", "THEfinalRULE")]
fake_val = subset(fake_val, VAL > 4 &  THEfinalRULE == 2)
dim(fake_val)[1]




```
DEF below 40 and low prob on total on faking good data
```{r}
fake_DEF = fake[c("DEF", "THEfinalRULE")]
#fake_DEF = subset(fake_DEF, DEF)
```




