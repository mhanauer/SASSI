---
title: "SASSI"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Libraring packages
```{r}
library(stringr)
library(prettyR)
library(caret)
```
Generating data for sensitivty
```{r}
library(bindata)

binCor = matrix(c(1, .6, .6,1), nrow = 2)

binaryDat = rmvbin(n = 100, bincorr = binCor, margprob = c(.5, .5))
cor(binaryDat)

dat =  factor(binaryDat[,1])
reference = factor(binaryDat[,2])

test_sen =  sensitivity(dat, reference, positive = levels(reference)[2])
test_sen
```














