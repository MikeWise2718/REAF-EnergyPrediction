library(testthat)
library(smseglm,verbose=T)

context("Smoothly Segemented LM")

test_that("basic 3 seg creation",{
  set.seed(1234)
  x <- 0:299
  y <- c(   0 + 0.5*0:99 + rnorm(100), 
           50 + 1.0*0:99 + rnorm(100),  
          150 + 2.0*0:99 + rnorm(100) )

  trndf <- data.frame(x,y)
  slmfit <- smseglm(trndf,"y","x",NULL,c(100,200),c("lo","med","hi"),trace=F)
  expect_equal(slmfit$nseg,3)
  expect_equal(length(slmfit$seg),3)
  lm1 <- slmfit$seg[[1]][[1]]
  lm2 <- slmfit$seg[[2]][[1]]
  lm3 <- slmfit$seg[[3]][[1]]
  clm1 <- lm1$coefficients
  clm2 <- lm2$coefficients
  clm3 <- lm3$coefficients
  c <- class(slmfit$seg[[1]])
  expect_equal(class(lm1),"lm")
  expect_equal(class(lm2),"lm")
  expect_equal(class(lm3),"lm")
  print(sprintf("fit1 clm1[[1]]:%.2f clm1[[2]]:%.2f",clm1[1],clm1[2]))
  print(sprintf("fit2 clm2[[1]]:%.2f clm2[[2]]:%.2f",clm2[1],clm2[2]))
  print(sprintf("fit3 clm3[[1]]:%.2f clm3[[2]]:%.2f",clm3[1],clm3[2]))
  
  expect_that(abs(clm1[1] - (   0.0)), is_less_than(2.0) )
  expect_that(abs(clm2[1] - ( -50.0)), is_less_than(2.0) )
  expect_that(abs(clm3[1] - (-250.0)), is_less_than(2.0) )
  expect_that(abs(clm1[2] - 0.5),      is_less_than(0.2) )
  expect_that(abs(clm2[2] - 1.0),      is_less_than(0.2) )
  expect_that(abs(clm3[2] - 2.0),      is_less_than(0.2) )
  
  yhat <- predict(slmfit,trndf)
  res <- sum(abs((trndf$y-yhat)^2))
  print(sprintf("res:%.1f",res))
})
  

test_that("basic 3 seg creation",{
  x <- 0:299
  y <- c(   0 + 0.5*0:99 + rnorm(100), 
            50 + 1.0*0:99 + rnorm(100),  
            150 + 2.0*0:99 + rnorm(100) )
  
  trndf <- data.frame(x,y)
  slmfit <- smseglm(trndf,"x","y",NULL,NULL,c("lo","med","hi"),trace=F)
})