install.packages("stringi")
install.packages("LearnGeom")


library(stringi)
library(LearnGeom)


# Question 1
# A

# We chose 'dog' to be our 'word'.
# We assume only lowercase letters are available in the keyboard (= 26 options)
# Therefore, the expected value is E(2+G(1/26^3)) = 2 + 26^3 = 17578


# B

result_vec_Q1 <- numeric(10000)
for (i in 1:10000) {
  counter <- 3
  monkey_word <- stri_rand_strings(n = 1, length = 3, pattern = "[a-z]")
  while (monkey_word != 'dog')
  {
    counter <- counter + 1
    stri_sub(monkey_word, from=4L, to=-1L) <- stri_rand_strings(n = 1, length = 1, pattern = "[a-z]")
    monkey_word <- stri_sub(monkey_word, from=2L, to=4L)
    
  }
  
  result_vec_Q1[i] <- counter
}

mean(result_vec_Q1)

# ?????? ????? ?????... ???? ???? ???? ??? ????? ??????? ???? ????? ??? ????? ??? ???? ????

mean(result_vec_Q1) + c(-1,1)*1.96*sd(result_vec_Q1)


quantile(x = result_vec_Q1,probs = c(.025,.975))


plot(sort(result_vec_Q1))

median(result_vec_Q1)

p <- 1/26^3
real_median <- -1/log2(1-p)

# ????? ???? - ????? ?? ?????? ?????? ????? ???????? ???? ???? ??????? ????? ????? ???? ??????, ?? ?????? ?? ?????? ???? ????
# ??????, ?? ?? ????? ????????





# Question 2


result_vec_Q2 <- numeric(10000)
for (i in 1:10000) {
  p.1 <- runif(2)
  p.2 <- runif(2)
  p.3 <- runif(2)
  
  result_vec_Q2[i] <- max(Angle(p.1,p.2,p.3),Angle(p.2,p.1,p.3),Angle(p.1,p.3,p.2))
}

mean(result_vec_Q2)
plot(sort(result_vec_Q2))
summary(result_vec_Q2)

mean(result_vec_Q2>90)
mean(result_vec_Q2==90)






# Question 3

#A
res_vec_Q3 <- numeric(10000)

for (i in 1:10000) {
  res_vec_Q3[i] <- abs(median(rnorm(500)))
}

quantile(x = res_vec_Q3, probs = .95)

# the answer is 0.1098604 (if we change the answer, we should change also in part 3.B)


# ?????? ??????

#B

res_vec_Q3B <- numeric(10000)

for (i in 1:10000) {
  res_vec_Q3B[i] <- abs(median(rnorm(n = 500,mean = 0.1,sd = 1)))
}

mean(res_vec_Q3B>0.1098604)


# ????? ???????? - ??????

# Question 4





# 1 2 3
# 4 5 6
# 7 8 9



cat_move <- function(num_1)
{
  if(num_1 == 1){
    num_1 <- sample(x = c(2,4,5),size = 1)
    return(num_1)}
  if(num_1 == 2){
    num_1 <- sample(x = c(1,3,4,5,6),size = 1)
    return(num_1)}
  if(num_1 == 3){
    num_1 <- sample(x = c(2,5,6),size = 1)
    return(num_1)}
  if(num_1 == 4){
    num_1 <- sample(x = c(1,2,5,7,8),size = 1)
    return(num_1)}
  if(num_1 == 5){
    num_1 <- sample(x = c(1,2,3,4,6,7,8,9),size = 1)
    return(num_1)}
  if(num_1 == 6){
    num_1 <- sample(x = c(2,3,5,8,9),size = 1)
    return(num_1)}
  if(num_1 == 7){
    num_1 <- sample(x = c(4,5,8),size = 1)
    return(num_1)}
  if(num_1 == 8){
    num_1 <- sample(x = c(4,5,6,7,9),size = 1)
    return(num_1)}
  if(num_1 == 9){
    num_1 <- sample(x = c(5,6,8),size = 1)
    return(num_1)}
}



cat_1 <- 1
cat_2 <- 9

round_number <- vector(length = 1050)

for (i in 1:1050) {
  cat_1 <- cat_move(cat_1)
  cat_2 <- cat_move(cat_2)
  ifelse(cat_1==cat_2,yes = round_number[i] <- TRUE,no = round_number[i] <- FALSE)
}

mean(round_number[51:1050])




# Question 5

