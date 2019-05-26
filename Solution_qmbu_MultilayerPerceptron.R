
DIMG <-read.csv("qmbu_data_set_images.csv", header = FALSE)
DLBL <-read.csv("qmbu_data_set_labels.csv", header = FALSE)

IMG <- as.matrix(DIMG)
LBL <- as.matrix(DLBL)


TrainIMG <- matrix(0,125,320)
TestIMG <- matrix(0,70,320)
TrainLBL <- matrix(0,125,1)
TestLBL <- matrix(0,70,1)
Flag <- matrix(0,195,1)
Y_Truth <- matrix(0,125,5)
Y_Predicted <- matrix(0,125,5)

for(i in 1:25) Flag[i]=1
for(i in 40:64) Flag[i]=1
for(i in 79:103) Flag[i]=1
for(i in 118:142) Flag[i]=1
for(i in 157:181) Flag[i]=1

s1=0
s2=0
for(i in 1:195)
{
  if(Flag[i]==1)
  {
    s1 = s1 + 1
    for(j in 1:320)
      TrainIMG[s1,j]=IMG[i,j]
    TrainLBL[s1]=LBL[i]
    
  }
  if(Flag[i]==0)
  {
    s2 = s2 + 1
    for(j in 1:320)
      TestIMG[s2,j]=IMG[i,j]
    TestLBL[s2]=LBL[i]
  }
}

print(s1)
print(s2)
View(TrainIMG)
View(TrainLBL)
View(TestIMG)
View(TestLBL)

for(i in 1:25) Y_Truth[i,1]=1
for(i in 26:50) Y_Truth[i,2]=1
for(i in 51:75) Y_Truth[i,3]=1
for(i in 76:100) Y_Truth[i,4]=1
for(i in 101:125) Y_Truth[i,5]=1

y_truth <- matrix(0,125,1)
y_truth2 <- matrix(0,70,1)

for(i in 1:25) y_truth[i]=1
for(i in 26:50) y_truth[i]=2
for(i in 51:75) y_truth[i]=3
for(i in 76:100) y_truth[i]=4
for(i in 101:125) y_truth[i]=5

for(i in 1:14) y_truth2[i]=1
for(i in 15:28) y_truth2[i]=2
for(i in 29:42) y_truth2[i]=3
for(i in 43:56) y_truth2[i]=4
for(i in 57:70) y_truth2[i]=5

safelog <- function(x) {
  return (log(x + 1e-100))
}

sigmoid <- function(x) {
  return (1/(1+exp(-x)))
}

softmax <- function(x) {
  x <- (exp(x))
  return (x/matrix(rowSums(x), nrow(x), ncol(x), byrow = FALSE))
}

s <- 0
s_old <- 0
eta <- 0.005
epsilon <- 1e-3
H <- 20
max_iteration <- 200
set.seed(521)

W <- matrix(runif(321 * H, min = -0.01, max = 0.01), 321, H)
V <- matrix(runif((H + 1) * 5, min = -0.01, max = 0.01), H + 1, 5)
Z <- matrix(0,125,H)
  
Z <- sigmoid(cbind(1,TrainIMG) %*% W)
Y_Predicted <- softmax(cbind(1,Z) %*% V)
iteration <- 1
objective_values <- -sum(Y_Truth * safelog(Y_Predicted))

while (1) {
  
  V <- V + eta * t(cbind(1,Z)) %*% (Y_Truth - Y_Predicted)
  W <- W + eta * t(cbind(1,TrainIMG)) %*% ((Y_Truth - Y_Predicted) %*% t(V[2:21,]) * Z * (1-Z))
  Z <- sigmoid(cbind(1,TrainIMG) %*% W)
  Y_Predicted <- softmax(cbind(1,Z) %*% V)
  s_old <- s
  s <- -sum(Y_Truth * safelog(Y_Predicted))
  objective_values <- c(objective_values, s)
  if (abs(s - s_old) < epsilon | iteration >= max_iteration) {
    break
  }
  
  iteration <- iteration + 1
}


plot(1:(iteration+1), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

y_predicted <- matrix(0,125,5)
confusion_matrix <- matrix(0,5,5)
cm2 <- matrix(0,5,5)

y_test_p <- matrix(0,70,5)
y_test_p2 <- matrix(0,70,5)
ytp <- matrix(0,70,5)

y_predicted <- apply(Y_Predicted, 1, which.max)
confusion_matrix <- table(y_predicted, y_truth)
print(confusion_matrix)

Z <- sigmoid(cbind(1,TestIMG) %*% W)
y_test_p <- softmax(cbind(1,Z) %*% V)

#y_test_p <- sigmoid(TestIMG,W,w0)

y_test_p2 <- apply(y_test_p, 1, which.max)
cm2 <- table(y_test_p2, y_truth2)
print(cm2)







