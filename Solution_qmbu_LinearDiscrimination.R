
# THE ALGORITHM WORKS A LITTLE BIT SLOW, PLEASE WAIT FOR RESULTS

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

sigmoid <- function(X, W, w0) {
  n=nrow(X)
  scores <- matrix(0,n,5)
  for(i in 1:n ){
    for(j in 1:5)
    {
      s=w0[j]
      for(k in 1:320){
        s<- s + X[i,k]*W[k,j]
      }
      scores[i,j]=1/(1 + exp(-s))
    }
  }
  return (scores)
}

gradient_W <- function(X, Y_truth, Y_predicted) {
  return (sapply(X = 1:ncol(Y_truth), function(c) colSums(matrix((Y_truth[,c] - Y_predicted[,c])*Y_predicted[,c]*(1-Y_predicted[,c]), nrow = nrow(X), ncol = ncol(X), byrow = FALSE) * X)))
}

gradient_w0 <- function(Y_truth, Y_predicted) {
  
  res <- c(0,0,0,0,0)
  for(i in 1:5){
    for(j in 1:125)
    {
      s=0
      s= s + (Y_truth[j,i]-Y_predicted[j,i])*Y_predicted[j,i]*(1-Y_predicted[j,i])
    }
    res[i]=s
  }
  return (res)
}

eta <- 0.01
epsilon <- 1e-3

set.seed(521)
W <- matrix(runif(320 * 5, min = -0.01, max = 0.01), 320, 5)
w0 <- runif(5, min = -0.01, max = 0.01)

iteration <- 1
objective_values <- c()
while (1) {
  Y_Predicted <- sigmoid(TrainIMG, W, w0)
  
  objective_values <- c(objective_values, sum(0.5*(Y_Truth-Y_Predicted)^2))
  
  W_old <- W
  w0_old <- w0
  
  W <- W + eta * gradient_W(TrainIMG, Y_Truth, Y_Predicted)
  w0 <- w0 + eta * gradient_w0(Y_Truth, Y_Predicted)
  
  print(sqrt(sum((w0 - w0_old)^2) + sum((W - W_old)^2)))
  
  if (sqrt(sum((w0 - w0_old)^2) + sum((W - W_old)^2)) < epsilon) {
    break
  }
  
  iteration <- iteration + 1
}

print(W)
print(w0)

plot(1:iteration, objective_values,
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

y_test_p <- sigmoid(TestIMG,W,w0)

y_test_p2 <- apply(y_test_p, 1, which.max)
cm2 <- table(y_test_p2, y_truth2)
print(cm2)







