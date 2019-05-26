safelog <- function(x) {
  return (log(x + 1e-100))
}

DIMG <-read.csv("qmbu_data_set_images.csv", header = FALSE)
DLBL <-read.csv("qmbu_data_set_labels.csv", header = FALSE)

IMG <- as.matrix(DIMG)
LBL <- as.matrix(DLBL)


TrainIMG <- matrix(0,125,320)
TestIMG <- matrix(0,70,320)
TrainLBL <- matrix(0,125,1)
TestLBL <- matrix(0,70,1)
Flag <- matrix(0,195,1)

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


pcd <- matrix(0,320,5)


for(i in 1:320)
{
    sa=0
    sb=0
    sc=0
    sd=0
    se=0
    
    for(j in 1:25) { if(TrainIMG[j,i]==1) { sa=sa+1 } }
    for(j in 26:50) { if(TrainIMG[j,i]==1) { sb=sb+1 } } 
    for(j in 51:75) { if(TrainIMG[j,i]==1) { sc=sc+1 } } 
    for(j in 76:100) { if(TrainIMG[j,i]==1) { sd=sd+1 } } 
    for(j in 101:125) { if(TrainIMG[j,i]==1) { se=se+1 } } 
    
    pcd[i,1]=sa/25
    pcd[i,2]=sb/25
    pcd[i,3]=sc/25
    pcd[i,4]=sd/25
    pcd[i,5]=se/25
    
}

print(pcd[,1])
print(pcd[,2])
print(pcd[,3])
print(pcd[,4])
print(pcd[,5])

Con1 <- matrix(0,5,5)
Con2 <- matrix(0,5,5)

for(i in 1:125)
{
    ga=0
    gb=0
    gc=0
    gd=0
    ge=0
    for(j in 1:320)
    {
        ga <- ga + TrainIMG[i,j]*safelog(pcd[j,1]) + (1-TrainIMG[i,j])*safelog(1-pcd[j,1])
        gb <- gb + TrainIMG[i,j]*safelog(pcd[j,2]) + (1-TrainIMG[i,j])*safelog(1-pcd[j,2])
        gc <- gc + TrainIMG[i,j]*safelog(pcd[j,3]) + (1-TrainIMG[i,j])*safelog(1-pcd[j,3])
        gd <- gd + TrainIMG[i,j]*safelog(pcd[j,4]) + (1-TrainIMG[i,j])*safelog(1-pcd[j,4])
        ge <- ge + TrainIMG[i,j]*safelog(pcd[j,5]) + (1-TrainIMG[i,j])*safelog(1-pcd[j,5])
    }  
    maxi <- max(ga,gb,gc,gd,ge)
    
    if(maxi==ga && TrainLBL[i]=="A") Con1[1,1] <- Con1[1,1] + 1
    if(maxi==ga && TrainLBL[i]=="B") Con1[1,2] <- Con1[1,2] + 1
    if(maxi==ga && TrainLBL[i]=="C") Con1[1,3] <- Con1[1,3] + 1
    if(maxi==ga && TrainLBL[i]=="D") Con1[1,4] <- Con1[1,4] + 1
    if(maxi==ga && TrainLBL[i]=="E") Con1[1,5] <- Con1[1,5] + 1
    
    if(maxi==gb && TrainLBL[i]=="A") Con1[2,1] <- Con1[2,1] + 1
    if(maxi==gb && TrainLBL[i]=="B") Con1[2,2] <- Con1[2,2] + 1
    if(maxi==gb && TrainLBL[i]=="C") Con1[2,3] <- Con1[2,3] + 1
    if(maxi==gb && TrainLBL[i]=="D") Con1[2,4] <- Con1[2,4] + 1
    if(maxi==gb && TrainLBL[i]=="E") Con1[2,5] <- Con1[2,5] + 1
    
    if(maxi==gc && TrainLBL[i]=="A") Con1[3,1] <- Con1[3,1] + 1
    if(maxi==gc && TrainLBL[i]=="B") Con1[3,2] <- Con1[3,2] + 1
    if(maxi==gc && TrainLBL[i]=="C") Con1[3,3] <- Con1[3,3] + 1
    if(maxi==gc && TrainLBL[i]=="D") Con1[3,4] <- Con1[3,4] + 1
    if(maxi==gc && TrainLBL[i]=="E") Con1[3,5] <- Con1[3,5] + 1
    
    if(maxi==gd && TrainLBL[i]=="A") Con1[4,1] <- Con1[4,1] + 1
    if(maxi==gd && TrainLBL[i]=="B") Con1[4,2] <- Con1[4,2] + 1
    if(maxi==gd && TrainLBL[i]=="C") Con1[4,3] <- Con1[4,3] + 1
    if(maxi==gd && TrainLBL[i]=="D") Con1[4,4] <- Con1[4,4] + 1
    if(maxi==gd && TrainLBL[i]=="E") Con1[4,5] <- Con1[4,5] + 1
    
    if(maxi==ge && TrainLBL[i]=="A") Con1[5,1] <- Con1[5,1] + 1
    if(maxi==ge && TrainLBL[i]=="B") Con1[5,2] <- Con1[5,2] + 1
    if(maxi==ge && TrainLBL[i]=="C") Con1[5,3] <- Con1[5,3] + 1
    if(maxi==ge && TrainLBL[i]=="D") Con1[5,4] <- Con1[5,4] + 1
    if(maxi==ge && TrainLBL[i]=="E") Con1[5,5] <- Con1[5,5] + 1
    
}

print(Con1)

for(i in 1:70)
{
  ga=0
  gb=0
  gc=0
  gd=0
  ge=0
  for(j in 1:320)
  {
    ga <- ga + TestIMG[i,j]*safelog(pcd[j,1]) + (1-TestIMG[i,j])*safelog(1-pcd[j,1])
    gb <- gb + TestIMG[i,j]*safelog(pcd[j,2]) + (1-TestIMG[i,j])*safelog(1-pcd[j,2])
    gc <- gc + TestIMG[i,j]*safelog(pcd[j,3]) + (1-TestIMG[i,j])*safelog(1-pcd[j,3])
    gd <- gd + TestIMG[i,j]*safelog(pcd[j,4]) + (1-TestIMG[i,j])*safelog(1-pcd[j,4])
    ge <- ge + TestIMG[i,j]*safelog(pcd[j,5]) + (1-TestIMG[i,j])*safelog(1-pcd[j,5])
  }  
  maxi <- max(ga,gb,gc,gd,ge)
  
  if(maxi==ga && TestLBL[i]=="A") Con2[1,1] <- Con2[1,1] + 1
  if(maxi==ga && TestLBL[i]=="B") Con2[1,2] <- Con2[1,2] + 1
  if(maxi==ga && TestLBL[i]=="C") Con2[1,3] <- Con2[1,3] + 1
  if(maxi==ga && TestLBL[i]=="D") Con2[1,4] <- Con2[1,4] + 1
  if(maxi==ga && TestLBL[i]=="E") Con2[1,5] <- Con2[1,5] + 1
  
  if(maxi==gb && TestLBL[i]=="A") Con2[2,1] <- Con2[2,1] + 1
  if(maxi==gb && TestLBL[i]=="B") Con2[2,2] <- Con2[2,2] + 1
  if(maxi==gb && TestLBL[i]=="C") Con2[2,3] <- Con2[2,3] + 1
  if(maxi==gb && TestLBL[i]=="D") Con2[2,4] <- Con2[2,4] + 1
  if(maxi==gb && TestLBL[i]=="E") Con2[2,5] <- Con2[2,5] + 1
  
  if(maxi==gc && TestLBL[i]=="A") Con2[3,1] <- Con2[3,1] + 1
  if(maxi==gc && TestLBL[i]=="B") Con2[3,2] <- Con2[3,2] + 1
  if(maxi==gc && TestLBL[i]=="C") Con2[3,3] <- Con2[3,3] + 1
  if(maxi==gc && TestLBL[i]=="D") Con2[3,4] <- Con2[3,4] + 1
  if(maxi==gc && TestLBL[i]=="E") Con2[3,5] <- Con2[3,5] + 1
  
  if(maxi==gd && TestLBL[i]=="A") Con2[4,1] <- Con2[4,1] + 1
  if(maxi==gd && TestLBL[i]=="B") Con2[4,2] <- Con2[4,2] + 1
  if(maxi==gd && TestLBL[i]=="C") Con2[4,3] <- Con2[4,3] + 1
  if(maxi==gd && TestLBL[i]=="D") Con2[4,4] <- Con2[4,4] + 1
  if(maxi==gd && TestLBL[i]=="E") Con2[4,5] <- Con2[4,5] + 1
  
  if(maxi==ge && TestLBL[i]=="A") Con2[5,1] <- Con2[5,1] + 1
  if(maxi==ge && TestLBL[i]=="B") Con2[5,2] <- Con2[5,2] + 1
  if(maxi==ge && TestLBL[i]=="C") Con2[5,3] <- Con2[5,3] + 1
  if(maxi==ge && TestLBL[i]=="D") Con2[5,4] <- Con2[5,4] + 1
  if(maxi==ge && TestLBL[i]=="E") Con2[5,5] <- Con2[5,5] + 1
  
}

print(Con2)

