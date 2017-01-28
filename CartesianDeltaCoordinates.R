dDiagonalRod<-200           ## Diagonal rod 200 mm
dRadius<-100                ## Projected radius downward from diagonal rod
towerXAngle<-210            ## Degrres of towerX
towerYAngle<-330            ## Degrres of tower
towerZAngle<-90             ## Degrres of tower
dTower1x<-(cos(towerXAngle*pi/180)*dRadius)
dTower1y<-(sin(towerXAngle*pi/180)*dRadius)
dTower2x<-(cos(towerYAngle*pi/180)*dRadius)
dTower2y<-(sin(towerYAngle*pi/180)*dRadius)
dTower3x<-(cos(towerZAngle*pi/180)*dRadius)
dTower3y<-(sin(towerZAngle*pi/180)*dRadius)

cartesianToDelta<-function(X,Y,Z) {
  dX<-sqrt(dDiagonalRod^2 - (dTower1x-X)^2 - (dTower1y-Y)^2) + Z - 173.2051
  dY<-sqrt(dDiagonalRod^2 - (dTower2x-X)^2 - (dTower2y-Y)^2) + Z - 173.2051
  dZ<-sqrt(dDiagonalRod^2 - (dTower3x-X)^2 - (dTower3y-Y)^2) + Z - 173.2051
  return(c(dX,dY,dZ))
}

.deltaToCartesian<-function(values,delta) {
  X<-values[1]
  Y<-values[2]
  Z<-values[3]
  dX<-delta[1]+173.2051-Z
  dY<-delta[2]+173.2051-Z
  dZ<-delta[3]+173.2051-Z
  error1<-(sqrt((X-dTower1x)^2 + (Y-dTower1y)^2 + dX^2) - dDiagonalRod)^2
  error2<-(sqrt((X-dTower2x)^2 + (Y-dTower2y)^2 + dY^2) - dDiagonalRod)^2
  error3<-(sqrt((X-dTower3x)^2 + (Y-dTower3y)^2 + dZ^2) - dDiagonalRod)^2
  return(sum(error1,error2,error3))
}
deltaToCartesian<-function(dX=0,dY=0,dZ=0) {
  if(length(dX==3)) {
    dY<-dX[2]
    dZ<-dX[3]
    dX<-dX[1]
  }
  X<-Y<-Z<-10
  model<-optim(par=list(X,Y,Z),.deltaToCartesian,method="BFGS",delta=c(dX,dY,dZ))
  return(model$par)
}

TARGET<-function(startX,startY,startZ,endX,endY,endZ) {
  return(c(startX,startY,startZ,endX,endY,endZ))
}

current<-TARGET(50,50,0,-50,-50,0)
current<-TARGET(0,0,0,2.5,5,0)
current<-TARGET(2.5,5,0,5,10,0)

B<-500
moves<-matrix(NA,nrow=B,ncol=3)
model.deltaCoord<-matrix(NA,nrow=B,ncol=3)
model.deltaError<-matrix(NA,nrow=B,ncol=3)
initial<-cartesianToDelta(current[1],current[2],current[3])
end<-cartesianToDelta(current[4],current[5],current[6])-initial
for(i in 1:B) {
  moves[i,]<-c(current[1]+(current[4]-current[1])*i/B,current[2]+(current[5]-current[2])*i/B,current[3]+(current[6]-current[3])*i/B)
  model.deltaCoord[i,]<-cartesianToDelta(current[1]+(current[4]-current[1])*i/B,current[2]+(current[5]-current[2])*i/B,current[3]+(current[6]-current[3])*i/B)
  model.deltaError[i,]<-deltaToCartesian(c(initial[1]+end[1]*i/B,initial[2]+end[2]*i/B,initial[3]+end[3]*i/B))-c(current[1]+(current[4]-current[1])*i/B,current[2]+(current[5]-current[2])*i/B,current[3]+(current[6]-current[3])*i/B)
}

plot(1:B,model.deltaCoord[,1],type="l",col="red",ylim=c(min(model.deltaCoord),max(model.deltaCoord)))
lines(1:B,model.deltaCoord[,2],col="blue")
lines(1:B,model.deltaCoord[,3],col="green")

plot(1:B,model.deltaError[,1],type="l",col="red",ylim=c(min(model.deltaError),max(model.deltaError)))
lines(1:B,model.deltaError[,2],col="blue")
lines(1:B,model.deltaError[,3],col="green")

#####     To check the effect of joining two moves
temp1<-model.deltaCoord
temp2<-model.deltaError

model.deltaCoord<-rbind(temp1,model.deltaCoord)
model.deltaError<-rbind(temp2,model.deltaError)
B<-500
plot(1:B,model.deltaCoord[,1],type="l",col="red",ylim=c(min(model.deltaCoord),max(model.deltaCoord)))
lines(1:B,model.deltaCoord[,2],col="blue")
lines(1:B,model.deltaCoord[,3],col="green")

plot(1:B,model.deltaError[,1],type="l",col="red",ylim=c(min(model.deltaError),max(model.deltaError)))
lines(1:B,model.deltaError[,2],col="blue")
lines(1:B,model.deltaError[,3],col="green")
#####     End check two joining moves


temp<-model.deltaCoord[-1,]-model.deltaCoord[-B,]
moves<-cbind(moves,moves[,1]^2)
model<-lm(temp[,1]~moves[-B,])
summary(model)

temp<-model.deltaCoord[-1,]-model.deltaCoord[-B,]-0.15-0.00355*moves[-B,1]
plot(1:499,temp[,1])
