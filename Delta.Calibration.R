Delta<-list(
  diagonal=200,
  radius=100,
  homedHeight=300,
  xstop=0,
  ystop=0,
  zstop=0,
  xadj=0,
  yadj=0,
  zadj=0)

Delta$towerX<-c(-Delta$radius*cos((30+Delta$xadj)*pi/180),Delta$radius*cos((30-Delta$yadj)*pi/180),-Delta$radius*sin((Delta$zadj)*pi/180))
Delta$towerY<-c(-Delta$radius*sin((30+Delta$xadj)*pi/180),-Delta$radius*sin((30-Delta$yadj)*pi/180),Delta$radius*cos((Delta$zadj)*pi/180))

Delta$Xbc<-Delta$towerX[3]-Delta$towerX[2]
Delta$Xca<-Delta$towerX[1]-Delta$towerX[3]
Delta$Xab<-Delta$towerX[2]-Delta$towerX[1]
Delta$Ybc<-Delta$towerY[3]-Delta$towerY[2]
Delta$Yca<-Delta$towerY[1]-Delta$towerY[3]
Delta$Yab<-Delta$towerY[2]-Delta$towerY[1]

Delta$coreFa<-Delta$towerX[1]^2 + Delta$towerY[1]^2
Delta$coreFb<-Delta$towerX[2]^2 + Delta$towerY[2]^2
Delta$coreFc<-Delta$towerX[3]^2 + Delta$towerY[3]^2

Delta$Q<-2*(Delta$Xca*Delta$Yab-Delta$Xab*Delta$Yca)

Delta$homedCarriageHeight<-Delta$homedHeight+Delta$diagonal-inverseTransform(Delta$diagonal,Delta$diagonal,Delta$diagonal)
  
inverseTransform<-function(Ha,Hb,Hc) {
  Fa<-Delta$coreFa + Ha^2
  Fb<-Delta$coreFb + Hb^2
  Fc<-Delta$coreFc + Hc^2
  
  P<-Delta$Xbc*Fa + Delta$Xca*Fb + Delta$Xab*Fc
  S<-Delta$Ybc*Fa + Delta$Yca*Fb + Delta$Yab*Fc
  
  R<-2*(Delta$Xbc*Ha + Delta$Xca*Hb + Delta$Xab*Hc)
  U<-2*(Delta$Ybc*Ha + Delta$Yca*Hb + Delta$Yab*Hc)
  
  A<- U^2 + R^2 + Delta$Q^2
  
  minusHalfB<- S*U + P*R + Ha*Delta$Q^2 + Delta$towerX[1]*U*Delta$Q-Delta$towerY[1]*R*Delta$Q
  C<-(S + Delta$towerX[1]*Delta$Q)^2 + (P-Delta$towerY[1]*Delta$Q)^2 + (Ha^2-Delta$diagonal^2)*Delta$Q^2
  result<-(minusHalfB-sqrt(minusHalfB^2-A*C))/A
  return(result)
}


#########################################################################################
##
##    Perform calibration of a linear delta robot
##
#########################################################################################

dDiagonalRod<-200           ## Diagonal rod 200 mm
dRadius<-100                ## Projected radius downward from diagonal rod
towerXAngle<-210            ## Degrres of towerX
towerYAngle<-330            ## Degrres of tower
towerZAngle<-90             ## Degrres of tower

cartesianToDelta<-function(X,Y,Z) {
  dX<-sqrt(dDiagonalRod^2 - (dTower1x-X)^2 - (dTower1y-Y)^2) + Z
  dY<-sqrt(dDiagonalRod^2 - (dTower2x-X)^2 - (dTower2y-Y)^2) + Z
  dZ<-sqrt(dDiagonalRod^2 - (dTower3x-X)^2 - (dTower3y-Y)^2) + Z
  return(c(dX,dY,dZ))
}

deltaToCartesian<-function(dX,tower) {
  dY<-dX[2]
  dZ<-dX[3]
  dX<-dX[1]
  Fa<-tower[1]^2+tower[2]^2+dX^2
  Fb<-tower[3]^2+tower[4]^2+dY^2
  Fc<-tower[5]^2+tower[6]^2+dZ^2
  
  P<-(tower[5]-tower[3])*Fa + (tower[1]-tower[5])*Fb + (tower[3]-tower[1])*Fc
  S<-(tower[6]-tower[4])*Fa + (tower[2]-tower[6])*Fb + (tower[4]-tower[2])*Fc
  
  R<-2*((tower[5]-tower[3])*dX + (tower[1]-tower[5])*dY + (tower[3]-tower[1])*dZ)
  U<-2*((tower[6]-tower[4])*dX + (tower[2]-tower[6])*dY + (tower[4]-tower[2])*dZ)
  
  Q<-2*((tower[1]-tower[3])*(tower[4]-tower[2])-(tower[3]-tower[1])*(tower[2]-tower[6]))
  A<- U^2 + R^2 + Q^2
  
  minusHalfB<- S*U + P*R + dX*Q^2 + tower[1]*U*Q-tower[2]*R*Q
  C<-(S + tower[1]*Q)^2 + (P-tower[2]*Q)^2 + (dX^2-dDiagonalRod^2)*Q^2
  result<-(minusHalfB-sqrt(minusHalfB^2-A*C))/A
  return(result)
}


corr.A<-1
corr.B<-10
end.A<-5
end.B<-0
end.C<--3
corr.radius<-10
slope.x<-0
slope.y<-0


error<-matrix(0,nrow=20,ncol=3)
for(i in 1:nrow(error)) {
  error[i,]<-c(runif(1,-50,50),runif(1,-50,50),0)
  error[i,3]<-error[i,1]*0.1+error[i,2]*0.1
}


.calc<-function(values) {
  out<-numeric(nrow(error))
  corr.A<-values[1]
  corr.B<-values[2]
  end.A<-values[3]
  end.B<-values[4]
  end.C<-values[5]
  corr.radius<-values[6]
  slope.x<-values[7]
  slope.y<-values[8]
  dTower1x<-(cos((towerXAngle)*pi/180)*(dRadius))
  dTower1y<-(sin((towerXAngle)*pi/180)*(dRadius))
  dTower2x<-(cos((towerYAngle)*pi/180)*(dRadius))
  dTower2y<-(sin((towerYAngle)*pi/180)*(dRadius))
  dTower3x<-(cos(towerZAngle*pi/180)*(dRadius))
  dTower3y<-(sin(towerZAngle*pi/180)*(dRadius))
  dTower1x2<-(cos((towerXAngle+corr.A)*pi/180)*(dRadius+corr.radius))
  dTower1y2<-(sin((towerXAngle+corr.A)*pi/180)*(dRadius+corr.radius))
  dTower2x2<-(cos((towerYAngle+corr.B)*pi/180)*(dRadius+corr.radius))
  dTower2y2<-(sin((towerYAngle+corr.B)*pi/180)*(dRadius+corr.radius))
  dTower3x2<-(cos(towerZAngle*pi/180)*(dRadius+corr.radius))
  dTower3y2<-(sin(towerZAngle*pi/180)*(dRadius+corr.radius))
  for(i in 1:nrow(error)) {
    dX<-sqrt(dDiagonalRod^2 - (dTower1x-error[i,1])^2 - (dTower1y-error[i,2])^2)+end.A
    dY<-sqrt(dDiagonalRod^2 - (dTower2x-error[i,1])^2 - (dTower2y-error[i,2])^2)+end.B
    dZ<-sqrt(dDiagonalRod^2 - (dTower3x-error[i,1])^2 - (dTower3y-error[i,2])^2)+end.C
    out[i]<-(error[i,3]+error[i,1]*slope.x+error[i,2]*slope.y-deltaToCartesian(c(dX,dY,dZ),tower=c(dTower1x2,dTower1y2,dTower2x2,dTower2y2,dTower3x2,dTower3y2)))^2
  }
  return(sum(out))
}

calculation<-optim(par=list(corr.A,corr.B,end.A,end.B,end.C,corr.radius,slope.x,slope.y),.calc,method="BFGS")
calculation
