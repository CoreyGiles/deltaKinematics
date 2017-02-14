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


