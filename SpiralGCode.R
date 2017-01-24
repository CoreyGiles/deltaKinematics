##  Radius to perform spiral
radius<-50
##  height of total movement
height<-400
circumference<-2*radius*pi

segmentsPerRevolution<-180

numberOfRotations<-16
numberOfOscilations<-4

segments<-0:(segmentsPerRevolution*numberOfRotations)

X<-sin((pi/180)*segments*360/segmentsPerRevolution)
Y<-cos((pi/180)*segments*360/segmentsPerRevolution)
Z<--cos((pi/180)*(segments/(numberOfRotations/numberOfOscilations))*360/segmentsPerRevolution)/2+0.5

firstRotation<-1:segmentsPerRevolution
X[firstRotation]<-X[firstRotation]*firstRotation/segmentsPerRevolution
Y[firstRotation]<-Y[firstRotation]*firstRotation/segmentsPerRevolution
lastHalfRotation<-(length(segments)-segmentsPerRevolution/2):length(segments)
Y[lastHalfRotation]<-Y[lastHalfRotation]*(length(segments)-lastHalfRotation)/(segmentsPerRevolution/2)

plot(1:length(segments),X)
plot(1:length(segments),Y)
plot(1:length(segments),Z)

spiralDataFrame<-data.frame(X=X*radius,Y=Y*radius,Z=Z*height)

outputGCode<-function(X,Y,Z) {
  print(paste("G1 X",round(X,2),"Y",round(Y,2),"Z",round(Z,2),sep=" "))
}

gCode<-apply(spiralDataFrame,1, function(x) outputGCode(x['X'],x['Y'],x['Z']))
write.csv(gCode,file="test.standard.txt",row.names=FALSE, quote=FALSE)


dDiagonalRod<-200
dRadius<-100
towerXAngle<-210
towerYAngle<-330
towerZAngle<-90
dTower1x<-(cos(towerXAngle*pi/180)*dRadius)
dTower1y<-(sin(towerXAngle*pi/180)*dRadius)
dTower2x<-(cos(towerYAngle*pi/180)*dRadius)
dTower2y<-(sin(towerYAngle*pi/180)*dRadius)
dTower3x<-(cos(towerZAngle*pi/180)*dRadius)
dTower3y<-(sin(towerZAngle*pi/180)*dRadius)

cartesianToDelta<-function(X,Y,Z) {
  dX<-sqrt(dDiagonalRod^2 - (dTower1x-X)^2 - (dTower1y-Y)^2) + Z - 173.21
  dY<-sqrt(dDiagonalRod^2 - (dTower2x-X)^2 - (dTower2y-Y)^2) + Z - 173.21
  dZ<-sqrt(dDiagonalRod^2 - (dTower3x-X)^2 - (dTower3y-Y)^2) + Z - 173.21
  #return(c(dX,dY,dZ)) 
  print(paste("G1 X",round(dX,2),"Y",round(dY,2),"Z",round(dZ,2),sep=" "))
}

cartesianToDelta(0,0,0)
deltaGCode<-apply(spiralDataFrame,1, function(x) cartesianToDelta(x['X'],x['Y'],x['Z']))
write.csv(deltaGCode,file="test.delta.txt",row.names=FALSE, quote=FALSE)
