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
X<-X*radius
Y<-Y*radius
Z<-Z*height

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

############## Functions to adjust carriage speed to attain suitable effector speed
feedRate<-function(speed,X1,X2,Y1,Y2,Z1,Z2,dX1,dX2,dY1,dY2,dZ1,dZ2) {
  X.diff<-X2-X1
  Y.diff<-Y2-Y1
  Z.diff<-Z2-Z1
  dist<-sqrt(X.diff^2+Y.diff^2+Z.diff^2)
  time<-dist/speed
  
  dX.diff<-dX2-dX1
  dY.diff<-dY2-dY1
  dZ.diff<-dZ2-dZ1
  ddist<-sqrt(dX.diff^2+dY.diff^2+dZ.diff^2)
  return(speed*ddist/dist)
}

writeGCode<-function(X,Y,Z,Feed) {
  print(paste("G1 X",round(X,2),"Y",round(Y,2),"Z",round(Z,2),"F",round(Feed,0),sep=" "))
}

speed<-20000 #mm/min
Fvalue<-numeric(length(X))

Fvalue[1]<-feedRate(speed,0,X[1],0,Y[1],0,Z[1],0,dX[1],0,dY[1],0,dZ[1])
for(i in 2:length(X)) {
  Fvalue[i]<-feedRate(speed,X[i-1],X[i],Y[i-1],Y[i],Z[i-1],Z[i],dX[i-1],dX[i],dY[i-1],dY[i],dZ[i-1],dZ[i])
}

Sprial.delta.mod.feedrate<-writeGCode(dX,dY,dZ,Fvalue)
write.csv(Sprial.delta.mod.feedrate,file=paste("spiral.speed.change.",speed,".gcode",sep=""),row.names=FALSE, quote=FALSE)


##  20,000 mm/min is the maximum effector speed before combined axis speed exceeds 30,000 mm/min (500 mm/s)
maxAxisSpeed<-function(speed,X1,X2,Y1,Y2,Z1,Z2,dX1,dX2,dY1,dY2,dZ1,dZ2) {
  X.diff<-X2-X1
  Y.diff<-Y2-Y1
  Z.diff<-Z2-Z1
  dist<-sqrt(X.diff^2+Y.diff^2+Z.diff^2)
  time<-dist/speed
  
  dX.diff<-dX2-dX1
  dY.diff<-dY2-dY1
  dZ.diff<-dZ2-dZ1
  return(list(max=max(dX.diff/time,dY.diff/time,dZ.diff/time),min=min(dX.diff/time,dY.diff/time,dZ.diff/time)))
}
maxMotorSpeed<-numeric(length(X))
maxMotorSpeed[1]<-maxAxisSpeed(speed,0,X[1],0,Y[1],0,Z[1],0,dX[1],0,dY[1],0,dZ[1])$max
for(i in 2:length(X)) {
  maxMotorSpeed[i]<-maxAxisSpeed(speed,X[i-1],X[i],Y[i-1],Y[i],Z[i-1],Z[i],dX[i-1],dX[i],dY[i-1],dY[i],dZ[i-1],dZ[i])$max
}
min(maxMotorSpeed)
max(maxMotorSpeed)
plot(1:length(X),maxMotorSpeed)

##  Max motor speed exceeds effector speed by nearly 15 %
##  Effector speed: 12000 mm/min (200 mm/s)
##  Carriage speed: 13686 mm/min (228 mm/s)

##  Max step rate achieved per segment (value returned is steps per second)
maxStepRate<-function(speed,X1,X2,Y1,Y2,Z1,Z2,dX1,dX2,dY1,dY2,dZ1,dZ2) {
  X.diff<-X2-X1
  Y.diff<-Y2-Y1
  Z.diff<-Z2-Z1
  dist<-sqrt(X.diff^2+Y.diff^2+Z.diff^2)
  time<-dist*60/speed
  
  dX.diff<-dX2-dX1
  dY.diff<-dY2-dY1
  dZ.diff<-dZ2-dZ1
  steps<-abs(dX.diff)+abs(dY.diff)+abs(dZ.diff)*80 #80 steps/mm for each axis
  return(steps/time)
}
maxStepsTotalRate<-numeric(length(X))
maxStepsTotalRate[1]<-maxStepRate(speed,0,X[1],0,Y[1],0,Z[1],0,dX[1],0,dY[1],0,dZ[1])
for(i in 2:length(X)) {
  maxStepsTotalRate[i]<-maxStepRate(speed,X[i-1],X[i],Y[i-1],Y[i],Z[i-1],Z[i],dX[i-1],dX[i],dY[i-1],dY[i],dZ[i-1],dZ[i])
}
min(maxStepsTotalRate)
max(maxStepsTotalRate)
plot(1:length(X),maxStepsTotalRate)

##  Max step rate for 12000 mm/min is 18195 steps/second across all steppers
##  At 20000 mm/min, step rate maxes at 30,326 steps/second

