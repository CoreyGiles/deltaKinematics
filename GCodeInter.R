## Start of GCODE reader and modifier

Gcode<-read.csv(file="test.csv",header=F,sep=" ")

GcodeVal<-matrix(NA,nrow=nrow(Gcode),ncol=4)

strsplit(Gcode[1,2],split="")
for(line in 1:nrow(Gcode)) {
  temp<-(Gcode[line,])
  if(temp[1]=="G1") {
    var1<-substr(Gcode[line,2],1,1)
    var2<-substr(Gcode[line,3],1,1)
    var3<-substr(Gcode[line,4],1,1)
    var4<-substr(Gcode[line,5],1,1)
    GcodeVal[line,1]<-substr(temp[,which(c(NA,var1,var2,var3,var4)=="X")],2,7)
    GcodeVal[line,2]<-substr(temp[,which(c(NA,var1,var2,var3,var4)=="Y")],2,7)
    GcodeVal[line,3]<-substr(temp[,which(c(NA,var1,var2,var3,var4)=="Z")],2,7)
  }
}




###   Understanding how the Teacup firmware parses GCODE. Might be good use emulate
powers<-c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)

decodeFloatToInt<-function(mantissa,exponent,multi) {
  r<-mantissa
  
  if(exponent) {
    exponent<-exponent-1
  }
  
  while (exponent && multi %% 10 == 0) {
    multi<-multi/10;
    exponent<-exponent-1;
  }
  
  r<-r*multi
  if (exponent) {
    r = (r + powers[exponent] / 2) / powers[exponent]
  }
  return(r)
}

processChar<-function(c,mantissa,exponent) {
  mantissa<-bitwShiftL(mantissa,3)+bitwShiftL(mantissa,1)+c
  if(exponent) {
    exponent<-exponent+1
  }
  return(list(mantissa=mantissa,exponent=exponent))
}

stepThroughValue<-function(value) {
  n<-nchar(value)
  mantissa<-0
  exponent<-0
  for(i in 1:n) {
    character<-substr(value,i,i)
    if(character!=".") {
      temp<-processChar(as.numeric(character),mantissa,exponent)
      mantissa<-temp$mantissa
      exponent<-temp$exponent
    } else {
      exponent<-exponent+1
    }
  }
  decodeFloatToInt(mantissa,exponent,1000)
}

stepThroughValue(10.5)

###   Microns can be directly imported as integers
###   Remove most of the decode float to integer code
###   Multiple X, Y, Z, E(?) axis by 1000
###   No "." allowed
