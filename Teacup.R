#################     This is the Teacup firware written in R

##    Initial printer and board setup
F_CPU<-16000000                 ## Hz

STEPS_PER_M_X<-80000            ## steps/m
STEPS_PER_M_Y<-80000            ## steps/m
STEPS_PER_M_Z<-80000            ## steps/m
STEPS_PER_M_E<-50000            ## steps/m

MAXIMUM_FEEDRATE_X<-30000       ## mm/min
MAXIMUM_FEEDRATE_Y<-30000       ## mm/min
MAXIMUM_FEEDRATE_Z<-30000       ## mm/min
MAXIMUM_FEEDRATE_E<-30000       ## mm/min

ACCELERATION<-1000              ## mm/s^2

MAX_JERK_X<-200                 ## mm/min
MAX_JERK_Y<-200                 ## mm/min
MAX_JERK_Z<-200                 ## mm/min
MAX_JERK_E<-200                 ## mm/min

dDiagonalRod<-200000           ## Diagonal rod 200 mm (200000 um)
dRadius<-100000                ## Projected radius downward from diagonal rod (100000 um)
towerXAngle<-210            ## Degrees of towerX
towerYAngle<-330            ## Degrees of tower
towerZAngle<-90             ## Degrees of tower
dTower1x<-(cos(towerXAngle*pi/180)*dRadius)
dTower1y<-(sin(towerXAngle*pi/180)*dRadius)
dTower2x<-(cos(towerYAngle*pi/180)*dRadius)
dTower2y<-(sin(towerYAngle*pi/180)*dRadius)
dTower3x<-(cos(towerZAngle*pi/180)*dRadius)
dTower3y<-(sin(towerZAngle*pi/180)*dRadius)

##    Some precomputed values
c_initial<-c(F_CPU/sqrt(STEPS_PER_M_X*ACCELERATION/2000),
             F_CPU/sqrt(STEPS_PER_M_Y*ACCELERATION/2000),
             F_CPU/sqrt(STEPS_PER_M_Z*ACCELERATION/2000),
             F_CPU/sqrt(STEPS_PER_M_E*ACCELERATION/2000))       ## c0_P

##    Save delta cartesian coordinates. Steps are left in absolute location -> will be changes just after
carthesian_to_carthesian<-function(start,target,delta_um,steps) {
  delta_um[1]<-abs(start$axis$X-target$axis$X)
  steps[1]<-target$axis$X*STEPS_PER_M_X/1000000
  delta_um[2]<-abs(start$axis$Y-target$axis$Y)
  steps[2]<-target$axis$Y*STEPS_PER_M_Y/1000000
  delta_um[3]<-abs(start$axis$Z-target$axis$Z)
  steps[3]<-target$axis$Z*STEPS_PER_M_Z/1000000
  delta_um<<-delta_um
  steps<<-steps
}

cartesianToDelta<-function(X,Y,Z) {
  dX<-sqrt(dDiagonalRod^2 - (dTower1x-X)^2 - (dTower1y-Y)^2) + Z
  dY<-sqrt(dDiagonalRod^2 - (dTower2x-X)^2 - (dTower2y-Y)^2) + Z
  dZ<-sqrt(dDiagonalRod^2 - (dTower3x-X)^2 - (dTower3y-Y)^2) + Z
  return(c(dX,dY,dZ))
}

##    Save direction of axis movement
set_direction<-function(dda,axis,delta_steps) {
  dir<-as.integer(delta_steps>=0)
  if(axis==1) {
    dda$X_dir<-dir
  } else if(axis==2) {
    dda$Y_dir<-dir
  } else if(axis==3) {
    dda$Z_dir<-dir
  }
  dda<<-dda
}

##    Return the axis direction flag
get_direction<-function(dda,axis) {
  if(axis==1) {
    return(dda$X_dir)
  } else if(axis==2) {
    return(dda$Y_dir)
  } else if(axis==3) {
    return(dda$Z_dir)
  }
}

##    Calculate the accepation ramp length in number of steps (?)
acc_ramp_len<-function(speed) {
  return((speed*speed)/((7200000*ACCELERATION)/STEPS_PER_M_X))
}

##    Calculate maximum speed at which we can cross two moves
##    Interestingly, we set the cross over speed to the lowest of the two moves
##    This prevents slow down when jerk is exceeded by the second move having a higher speed
dda_find_crossing_speed<-function(prev=NULL,current) {
  prevF<-integer(3)
  currF<-integer(3)
  dv<-integer(1)
  
  if(is.null(prev))  {
    return(current)
  }
  
  F<-prev$endpoint$F
  if(current$endpoint$F<F) {
    F<-current$endpoint$F
  }
  
  for(i in 1:3) {
    prevF[i]<-as.integer(prev$delta[[i]]*F/prev$total_steps)
    currF[i]<-as.integer(current$delta[[i]]*F/current$total_steps)
  }
  
  max_speed_factor<-1e99
  for(i in 1:3) {
    if(get_direction(prev,i)==get_direction(current,i)) {
      dv<-abs(prevF[i]-currF[i])
    } else {
      dv<-currF[i]-prevF[i]
    }
    
    if(dv) {
      speed_factor<-bitwShiftL(MAX_JERK_X,8)/dv
      if(speed_factor<max_speed_factor) {
        max_speed_factor<-speed_factor
      }
    }
  }
  if(max_speed_factor>=bitwShiftL(1,8)) {
    current$crossF<-F
  } else {
    current$crossF<-bitwShiftR((F*max_speed_factor),8)
  }
  return(current)
}

#######################################   DDA_CREATE()    #############################################

##    Initial target and previous target variables. These are modified GCODE entries
start<-list(axis=list(X=as.integer(0),Y=as.integer(0),Z=as.integer(0)),F=as.integer(0))    ### Axis locations in um
nextTarget<-list(axis=list(X=as.integer(10100),Y=as.integer(3500),Z=as.integer(200)),F=as.integer(3000))    ### Axis locations in um

##    For acceleration temporal
step_interval<-list(X=0xFFFFFFFF,Y=0xFFFFFFFF,Z=0xFFFFFFFF)

##    Inside DDA_Create
prev_dda<-NULL
dda<-list(endpoint=nextTarget,delta=list(X=integer(0),Y=integer(0),Z=integer(0)), crossF=0, start_steps=0, end_steps=0,step_interval=step_interval,c=0xFFFFFFFF)
delta_um<-integer(3)
steps<-integer(3)

##    Move state tracker
counter<-list(X=0,Y=0,Z=0)
time<-list(X=0,Y=0,Z=0)
move_state<-list(counter=counter,time=time,endstop_stop=0)

carthesian_to_carthesian(start,nextTarget,delta_um,steps)
for(i in 1:3) {
  delta_steps<-integer(1)
  
  delta_steps<-steps[i]-start$axis[[i]]     # Convert the position in steps to change in steps
  dda$delta[[i]]<-abs(delta_steps)          # save the number of steps to take during movement
  start$steps$axis[i]<-steps[i]             # Save axis step location
  
  set_direction(dda,i,delta_steps)          # Set direction flag for axis movement
}

## Set extruder steps...      Not done here

##  Setup fast axis steps, um and axis number
for(i in 1:3) {
  if(i==1 || dda$delta[[i]]>dda$total_steps) {
    dda$total_steps<-dda$delta[[i]]
    dda$fast_um<-delta_um[i]
    dda$fast_axis<-i
  }
}

#######################################   Acceleration Ramping   #############################################
if(dda$total_steps!=0) {
  distance<-as.integer(sqrt(sum(delta_um^2)))
  
  ##  Calculate move speed in mm.us / (step.min)
  ##  mm (distance) * 60000000 (us/min) / steps (total steps)
  ##  mm.us / (step.min)   /   mm/min (feedrate)   =  us / step (c)
  move_duration<-as.integer(((distance*2400)/dda$total_steps)*(F_CPU/40000))
  ##           <-(distance/1000)*(60*F_CPU)/dda$total_steps    mm*(1 minute of CPU ticks)/steps
  ##  consider changing F to us/mm to change the divide into a multiply
  ##  3000 mm/min -> 50 mm/sec -> 0.02 sec/mm - > 20,000 us/mm
  ##  distance*(F_CPU/1000000)/1000 -> distance*(F_CPU/1000)
  ##  mm (distance) / steps = mm/step
  # equivalent old way (distance/1000)*(60*F_CPU)/dda$total_steps       / feedrate (mm/min)
  # potential new way (distance)*(F_CPU/1000000)/dda$total_steps        * feedrate (us/um)
  
  ##  This sets the minimum time between steps (c)
  ##  Smaller number means faster!    i.e. smaller number of ticks between stepper pulses
  c_limit<-0
  for(i in 1:3) {
    c_limit_calc<-(delta_um[i]*24000)/MAXIMUM_FEEDRATE_X
    if(c_limit_calc>c_limit) {
      c_limit<-c_limit_calc
    }
  }
  c_limit<-c_limit/dda$total_steps * (F_CPU / 40000)
  
  dda$c_min<-as.integer(move_duration/dda$endpoint$F)             # This would be multiply with the above
  if(dda$c_min<c_limit) {
    dda$c_min<-c_limit
    dda$endpoint$F<-move_duration/dda$c_min
  }
  
  dda$rampup_steps<-as.integer(acc_ramp_len(dda$fast_um*dda$endpoint$F/distance))
  if(dda$rampup_steps>dda$total_steps/2) {
    dda$rampup_steps<-dda$total_steps/2
  }
  dda$rampdown_steps<-dda$total_steps-dda$rampup_steps
  
  dda$distance<-distance
  dda<-dda_find_crossing_speed(prev_dda,dda)
  #Join moves
}

#######################################   Acceleration Temporal   #############################################
if(dda$total_steps!=0) {
  distance<-as.integer(sqrt(sum(delta_um^2)))
  
  move_duration<-integer(1)
  md_candidate<-integer(1)
  
  ###   Slightly different from ramping equation
  move_duration<-distance*((F_CPU*60)/(dda$endpoint$F*1000))                  ## mm.(ticks/min) / (mm.min) = ticks for whole move
  for(i in 1:3) {
    md_candidate<-dda$delta[[i]]*((F_CPU*60)/(MAXIMUM_FEEDRATE_X*1000))       ## Will slow down the whole movement if one axis is slower
    if(md_candidate>move_duration) {
      move_duration<-md_candidate                                             ## md_candidate is in steps, not um
    }
  }
  
  c_limit<-0
  for(i in 1:3) {
    c_limit_calc<-(delta_um[i]*24000)/MAXIMUM_FEEDRATE_X
    if(c_limit_calc>c_limit) {
      c_limit<-c_limit_calc
    }
  }
  c_limit<-c_limit/dda$total_steps * (F_CPU / 40000)
  
  for(i in 1:3) {
    dda$step_interval[[i]]<-0xFFFFFFFF
    if(dda$delta[[i]]) {
      dda$step_interval[[i]]<-move_duration/dda$delta[[i]]
    }
  }
  
  dda$c<-0xFFFFFFFF
  dda$axis_to_step<-1
  for(i in 1:3) {
    if(dda$step_interval[[i]]<dda$c) {
      dda$axis_to_step<-i
      dda$c<-dda$step_interval[[i]]
    }
  }
  
  prev_dda<-dda
}


#######################################    DDA_START()   #############################################
#x_direction(dda->x_direction)
#y_direction(dda->y_direction)
#z_direction(dda->z_direction)
#e_direction(dda->e_direction)

move_state$counter$X<-move_state$counter$Y<-move_state$counter$Z<- -bitwShiftR(dda$total_steps,1)
move_state$endstop_stop<-0
move_state$steps<-dda$delta
move_state$time$X<-move_state$time$Y<-move_state$time$Z<-0
dda$live<-1
#timer_set(dda$c,0)

#######################################    DDA_STEP()   #############################################



#######################################    DDA_CLOCK()   #############################################

#If we calculate the length of time for the total movement in seconds: move_duration (seconds)
#Then we can calculate how far each axis has to travel each second: travel[i]<-delta_um[i]/move_duration = (um/s)
#Then we put the travel/second into the partial differential equation:
#      delta_dx<-(dTower1x+dTower1y-X-Y)/dX        # This should give us the required move distance per second
#      delta_dy<-(dTower2x+dTower2y-X-Y)/dy        # um per second
#      delta_dz<-(dTower3x+dTower3y-X-Y)/dz
#
#If we multiply  steps_per_second = delta_d* steps_Per_um, we should have number of steps/second
#1/steps_per_second = seconds/step       Multiply by F_CPU to get ticks per step.

#Update dda$step_interval with the new times
#If delta_d* is not the same sign as before, update *_direction(dda->*_direction)

###  UPDATE
#dda$delta is number of steps
#move_duration is number of ticks for whole move
#dda$delta/move_duration is steps per tick (will underflow!)
#if dTower** is measured in step lengths (dDiagonalRod*steps_per_M/1000), delta_d* will provide steps/tick
#  dx/(dTower1x+dTower1y-X-Y) is ticks/step

###  CORRECTION
#  delta_d* will prove change in d* per one unit change in X or Y (not per second)
#  Can we mix units? Make X and Y in mm to ensure delta_d* is always large?
  
  X<-Y<-Z<-50000
  temp<-round(cartesianToDelta(X,Y,Z),5)   ##  It looks like X/Y/Z should be in the same units as rod/radius
  dX<-temp[1]
  dY<-temp[2]
  dZ<-temp[3]
  delta_dx<-(dTower1x+dTower1y-X-Y)/dX      ##  This ends up being a ratio. Just slightly smaller or larger than 1/-1
  delta_dy<-(dTower2x+dTower2y-X-Y)/dY      ##  So it doesn't matter what units everything is in, as long as they are the same!
  delta_dz<-(dTower3x+dTower3y-X-Y)/dZ
  print(paste(round(delta_dx,2),round(delta_dy,2),round(delta_dz,2)))

  
