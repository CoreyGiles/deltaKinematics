#################     This calculates the number of clock cycles between step pulses
#################     This is mostly what happens in dda_clock()
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

c_initial<-c(F_CPU/sqrt(STEPS_PER_M_X*ACCELERATION/2000),
             F_CPU/sqrt(STEPS_PER_M_Y*ACCELERATION/2000),
             F_CPU/sqrt(STEPS_PER_M_Z*ACCELERATION/2000),
             F_CPU/sqrt(STEPS_PER_M_E*ACCELERATION/2000))       ## c0_P

ACCELERATE_RAMP_LEN<-function(speed) {
  return(speed*speed/((7200000*ACCELERATION)/STEPS_PER_M_X))
}

move_state<-list()
move_state$step_no<-0
dda<-list()
dda$crossF<-1000
dda$rampup_steps<-500
dda$rampdown_steps<-500
dda$start_steps<-0
dda$total_steps<-10000
dda$end_steps<-round(ACCELERATE_RAMP_LEN(dda$crossF),0)
dda$fast_axis<-1


move_c<-c_initial[dda$fast_axis]


for(i in seq(5,10000,5)) {
  move_step_no<-move_state$step_no
  recalc_speed<-0
  move_no<-0
  
  if(move_step_no<dda$rampup_steps) {
    move_no<-dda$start_steps+move_step_no
    recalc_speed<-1
  } else if(move_step_no>=dda$rampdown_steps) {
    move_no<-dda$total_steps-move_step_no+dda$end_steps
    recalc_speed<-1
  }
  if(recalc_speed) {
    if(move_no==0) {
      move_c<-c_initial[dda$fast_axis]
    } else {
      move_c<-c_initial[dda$fast_axis]*(1/(2*sqrt(move_no)))
    } 
  }
  return(move_c)
}

temp<-numeric(100)
for(move_no in 1:100) {
  temp[move_no]<-print(c_initial[dda$fast_axis]*(1/(2*sqrt(move_no))))
}

temp3<-numeric(100)
for(move_no in 1:100) {
  temp3[move_no]<-print(c_initial[dda$fast_axis]*(sqrt(move_no+1)-sqrt(move_no)))
}

plot(1:100,temp,type="l")

diff<-function(value) {
  return(value[-1]-value[-length(value)])
}
plot(1:99,diff(temp))

temp2<-numeric(100)
for(move_no in 1:100) {
  if(move_no==1) {
    temp2[move_no]<-c_initial[1]-2*c_initial[1]/(4*move_no+1)
  } else {
    temp2[move_no]<-temp[move_no-1]-2*temp[move_no-1]/(4*move_no+1)
  }
}

plot(1:100,temp,type="l",col="red")
lines(1:100,temp2,type="l",col="blue")
lines(1:100,temp3,type="l",col="green")
