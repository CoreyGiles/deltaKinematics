max_jerk<-c(20,20,20)       ## mm/min
maximum_feedrate<-c(12000,12000,12000)       ## mm/min
steps_per_m<-80000
ACCELERATION<-1000      ##mm/s^2
F_CPU<-16000000             ## Hz

makeDDA<-function(endF=6000,delta_um=c(10000,10000,0)) {
  temp<-list(endF=endF,delta_um=delta_um)
  temp$total_steps<-max(abs(temp$delta_um*80))
  temp$distance<-sqrt(sum(temp$delta_um^2))
  temp$fast_um<-max(temp$delta_um)
  return(temp)
}
prev<-makeDDA()
current<-makeDDA()

## pre-calculate move speed in millimeter microseconds per step minute for less math in interrupt context
## mm (distance) * 60000000 us/min / step (total_steps) = mm.us per step.min
##  note: um (distance) * 60000 == mm * 60000000
## so in the interrupt we must simply calculate
## mm.us per step.min / mm per min (F) = us per step
#############   COREY - What if F was inverted - then this calculation above could be a multiplication
##
## uint32_t move_duration = ((distance * 2400) / dda->total_steps) * (F_CPU / 40000);

#move_duration<-current$distance*60000000/max(abs(current$delta_um*80))    ##  mm.us/(min.step)
move_duration<-current$distance*2400/current$total_steps*(F_CPU/40000)


#### COREY - The code below just checks whether the speed we designated for the carriage will cause
##  the speed on any of the axis to exceed the max axis speed - We can remove all this is we know the
##  gcode will never generate code above this limit!
c_limit<-0

#for (i = X; i < AXIS_COUNT; i++) {
#  c_limit_calc = (delta_um[i] * 2400L) / dda->total_steps * (F_CPU / 40000) / pgm_read_dword(&maximum_feedrate_P[i]);
#  if (c_limit_calc > c_limit)
#    c_limit = c_limit_calc;
#}
for(i in 1:3) {
  c_limit_calc<-(current$delta_um[i]*2400)/current$total_steps * (F_CPU/40000) / maximum_feedrate[i]
  if(c_limit_calc>c_limit) {
    c_limit<-c_limit_calc
  }
}

#dda->c_min = move_duration / dda->endpoint.F;
#if (dda->c_min < c_limit) {
#  dda->c_min = c_limit;
#  dda->endpoint.F = move_duration / dda->c_min;
#}
current$c_min<-move_duration/current$endF
if(current$c_min<c_limit) {
  current$c_min<-c_limit
  current$endF<-move_duration/current$c_min
}

#if (dda->endpoint.F > 65535)
#  dda->endpoint.F = 65535;


#dda->rampup_steps = acc_ramp_len(muldiv(dda->fast_um, dda->endpoint.F, distance), dda->fast_spm);
#uint32_t acc_ramp_len(uint32_t feedrate, uint32_t steps_per_m) {
#  return (feedrate * feedrate) /
#    (((uint32_t)7200000UL * ACCELERATION) / steps_per_m);
#}
acc_ramp_len<-function(feedrate,steps_per_m) {
  return((feedrate*feedrate)/(7200000*ACCELERATION/steps_per_m))
}
current$rampup_steps<-acc_ramp_len(current$fast_um*current$endF/current$distance,steps_per_m)


if (current$rampup_steps > current$total_steps / 2) {
  current$rampup_steps<-current$total_steps / 2;
}
current$rampdown_steps <- current$total_steps - current$rampup_steps;


#########################     Lookahead - crossing speed      #######################
## The speed change is based on the slower of the two speeds. This ensures that the algorithm doesn't stop the motion
## when the jerk is too high purely based on speed differences (two moves in the same direction, but different speeds)

F<-prev$endF
if(current$endF < F) {
  F<-current$endF
}

#for (i = X; i < AXIS_COUNT; i++) {
#  prevF[i] = muldiv(prev->delta_um[i], F, prev->distance);
#  currF[i] = muldiv(current->delta_um[i], F, current->distance);
#}
##  This iterates over each axis and finds the speed of each axis (the lowest speed avoids slowing down 
##  muldiv takes the distance to travel in each axis, multiplies by speed and divides by distance
##  speed (m/s) / distance (m)
##    = 1/time
prevF<-numeric(3)
currF<-numeric(3)
for(i in 1:3) {
  prevF[i]<-prev$delta_um[i]*F/prev$distance
  currF[i]<-current$delta_um[i]*F/current$distance
}

max_speed_factor<-bitwShiftL(2,8)

#for (i = X; i < AXIS_COUNT; i++) {
#  dv = currF[i] > prevF[i] ? currF[i] - prevF[i] : prevF[i] - currF[i];            dv<-abs(currF[i]-prevF[i])
#  if (dv) {
#    speed_factor = ((uint32_t)pgm_read_dword(&maximum_jerk_P[i]) << 8) / dv;       speed_factor<-bitwShiftL(max_jerk[i],8)/dv
#    if (speed_factor < max_speed_factor)
#      max_speed_factor = speed_factor;                                             
#  }
#}
for(i in 1:3) {
  dv<-abs(currF[i]-prevF[i])
  speed_factor<-bitwShiftL(max_jerk[i],8)/dv
  if(speed_factor<max_speed_factor) {
    max_speed_factor<-speed_factor
  }
}

#if (max_speed_factor >= ((uint32_t)1 << 8))
#  current->crossF = F;
#else
#  current->crossF = (F * max_speed_factor) >> 8;

if(max_speed_factor>=256) {
  current$crossF<-F
} else {
  current$crossF<-bitwShiftR(F*max_speed_factor,8)
}
    ###   To replace all this code, we just need to input a recomputed crossF
    ###   This has already been setup in GCODE inputs

#########################   COREY - join moves
#dda_join_moves(prev_dda, dda);
