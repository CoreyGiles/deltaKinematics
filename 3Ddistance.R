#uint32_t approx_distance_3(uint32_t dx, uint32_t dy, uint32_t dz) {
approx_distance<-function(dx,dy,dz) {
 if (dx<dy) {
   min<-dy
   med<-dx
 } else {
   min<-dx
   med<-dy
 }
  if (dz<min) {
    max<-med
    med<-min
    min<-dz
  } else if(dz< med) {
    max<-med
    med<-dz
  } else {
    max<-dz
  }
  approx<-(max*860 + (med*851) + (min*520))
  if(max<bitwShiftL(med,1)) approx<-approx-max*294
  if(max<bitwShiftL(min,2)) approx<-approx-max*113
  if(med<bitwShiftL(min,2)) approx<-approx-med*40
  return(bitwShiftR(approx+512,10))
}
approx_distance(20,10,10)
sqrt(20^2+10^2+10^2)

### Distances are entered in microns
##    Consider changeing distance to move_duration or something else to reduce longer calculations

##    pre-calculate move speed in millimeter microseconds per step minute for less math in interrupt context
##    mm (distance) * 60000000 us/min / step (total_steps) = mm.us per step.min
##    note: um (distance) * 60000 == mm * 60000000
##    so in the interrupt we must simply calculate
##    mm.us per step.min / mm per min (F) = us per step

##    // break this calculation up a bit and lose some precision because 300,000um * 60000 is too big for a uint32
##    calculate this with a uint64 if you need the precision, but it'll take longer so routines with lots of short moves may suffer
##    2^32/6000 is about 715mm which should be plenty

##    changed * 10 to * (F_CPU / 100000) so we can work in cpu_ticks rather than microseconds.
##    timer.c timer_set() routine altered for same reason

##    changed distance * 6000 .. * F_CPU / 100000 to
##    distance * 2400 .. * F_CPU / 40000 so we can move a distance of up to 1800mm without overflowing
## 		uint32_t move_duration = ((distance * 2400) / dda->total_steps) * (F_CPU / 40000);
