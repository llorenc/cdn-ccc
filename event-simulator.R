##########################################################################################
## simple event simulator support
##########################################################################################
library(R6)

simu.currtime <- 0.0   # current simulated time (seconds)
simu.events <- NULL    # event list

Event <- 
    R6Class("Event",
            portable = TRUE,
            cloneable = FALSE,
            public = list(
                time = NA,
                handle.event = function() { stop("no handle.event?") },
                initialize = function() {
                    self$time <- NA
                },
                sched.event = function(time) {
                    self$time <- simu.currtime + time
                    simu.insert.event(self)
                }
            ))
 
simu.init.event.sim <- function() {
  simu.currtime <<- 0.0
  simu.events <<- NULL
}

simu.insert.event <- function(event) {
  if(is.null(simu.events)) {
    simu.events <<- list(event)
  } else {
    if(event$time <= simu.events[[1]]$time) { # first position
      simu.events <<- c(list(event), simu.events)
    } else {
      number.of.events <- length(simu.events)
      if(event$time >= simu.events[[number.of.events]]$time) { # last position
        simu.events <<- c(simu.events, list(event))
      } else { ## find insertion point
        inspt <- simu.events.binsearch(number.of.events, event$time)
        simu.events <<- c(simu.events[1:(inspt-1)],list(event),simu.events[inspt:number.of.events])
      }
    }
  }
}

simu.iterate <- function() { 
  head <- simu.events[[1]]
  simu.currtime <<- head$time # update current simulated time
  if(length(simu.events) == 1) simu.events <<- NULL
  else simu.events <<- simu.events[-1]
  head$handle.event()
  return(simu.currtime)
}

##  binary search of insertion point of y in the sorted vector simu.events
simu.events.binsearch <- function(hi, y) {
  lo <- 1
  while(lo+1 < hi) {
    mid <- (lo+hi) %/% 2
    if(y == simu.events[[mid]]$time) return(mid)
    if(y < simu.events[[mid]]$time) hi <- mid else lo <- mid
  }
  if(y <= simu.events[[lo]]$time) return(lo)
  if(y < simu.events[[hi]]$time) return(hi)
  return(hi+1)
}
