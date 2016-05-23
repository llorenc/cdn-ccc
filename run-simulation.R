#################################################################################
## Simulation of CDN CCC.
## (c) Llorenç Cerdà, 2016
#################################################################################

run.simulation <- function(
    fname=NULL,
    proxy.access.bw,
    sla.bw=1,  # SLA Bw in Mbps
    cdn.load=0.5,
    background.load=0.05,
    Rweight=1e8, # alpha in equation (3)
    zrr.update.time=1,
    owd.mean=100, # ms
    owd.coef.var=0.1,
    choose.proxy.type='cost',
    max.queue.len=max.queue.len,
    seed=0 # simulation seed
) {
    ##
    ## Prepare the simulation
    ##
    set.owd.parameters(owd.mean, owd.coef.var)
    set.seed(seed)
    zrr <- ZoneRR$new(fname=fname,
                      N=number.of.proxies,
                      sla.bw=sla.bw,
                      proxy.access.bw=proxy.access.bw,
                      cdn.load=cdn.load,
                      bgd.load=background.load,
                      Rweight=Rweight,
                      zrr.update.time=zrr.update.time,
                      max.queue.len=max.queue.len,
                      choose.proxy.type=choose.proxy.type
                      )
    ##
    ## run the simulation
    ## 
    simu.percentage.time <- simu.time/100
    cat("Simulated 0%")
    sys.t <- system.time(
        while(simu.iterate() < simu.time) {
            if(simu.currtime > simu.percentage.time) {
                cat(sprintf("\rSimulated %3d%%", round(simu.percentage.time/simu.time*100)))
                simu.percentage.time <- simu.percentage.time+simu.time/100
            }
        })
    message("\ndone")
    print(sys.t)
    if(!is.null(fname)) {
        zrr$close.trace()
    }
    ## some statistics
    zrr$print()
    zrr
}
