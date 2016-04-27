#################################################################################
## Simulation of CDN CCC.
## (c) Llorenç Cerdà, 2016
#################################################################################

source("r6-cdn-ccc.R")
library('ggplot2')

run.simulation <- function(
    fname=NULL,
    sla.bw=5e6/8,  # SLA Bw in Bps (Bytes per second)
    cdn.load=0.5,
    background.load=0.05,
    Rweight=1e8, # alpha in equation (3)
    Pweight=1e8, # Penalty weight
    zrr.update.time=1,
    seed=0 # simulation seed
) {
    ##
    ## Prepare the simulation
    ##
    proxy.access.bw <- # Bw in Bps (Bytes per second) of each proxy
        c(rep(bw1, len=bw1.num),
          rep(bw2, len=bw2.num),
          rep(bw3, len=bw3.num))
    set.seed(seed)
    zrr <- ZoneRR$new(fname=fname,
                      N=number.of.proxies,
                      sla.bw=sla.bw,
                      proxy.access.bw=proxy.access.bw,
                      cdn.load=cdn.load,
                      bgd.load=background.load,
                      Rweight=Rweight,
                      Pweight=Pweight,
                      zrr.update.time=zrr.update.time)
    ##
    ## run the simulation
    ## 
    simu.percentage.time <- simu.time/100
    cat("Simulated 0%")
    system.time(
        while(simu.iterate() < simu.time) {
            if(simu.currtime > simu.percentage.time) {
                cat(sprintf("\rSimulated %3d%%", round(simu.percentage.time/simu.time*100)))
                simu.percentage.time <- simu.percentage.time+simu.time/100
            }
        })
    message("\ndone")
    zrr$close.trace()
    ## some statistics
    zrr$print()
    zrr
}

##
## Simulation parameters
##
debug.events <- F #  TRUE
number.of.proxies <- 10
simu.time <- 600 # seconds
## Proxies configuration
bw1 <- 10e6/8 # proxies with lower Bw
bw2 <- 20e6/8 # proxies with medium Bw
bw3 <- 100e6/8 # proxies with higher Bw
bw1.num <- round(number.of.proxies/3)
bw2.num <- round(number.of.proxies/3)
bw3.num <- number.of.proxies-2*round(number.of.proxies/3)
sla.bw <- 1e6/8

##
## run simulations varying weights (w) and update interval (ut)
##
simu.res <- list()
for(ut in c(0.1, 0.5, 1)) {
    for(w in c(1, 10, 100)) {
        ## for(ut in c(1)) {
        ##     for(w in c(10, 100)) {
        fname <- paste0(format(Sys.time(), "%Y-%m-%d-t"),
                        ut, "w", w, "s", simu.time, ".trace")
        message(fname)
        ##
        zrr <- run.simulation(
            fname=fname,
            sla.bw=sla.bw*1.5,  # SLA Bw in Bps (Bytes per second)
            cdn.load=0.5,
            background.load=0.01,
            Rweight=w*1000*sla.bw, # alpha in equation (3)
            Pweight=w*10*sla.bw, # Penalty weight
            zrr.update.time=ut,
            seed=0 # simulation seed
        )
        simu.res <- c(simu.res, list(list(zrr=zrr, ut=ut, w=w)))
    }
}

##
## facet_grid of all traces
##
trace.ta.all <- data.frame()
for(ut in c(0.1, 0.5, 1)) {
    for(w in c(1, 10, 100)) {
        fname <- paste0(format(Sys.time(), "%Y-%m-%d-t"),
                        ut, "w", w, "s", simu.time, ".trace")
        trace.ta.all <- rbind(trace.ta.all,
                              cbind(read.table(header=T, gzfile(paste0(fname, '.gz'))),
                                    ut=ut, w=w, Link=NA))
    }
}
trace.ta.all$Link[trace.ta.all$proxy %in% 1:bw1.num] <- as.character(bw1*8e-6)
trace.ta.all$Link[trace.ta.all$proxy %in% (bw1.num+1):(bw1.num+bw2.num)] <- as.character(bw2*8e-6)
trace.ta.all$Link[trace.ta.all$proxy %in% (bw1.num+bw2.num+1):number.of.proxies] <- as.character(bw3*8e-6)

trace.ta.all.cdn <- trace.ta.all[trace.ta.all$type=='cdn',]

ggplot(data=trace.ta.all.cdn, aes(throughput*8e-6, color=Link)) +
    facet_grid(ut ~ w) +
        stat_ecdf() +
            scale_x_log10() +
                geom_vline(xintercept = sla.bw*8e-6) + annotation_logticks(sides="b") +
                    xlab("throughput [Mbps] (log scale)") + ylab("ECDF")

ggsave(file="throughput-ecdf.pdf")

