#################################################################################
## Simulation of CDN CCC.
## (c) Llorenç Cerdà, 2016
#################################################################################

source("r6-cdn-ccc.R")
source("run-simulation.R")
library('ggplot2')

## library('utils') # Rprof()
## Rprof()

##
## Simulation parameters
##
debug.events <- F #  TRUE
number.of.proxies <- 10
simu.time <- 300 # seconds
## Proxies configuration
bw1 <- 10e6/8 # proxies with lower Bw
bw2 <- 20e6/8 # proxies with medium Bw
bw3 <- 100e6/8 # proxies with higher Bw
bw1.num <- round(number.of.proxies/3)
bw2.num <- round(number.of.proxies/3)
bw3.num <- number.of.proxies-2*round(number.of.proxies/3)
proxy.access.bw <- # Bw in Bps (Bytes per second) of each proxy
    c(rep(bw1, len=bw1.num),
      rep(bw2, len=bw2.num),
      rep(bw3, len=bw3.num))

##fname <- paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".trace")
sla.bw <- 1 # in Mbps
alpha <- 1

##
## testing
##
debug.events <- F
simu.time <- 20 # seconds
zrr <- run.simulation(
    proxy.access.bw=proxy.access.bw,
    sla.bw=sla.bw/alpha,  # SLA Bw in Mbps
    cdn.load=0.5,
    background.load=0, #0.05,
    Rweight=1e10, # gamma in equation (3)
    zrr.update.time=1,
    owd.mean=0,
    # owd.mean=20,
    owd.coef.var=0.1,
    choose.proxy.type='roundrobin',
    seed=0 # simulation seed
)

zrr$Qreq
zrr$Rqueue
zrr$Penalty

## print the queue of one proxy
zrr$proxies[[1]]$print.queues()
zrr$proxies[[2]]$print.queues()
zrr$proxies[[3]]$print.queues()
zrr$proxies[[4]]$print.queues()
zrr$proxies[[6]]$print.queues()

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
## check one trace
##
fname <- '2016-04-25-t0.1w100s600.trace'

##
trace.ta <- read.table(header=T, gzfile(paste0(fname, '.gz')))

trace.ta <- cbind(trace.ta, Bw=NA)
trace.ta$Bw[trace.ta$proxy %in% 1:bw1.num] <- as.character(bw1*8e-6)
trace.ta$Bw[trace.ta$proxy %in% (bw1.num+1):(bw1.num+bw2.num)] <- as.character(bw2*8e-6)
trace.ta$Bw[trace.ta$proxy %in% (bw1.num+bw2.num+1):number.of.proxies] <- as.character(bw3*8e-6)

ggplot(data=trace.ta, aes(QLen, color=Bw)) +
    stat_ecdf() +
        xlab("Queue Length") + ylab("ECDF")

## trace.ta.cdn$proxy <- as.factor(trace.ta.cdn$proxy)
trace.ta.cdn <- trace.ta[trace.ta$type=='cdn',]
ggplot(data=trace.ta.cdn, aes(throughput*8e-6, color=Bw)) +
    stat_ecdf() +
        scale_x_log10() +
            geom_vline(xintercept = sla.bw*8e-6) + annotation_logticks(sides="b") +
                xlab("throughput [Mbps] (log scale)") + ylab("ECDF")

## print Q and R queues of the ZRR
zrr$Qreq
zrr$Rqueue
zrr$Penalty

## print the queue of one proxy
zrr$proxies[[1]]$print.queues()
zrr$proxies[[2]]$print.queues()
zrr$proxies[[3]]$print.queues()
zrr$proxies[[4]]$print.queues()
zrr$proxies[[6]]$print.queues()

##
## facet_grid of all traces
##
trace.ta.all <- data.frame()
for(ut in c(0.1, 0.5, 1)) {
    for(w in c(1, 10, 100)) {
        ## for(ut in c(1)) {
        ##     for(w in c(10, 100)) {
        fname <- paste0("2016-04-25-t",
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

ggplot(data=trace.ta.all, aes(QLen, color=Link)) +
    facet_grid(ut ~ w) +
        stat_ecdf() +
            xlab("Queue Length") + ylab("ECDF")

ggsave(file="queue-length-ecdf.pdf")

## summaryRprof(filename = "Rprof.out")
