rm(list=ls())

# Load
library("ggthemes")
library("ggplot2")
library("egg")
library("gridExtra")
library("grid")
library("dplyr")
library("imager")
library("fs")
library(png)

u.lambda <- function(w1, w2, w3, w4, beta, lambda){
        alpha.star <- (-(1-lambda)*beta*w1 + (1-lambda)*beta*w2 - lambda*beta*w3 - (1-beta)*w3 + lambda*beta*w4 + (1-beta)*w4)/((lambda*w1) - (lambda*w2) - (lambda*w3) + (lambda*w4)) # This is the formula for the slope of u(...) for different values of lambda.
        return(alpha.star)
}

titles <- c("Basic collective action: No Monitoring", "Only Collective Monitoring Benefits", "Only Private Monitoring Benefits", "Collective and Private Monitoring Benefits")

lambda <- seq(0,1,.0001)

gameA <- c(2, 1, -1, 0) # includes p=0, c=0.
gameB <- c(2.25, 1.25, -0.75, 0) # includes p=0, c=0.25, i.e., only collective benefits from monitoring.
gameC <- c(2.25, 1, -0.75, 0) # includes p=0.25, c=0, i.e., only private benefits from monitoring.
gameD <- c(2.5, 1.25, -0.5, 0) # includes p=0.25, c=0.25, i.e., both private and collective benefits from monitoring.

beliefs <- c(1/4, 1/3, 4/10, 4/9, 2/3)

# Limits are defined as follows: if the mixing probability of a given game (A-D) is greater than the belief [1]-[5],
## then "max", if it is smaller than the belief, then "min". If equal, then no trace possible.
## For example, gameA's MSNE is 0.5, which is greater than 1/4, 1/3, 4/10, and 4/9; but smaller than 2/3: Hence "max, max, max, max, min".
limits <- cbind(c("max", "max", "max", "max", "min"),
                c("max", "max", "max", "min", "min"),
                c("max", "max", "min", "min", "min"),
                c("max", "min", "min", "min", "min"))
xlim <- cbind(rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA, 5))
ystable <- cbind(rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA, 5))
yvarying <- cbind(rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA, 5))

names(beliefs) <- c("lo", "medlo","medium", "medhi", "hi")
names(gameA) <- c("w1", "w2", "w3", "w4")
names(gameB) <- c("w1", "w2", "w3", "w4")
names(gameC) <- c("w1", "w2", "w3", "w4")
names(gameD) <- c("w1", "w2", "w3", "w4")

games <- cbind(gameA, gameB, gameC, gameD)

for(a in seq(1,4,1)){
        for(b in seq(1,5,1)){
                
                alpha.mix <- rep(NA, 10001)
                k <- 1
                for (i in seq(0,1, .0001)){
                        alpha.mix[k] <- ifelse(u.lambda(games[1,a], games[2,a], games[3,a], games[4,a], beliefs[b],i)>1, NA,
                                               ifelse(u.lambda(games[1,a], games[2,a], games[3,a], games[4,a], beliefs[b],i)<0, NA,
                                                      u.lambda(games[1,a], games[2,a], games[3,a], games[4,a], beliefs[b],i)))
                        k <- k+1  
                }
                
                alpha.coop <- rep(1, 10001)
                alpha.def <- rep(0, 10001)
                
                d <- as.data.frame(cbind(lambda, alpha.coop, alpha.def, alpha.mix))
                
                if (limits[b,a]=="max"){
                        d$alpha.coop <- ifelse(d$alpha.mix>1, NA, ifelse(d$alpha.mix<0, NA, d$alpha.coop))
                        xlim[b,a] <- summary(d$lambda[d$alpha.mix == max(d$alpha.mix, na.rm=TRUE)])[[1]]
                        ystable[b,a] <- 0
                        yvarying[b,a] <- 1
                } else {
                        d$alpha.def <- ifelse(d$alpha.mix>1, NA, ifelse(d$alpha.mix<0, NA, d$alpha.def))
                        xlim[b,a] <- summary(d$lambda[d$alpha.mix == min(d$alpha.mix, na.rm=TRUE)])[[1]]
                        ystable[b,a] <- 1
                        yvarying[b,a] <- 0
                }
                
                myplot <- ggplot(d, aes(x=lambda, y=alpha.mix))
                fullplot <- myplot +
                        geom_line() +
                        geom_segment(aes(x=xlim[b,a], xend=1, y=yvarying[b,a], yend=yvarying[b,a]), colour="black") +
                        geom_segment(aes(x=0, xend=1, y=ystable[b,a], yend=ystable[b,a]), colour="black") +
                        theme_minimal() +
                        labs(title = titles[a], y = expression(paste("Rationality (",alpha,")")), x = expression(paste("Belief about Other(s)’ Rationality (", lambda, ")"))) +
                        theme(panel.grid.minor = element_blank(),
                              plot.title = element_text(hjust=0.5, size=30),
                              axis.text=element_text(size=20),
                              axis.title=element_text(size=30),
                              axis.title.y=element_text(angle=-90, vjust = -1, size=20),
                              axis.title.x=element_text(hjust=0.5, vjust = -1, angle=0, size=20))
                fullplot
                if (labels(games)[[2]][a] == "gameA") {
                        if (labels(beliefs)[b] == "lo") {
                                gameA.lo <- fullplot
                        } else if (labels(beliefs)[b] == "medlo") {
                                gameA.medlo <- fullplot
                        } else if (labels(beliefs)[b] == "medium") {
                                gameA.med <- fullplot
                        } else if (labels(beliefs)[b] == "medhi") {
                                gameA.medhi <- fullplot
                        } else {
                                gameA.hi <- fullplot
                        }
                } else if (labels(games)[[2]][a] == "gameB") {
                        if (labels(beliefs)[b] == "lo") {
                                gameB.lo <- fullplot
                        } else if (labels(beliefs)[b] == "medlo") {
                                gameB.medlo <- fullplot
                        } else if (labels(beliefs)[b] == "medium") {
                                gameB.med <- fullplot
                        } else if (labels(beliefs)[b] == "medhi") {
                                gameB.medhi <- fullplot
                        } else {
                                gameB.hi <- fullplot
                        }
                } else if (labels(games)[[2]][a] == "gameC") {
                        if (labels(beliefs)[b] == "lo") {
                                gameC.lo <- fullplot
                        } else if (labels(beliefs)[b] == "medlo") {
                                gameC.medlo <- fullplot
                        } else if (labels(beliefs)[b] == "medium") {
                                gameC.med <- fullplot
                        } else if (labels(beliefs)[b] == "medhi") {
                                gameC.medhi <- fullplot
                        } else {
                                gameC.hi <- fullplot
                        }
                } else {
                        if (labels(beliefs)[b] == "lo") {
                                gameD.lo <- fullplot
                        } else if (labels(beliefs)[b] == "medlo") {
                                gameD.medlo <- fullplot
                        } else if (labels(beliefs)[b] == "medium") {
                                gameD.med <- fullplot
                        } else if (labels(beliefs)[b] == "medhi") {
                                gameD.medhi <- fullplot
                        } else {
                                gameD.hi <- fullplot
                        }
                }
                ggsave(paste("trace", "-", labels(games)[[2]][a],"-", labels(beliefs)[b], ".png", sep=""), width = 10, height = 10)
                
        }
}


#That actually blows so I'm going to try and print them all together as images


a_hi <- rasterGrob(readPNG('./trace-gameA-hi.png'))
a_medhi <- rasterGrob(readPNG('./trace-gameA-medhi.png'))
a_med <- rasterGrob(readPNG('./trace-gameA-medium.png'))
a_medlo <- rasterGrob(readPNG('./trace-gameA-medlo.png'))
a_lo <- rasterGrob(readPNG('./trace-gameA-lo.png'))
b_hi <- rasterGrob(readPNG('./trace-gameB-hi.png'))
b_medhi <- rasterGrob(readPNG('./trace-gameB-medhi.png'))
b_med <- rasterGrob(readPNG('./trace-gameB-medium.png'))
b_medlo <- rasterGrob(readPNG('./trace-gameB-medlo.png'))
b_lo <- rasterGrob(readPNG('./trace-gameB-lo.png'))
c_hi <- rasterGrob(readPNG('./trace-gameC-hi.png'))
c_medhi <- rasterGrob(readPNG('./trace-gameC-medhi.png'))
c_med <- rasterGrob(readPNG('./trace-gameC-medium.png'))
c_medlo <- rasterGrob(readPNG('./trace-gameC-medlo.png'))
c_lo <- rasterGrob(readPNG('./trace-gameC-lo.png'))
d_hi <- rasterGrob(readPNG('./trace-gameD-hi.png'))
d_medhi <- rasterGrob(readPNG('./trace-gameD-medhi.png'))
d_med <- rasterGrob(readPNG('./trace-gameD-medium.png'))
d_medlo <- rasterGrob(readPNG('./trace-gameD-medlo.png'))
d_lo <- rasterGrob(readPNG('./trace-gameD-lo.png'))



combined <- grid.arrange(a_hi, b_hi, c_hi, d_hi, a_medhi, b_medhi, c_medhi, d_medhi,
                         a_med, b_med, c_med, d_med, a_medlo, b_medlo, c_medlo, d_medlo,
                         a_lo, b_lo, c_lo, d_lo, ncol = 5, nrow=4)


grid.draw(combined)







### Combining individual graphs (currently doesn't work right!)
## A: Graph combines by game.
comb.gamaA <- ggarrange(gameA.lo, gameA.medlo, gameA.med, gameA.medhi, gameA.hi, nrow=5, ncol=1)
ggsave(comb.trace.gameA, filename = "tracecomb_gameA.png", width = 10, height = 50)

comb.gamaB <- ggarrange(gameB.lo, gameB.medlo, gameB.med, gameB.medhi, gameB.hi, nrow=5, ncol=1)
ggsave(comb.trace.gameB, filename = "tracecomb_gameB.png", width = 10, height = 50)

comb.gamaC <- ggarrange(gameC.lo, gameC.medlo, gameC.med, gameC.medhi, gameC.hi, nrow=5, ncol=1)
ggsave(comb.trace.gameC, filename = "tracecomb_gameC.png", width = 10, height = 50)

comb.gamaD <- ggarrange(gameD.lo, gameD.medlo, gameD.med, gameD.medhi, gameD.hi, nrow=5, ncol=1)
ggsave(comb.trace.gameD, filename = "tracecomb_gameD.png", width = 10, height = 50)


## B: Graph combines by belief.
comb.trace.lo <- ggarrange(gameA.lo, gameB.lo, gameC.lo, gameD.lo, nrow=2, ncol=2)
ggsave(comb.trace.lo, filename = "tracecomb_lo.png", width = 20, height = 20)

comb.trace.medlo <- ggarrange(gameA.medlo, gameB.medlo, gameC.medlo, gameD.medlo, nrow=2, ncol=2)
ggsave(comb.trace.medlo, filename = "tracecomb_medlo.png", width = 20, height = 20)

comb.trace.med <- ggarrange(gameA.med, gameB.med, gameC.med, gameD.med, nrow=2, ncol=2)
ggsave(comb.trace.medlo, filename = "tracecomb_medlo.png", width = 20, height = 20)

comb.trace.medhi <- ggarrange(gameA.medhi, gameB.medhi, gameC.medhi, gameD.medhi, nrow=2, ncol=2)
ggsave(comb.trace.medhi, filename = "tracecomb_medhi.png", width = 20, height = 20)

comb.trace.hi <- ggarrange(gameA.hi, gameB.hi, gameC.hi, gameD.hi, nrow=4, ncol=4)
ggsave(comb.trace.hi, filename = "tracecomb_hi.png", width = 20, height = 20)


## C: Giant graph combine.
comb.trace.all <- ggarrange(gameA.lo, gameB.lo, gameC.lo, gameD.lo,
                            gameA.medlo, gameB.medlo, gameC.medlo, gameD.medlo,
                            gameA.med, gameB.med, gameC.med, gameD.med,
                            gameA.medhi, gameB.medhi, gameC.medhi, gameD.medhi,
                            gameA.hi, gameB.hi, gameC.hi, gameD.hi, ncol=4, nrow=5)
ggsave(comb.trace.all, filename = "tracecomb_all.png", width = 40, height = 50)