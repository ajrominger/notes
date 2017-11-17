library(parallel)

setwd('~/Dropbox/Research/notes/islandDiv')

## function to simulate island evolution by linear birth-death-immigration
islandSim <- function(ages, maxAge, la, mu, nu) {
    ## matrix to hold spp
    spp <- matrix(0, nrow = 1000, ncol = length(ages))
    spp[1, 1] <- 1
    
    ## where do unborn species begin
    sppLatent <- 2
    
    ## vector to hold time
    tt <-  numeric(maxAge * (la + mu + nu) * 500)
    
    for(i in 2:length(tt)) {
        ## which islands are up
        islandsUp <- which(ages <= tt[i - 1])
        
        ## pick an event: 1 = speciation, 0 = immigration, -1 = extinction
        event <- sample(-1:1, 1, prob = c(mu, nu, la))
        # print(event)
        
        ## update time
        S0 <- sum(spp)
        if(S0 == 0) {
            # print('all dead')
            break
        }
        tt[i] <- tt[i - 1] + rexp(1, S0 * (la + mu + nu))
        if(tt[i] > maxAge) {
            # print('age limit hit')
            tt[i] <- 0
            break
        }
        
        ## pick island and spp for event to happen
        poss <- which(spp == 1, arr.ind = TRUE)
        thisOne <- as.integer(poss[sample(nrow(poss), 1), ])
        
        if(event == 1) {
            spp[sppLatent, thisOne[2]] <- 1
            sppLatent <- sppLatent + 1
            if(sppLatent > nrow(spp)) 
                spp <- rbind(spp, 
                             matrix(0, nrow = 1000, ncol = length(ages)))
        } else if(event == 0) {
            if(length(islandsUp) > 1) {
                nuIsland <- sample(islandsUp[islandsUp != thisOne[2]], 1)
                spp[thisOne[1], nuIsland] <- 1
            }
        } else {
            spp[thisOne[1], thisOne[2]] <- 0
        }
    }
    
    spp <- spp[1:(sppLatent + 1), ]
    return(colSums(spp))
}

## island ages for hawaii
maxAge <- 5
ages <- maxAge - c(5, 3, 1.76, 1.1, 0.4)

## vectors of possible parameters
La <- seq(2.5, 3.5, length.out = 5)
Mu <- seq(2.3, 3.2, length.out = 5)
Nu <- seq(0.5, 1.5, length.out = 3)

pars <- as.matrix(expand.grid(la = La, mu = Mu, nu = Nu))

islandDiv <- mclapply(1:nrow(pars), mc.cores = 6, FUN = function(i) {
    print(i)
    Sout <- replicate(100, islandSim(ages, maxAge, pars[i, 1], pars[i, 2], pars[i, 3]))
    
    out <- t(apply(Sout, 1, function(x) c(m = mean(x), 
                                          ci = quantile(x, c(0.025, 0.975), names = FALSE))))
    out <- cbind(age = maxAge - ages, out)
    
    return(cbind(pars[rep(i, nrow(out)), ], out))
})

islandDiv <- do.call(rbind, islandDiv)
write.csv(islandDiv, file = 'islandDiv.csv', row.names = FALSE)
