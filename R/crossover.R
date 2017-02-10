#' Crossover function
#' Function that causes uniform crossing over among loci in a population of 
#' agents
#' @param agents A list of agents on which crossing over will be performed
#' @param prob The probability that crossing over happens at a locus
#' @return agents The list of agents after crossing over
crossover <- function(agents, prob = 0.1){
    for(i in 1:length(agents)){
        cross <- runif(n=length(agents[[i]]));
        if(min(cross) < prob){
            partner <- sample(x=1:length(agents), size = 1);
            switch  <- which(cross < prob);
            temp    <- agents[[i]][switch];
            agents[[i]][switch] <- agents[[partner]][switch];
            agents[[partner]][switch] <- temp;
        }
    }
    return(agents);
}