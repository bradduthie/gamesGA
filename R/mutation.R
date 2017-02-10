#' Mutation function
#' Function that causes alleles to mutate in agents
#' @param agents A list of agents on which mutation will occur
#' @param prob The probability that a mutation will occur for any locus
#' @return agents The list of agents after mutation
mutation <- function(agents, prob = 0.01){
    for(i in 1:length(agents)){
        mutate <- runif(n=length(agents[[i]]));
        if(min(mutate) < prob){
            change <- which(mutate < prob); 
            newmut <- rbinom(n=length(change), size=1, prob=0.5);
            agents[[i]][change] <- newmut;
        }
    }
    return(agents);
}