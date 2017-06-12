#' 
#' Tournament function
#' 
#' Function that simulates selection of next generation of agents according to
#' their fitness
#' 
#' @param agents A list of agents to be assessed by fitness
#' @param fitness The fitness vector on which agents will be assessed
#' @return agents A new list of agents selected according to fitness
tournament <- function(agents, fitness){
    sum_fit    <- lapply(X = fitness, FUN = sum);
    tot_fit    <- unlist(sum_fit);
    new_agents <- NULL;
    for(i in 1:length(fitness)){
        rand_draw <- sample(x = 1:length(fitness), size = 10, replace = TRUE);
        maxfit    <- max(tot_fit[rand_draw]);
        take_draw <- which(tot_fit[rand_draw] == maxfit)[1];
        winner    <- rand_draw[take_draw];
        new_agents[[i]] <- agents[[winner]];
    } 
    return(new_agents);
}