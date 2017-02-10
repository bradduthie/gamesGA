rm(list=ls(all=TRUE));

# Compile the fitness function with the command below
# R CMD SHLIB -o fitness.so fitness.c

setwd("~/Dropbox/projects/games_genetic_algorithm");

dyn.load('src/fitness.so');
source("R/crossover.R");
source("R/mutation.R");
source("R/fitness.R");
source("R/tournament.R");
source("R/sample_round.R");


agents <- NULL;

history_vec <- c(0,0,0,0,0,1,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,1,1,1);
history     <- matrix(data = history_vec, ncol = 3, byrow = TRUE);

for(i in 1:100){
    agents[[i]] <- rbinom(n=9, size=1, prob=0.5);
}



PD <- function(a1_play, a2_play){
    points <- 0;
    if(a1_play == 0 & a2_play == 0){
        points <- 2;
    }
    if(a1_play == 0 & a2_play == 1){
        points <- 0;
    }
    if(a1_play == 1 & a2_play == 0){
        points <- 3;
    }
    if(a1_play == 1 & a2_play == 1){
        points <- 1;
    }
    return(points);
}


mean_fitness <- NULL;
generations  <- 20;

gen <- 0;

while(gen < generations){
    
    agents  <- crossover(agents = agents, prob = 0.05);
    
    agents  <- mutation(agents = agents, prob = 0.05);
    
    fitness <- check_fitness(history = history, agents = agents);
    
    agents  <- tournament(agents = agents, fitness = fitness);
    
    mean_fitness[[gen+1]] <- mean(fitness);
    
    gen <- gen + 1;
    
    print(gen);
}


final_agent_vec <- unlist(agents);
final_agents    <- matrix(data=final_agent_vec, ncol = 9 , byrow = TRUE);


