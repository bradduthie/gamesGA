rm(list=ls(all=TRUE));

# Compile the fitness function with the command below
# R CMD SHLIB -o fitness.so fitness.c
# CC = 3, DC = 4, CD = -1, DD = 0 : usually results in cooperation

setwd("~/Dropbox/projects/games_genetic_algorithm");

dyn.load('src/fitness.so');
source("R/crossover.R");
source("R/mutation.R");
source("R/fitness.R");
source("R/tournament.R");
source("R/sample_round.R");

games_ga <- function(CC = 2, CD = 0, DC = 3, DD = 1, callC = TRUE, 
                     generations = 100, rounds = 20, num_opponents = 10,
                     cross_prob = 0.05, mutation_prob = 0.05){

    agents      <- NULL;
    history_vec <- c(0,0,0,0,0,1,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,1,1,1);
    history     <- matrix(data = history_vec, ncol = 3, byrow = TRUE);
    hist_pretty <- history;
    
    hist_pretty[hist_pretty==0]   <- "C";
    hist_pretty[hist_pretty=="1"] <- "D";
    hist_print                    <- NULL;
    
    for(i in 1:dim(hist_pretty)[1]){
        row        <- paste(hist_pretty[i,], collapse=" ");
        hist_print <- c(hist_print, row);
    }
    hist_print <- c(hist_print, "1st");

    for(i in 1:100){
        agents[[i]] <- rbinom(n=9, size=1, prob=0.5);
    }

    payoffs <- c(CC, CD, DC, DD);

    AGENTS       <- NULL;
    FITNESS      <- NULL;
    mean_fitness <- NULL;

    gen <- 0;

    while(gen < generations){
    
        agents  <- crossover(agents = agents, prob = cross_prob);
    
        agents  <- mutation(agents = agents, prob = mutation_prob);
    
        fitness <- check_fitness(history = history, agents = agents, 
                                 pay = payoffs, useC = callC, rounds = rounds,
                                 num_opponents = num_opponents);
    
        agents  <- tournament(agents = agents, fitness = fitness);
    
        gen <- gen + 1;
    
        AGENTS[[gen]]  <- agents;
        FITNESS[[gen]] <- fitness;
    }

    MEAN_FITNESS <- lapply(X=FITNESS, FUN=mean);
    MEAN_FITNESS <- unlist(MEAN_FITNESS);
    
    final_agent_vec <- unlist(agents);
    final_agents    <- matrix(data=final_agent_vec, ncol = 9 , byrow = TRUE);

    final_agents[final_agents==0]   <- "C";
    final_agents[final_agents=="1"] <- "D";
    strats <- NULL;
    for(i in 1:100){
        strats[[i]] <- paste(final_agents[i,], collapse=" ");
    }

    strats_tab <- table(strats);
    strats_fr  <- as.numeric(strats_tab);
    strats_or  <- order(strats_fr, decreasing=TRUE);
    strats_fr  <- strats_fr[strats_or];
    genomes    <- rownames(strats_tab[strats_or]);
    uni_genome <- length(genomes);

    tops <- matrix(data=0, nrow=uni_genome, ncol = 9);
    for(i in 1:uni_genome){
        split_geno <- strsplit(x=genomes[i], split = " ");
        tops[i,]  <- split_geno[[1]];
    }
    summ_geno   <- rbind(hist_print, tops);
    percentiles <- c("Final %", strats_fr[1:uni_genome]);
    summ_geno   <- cbind(summ_geno, percentiles);
    rownames(summ_geno) <- NULL
    colnames(summ_geno) <- summ_geno[1,];
    summ_geno <- summ_geno[-1,];
    
    return(list(genos=summ_geno, fitness = MEAN_FITNESS));
}
