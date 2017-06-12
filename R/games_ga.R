#' 
#' Games genetic algorithm: 
#' 
#' This function runs a genetic algorithm that identifies sequential strategies 
#' for maximising payoffs given any two by two symmetrical payoff matrix. 
#' Simulated players remember three rounds into the past.
#' 
#' @param CC The number of points awarded to a focal agent when the focal agent 
#' and its opponent both cooperate
#' @param CD The number of points awarded to a focal agent when the focal agent
#' cooperates and its opponent defects
#' @param DC The number of points awarded to a focal agent when the focal agent
#' defects and its opponent cooperates
#' @param DD The number of points awarded to a focal agent when the focal agent
#' and its opponent both defect
#' @param callC Whether or not the function calls c in the genetic algorithm. 
#' While not calling c is an option, the default value is TRUE because using the
#' compiled c code greatly speeds up the genetic algorithm, making the whole
#' program run much faster
#' @param generations The number of generations the genetic algorithm will run
#' before returning selected genotypes and fitness history. Each generation 
#' includes some number of rounds of the game that each strategy will play
#' @param rounds The number of rounds of the game that a focal player will play
#' against its opponent before moving on to the next opponent. Rounds are played
#' iteratively against opponents, allowing the history of interactions to affect
#' strategies and therefore total payoffs after all rounds are completed
#' @param num_opponents The number of randomly selected opponents that a focal
#' player will play during the course of one generation; the focal player will
#' play in the same number of rounds with each opponent
#' @param cross_prob A uniform probability of random crossing over event for a 
#' focal player's locus with the same locus from another randomly selected 
#' player.
#' @param mutation_prob The probability that a given locus will mutate; mutation
#' from `C` to `D` occurs with the same probability as `D` to `C` (no bias)
#' @return A list, the elements of which include: 1. A table of the genomes
#' of strategies and their frequencies in the population and 2. The mean
#' fitness calculated over all players population in each generation. Fitness of
#' one player is the number of points accrued over all rounds and opponents in a
#' generation.
#' @examples
#' games_ga(CC = 3, CD = 0, DC = 5, DD = 1, generations = 250, rounds = 100)
#' @useDynLib gamesGA
#' @importFrom stats rnorm rpois rbinom runif
#' @export

# Compile the fitness function with the command below
# R CMD SHLIB -o fitness.so fitness.c
# CC = 3, DC = 4, CD = -1, DD = 0 : usually results in cooperation

games_ga <- function(CC = 3, CD = 0, DC = 5, DD = 1, callC = TRUE, 
                     generations = 250, rounds = 100, num_opponents = 100,
                     cross_prob = 0.05, mutation_prob = 0.05){

    if(num_opponents > 100){
        stop("Number of opponents must be less than or equal to 100");    
    }
    
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
    
        fitness <- fitness(history = history, agents = agents, 
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
    geno_rows <- NULL;
    for(i in 1:dim(summ_geno)[1]){
        geno_rows <- c(geno_rows, paste("Strategy",i));
    }
    rownames(summ_geno) <- geno_rows;
    
    return(list(genos=summ_geno, fitness = MEAN_FITNESS));
}
