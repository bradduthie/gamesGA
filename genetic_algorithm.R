rm(list=ls(all=TRUE));


agents <- NULL;

history_vec <- c(0,0,0,0,0,1,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,1,1,1);
history     <- matrix(data = history_vec, ncol = 3, byrow = TRUE);

for(i in 1:100){
    agents[[i]] <- rbinom(n=9, size=1, prob=0.5);
}


crossover <- function(agents, prob = 0.1){
    for(i in 1:length(agents)){
        cross <- runif(n=9);
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

mutation <- function(agents, prob = 0.01){
    for(i in 1:length(agents)){
        mutate <- runif(n=9);
        if(min(mutate) < prob){
            change <- which(mutate < prob); 
            newmut <- rbinom(n=length(change), size=1, prob=0.5);
            agents[[i]][change] <- newmut;
        }
    }
    return(agents);
}

PD <- function(a1_play, a2_play){
    points <- 0;
    if(a1_play == 0 & a2_play == 0){
        points <- 0;
    }
    if(a1_play == 0 & a2_play == 1){
        points <- 0;
    }
    if(a1_play == 1 & a2_play == 0){
        points <- 20;
    }
    if(a1_play == 1 & a2_play == 1){
        points <- 0;
    }
    return(points);
}



check_fitness <- function(history, agents){
    
    fitness <- NULL;
    
    for(foc in 1:100){
        
        opponents <- sample(x=1:100, size=10, replace=TRUE);
        foc_score <- rep(0, length(opponents));
        
        for(opps in 1:length(opponents)){
            opp     <- opponents[opps];
            agent_1 <- rep(0, 100);
            agent_2 <- rep(0, 100);
            payoff1 <- rep(0, 100);
            payoff2 <- rep(0, 100);
            
            # Special round 1 (not enough history);
            agent_1[1] <- agents[[foc]][9]; # First turn
            agent_2[1] <- agents[[opp]][9];
            payoff1[1] <- PD(agent_1[1], agent_2[1]);
            payoff2[1] <- PD(agent_2[1], agent_1[1]);
            
            # Special round 2 (not enough history);
            resp_1     <- which(history[1:2,3] == agent_2[1]);
            resp_2     <- which(history[1:2,3] == agent_1[1]);
            agent_1[2] <- agents[[foc]][resp_1];
            agent_2[2] <- agents[[opp]][resp_2];
            payoff1[2] <- PD(agent_1[2], agent_2[2]);
            payoff2[2] <- PD(agent_1[2], agent_2[2]);
            
            # Special round 3 (not enough history);
            resp_1     <- which( history[1:4,2] == agent_2[1] & 
                                     history[1:4,3] == agent_2[2]
            );
            resp_2     <- which( history[1:4,2] == agent_1[1] & 
                                     history[1:4,3] == agent_1[2]
            );
            agent_1[3] <- agents[[foc]][resp_1];
            agent_2[3] <- agents[[opp]][resp_2];
            payoff1[3] <- PD(agent_1[3], agent_2[3]);
            payoff2[3] <- PD(agent_1[3], agent_2[3]);
            
            # Remaining rounds (enough history for complete memory);
            for(round in 4:100){
                mem1 <- round - 3;
                mem2 <- round - 2;
                mem3 <- round - 1;
                resp_1     <- which( history[,1] == agent_2[mem1] & 
                                         history[,2] == agent_2[mem2] &
                                         history[,3] == agent_2[mem3]
                );
                resp_2     <- which( history[,1] == agent_1[mem1] & 
                                         history[,2] == agent_1[mem2] &
                                         history[,3] == agent_1[mem3]
                );        
                agent_1[round] <- agents[[foc]][resp_1];
                agent_2[round] <- agents[[opp]][resp_2];
                payoff1[round] <- PD(agent_1[round], agent_2[round]);
                payoff2[round] <- PD(agent_2[round], agent_1[round]);
            }
            
            foc_score[opps] <- sum(payoff1);
        }
        
        fitness[foc] <- sum(foc_score);
    }
    return(fitness);
}



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





mean_fitness <- NULL;
generations  <- 20;

gen <- 0;

while(gen < generations){
    
    agents  <- crossover(agents = agents, prob = 0.01);
    
    agents  <- mutation(agents = agents, prob = 0.01);
    
    fitness <- check_fitness(history = history, agents = agents);
    
    agents  <- tournament(agents = agents, fitness = fitness);
    
    mean_fitness[[gen+1]] <- mean(fitness);
    
    gen <- gen + 1;
    
    print(gen);
}


final_agent_vec <- unlist(agents);
final_agents    <- matrix(data=final_agent_vec, ncol = 9 , byrow = TRUE);



see_rounds <- function(agents, foc_agent, opp_agent){
    agent_1 <- rep(0, 100);
    agent_2 <- rep(0, 100);
    payoff1 <- rep(0, 100);
    payoff2 <- rep(0, 100);
    
    # Special round 1 (not enough history);
    agent_1[1] <- agents[[foc_agent]][9]; # First turn
    agent_2[1] <- agents[[opp_agent]][9];
    payoff1[1] <- PD(agent_1[1], agent_2[1]);
    payoff2[1] <- PD(agent_2[1], agent_1[1]);
    
    # Special round 2 (not enough history);
    resp_1     <- which(history[1:2,3] == agent_2[1]);
    resp_2     <- which(history[1:2,3] == agent_1[1]);
    agent_1[2] <- agents[[foc_agent]][resp_1];
    agent_2[2] <- agents[[opp_agent]][resp_2];
    payoff1[2] <- PD(agent_1[2], agent_2[2]);
    payoff2[2] <- PD(agent_1[2], agent_2[2]);
    
    # Special round 3 (not enough history);
    resp_1     <- which( history[1:4,2] == agent_2[1] & 
                             history[1:4,3] == agent_2[2]
    );
    resp_2     <- which( history[1:4,2] == agent_1[1] & 
                             history[1:4,3] == agent_1[2]
    );
    agent_1[3] <- agents[[foc_agent]][resp_1];
    agent_2[3] <- agents[[opp_agent]][resp_2];
    payoff1[3] <- PD(agent_1[3], agent_2[3]);
    payoff2[3] <- PD(agent_1[3], agent_2[3]);
    
    # Remaining rounds (enough history for complete memory);
    for(round in 4:100){
        mem1 <- round - 3;
        mem2 <- round - 2;
        mem3 <- round - 1;
        resp_1     <- which( history[,1] == agent_2[mem1] & 
                                 history[,2] == agent_2[mem2] &
                                 history[,3] == agent_2[mem3]
        );
        resp_2     <- which( history[,1] == agent_1[mem1] & 
                                 history[,2] == agent_1[mem2] &
                                 history[,3] == agent_1[mem3]
        );        
        agent_1[round] <- agents[[foc_agent]][resp_1];
        agent_2[round] <- agents[[opp_agent]][resp_2];
        payoff1[round] <- PD(agent_1[round], agent_2[round]);
        payoff2[round] <- PD(agent_2[round], agent_1[round]);
    }
    
    round_hist <- data.frame(agent_1, agent_2, payoff1, payoff2);
    return(round_hist);
}




