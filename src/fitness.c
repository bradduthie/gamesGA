#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>


/* =============================================================================
 * Payoff function
 * ===========================================================================*/
int PD(int a1_play, int a2_play, int pay1, int pay2, int pay3, int pay4){
  int points;
  
  points = 0;
  
  if(a1_play == 0 && a2_play == 0){
      points = pay1;
  }
  if(a1_play == 0 && a2_play == 1){
      points = pay2;
  }
  if(a1_play == 1 && a2_play == 0){
      points = pay3;
  }
  if(a1_play == 1 && a2_play == 1){
      points = pay4;
  }
  return points;
}

/* =============================================================================
 * FITNESS FUNCTION:
 * ===========================================================================*/

/* =============================================================================
 *  This function replicates the algorithm in fitness.R, finding the fitness of
 *  agents based on payoffs of iterated games in which two options are available
 *  and the payoff marix is symmetric.
 *  Inputs include:
 *      HISTORY:    An array of the possible histories of opponent's play
 *      AGENTS:     An array of agents (binary, determining genotypes)
 *      PARAMETERS: Parameters needed: number of opponents, rounds of play
 * ===========================================================================*/
SEXP fitness(SEXP HISTORY, SEXP AGENTS, SEXP PARAMETERS){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    double *history_ptr;       /* Pointer to HISTORY (interface R and C) */
    double *agent_ptr;         /* Pointer to AGENTS (interface R and C) */
    double *paras_ptr;         /* Pointer to PARAMETERS (interface R and C) */
    int *dim_HISTORY;          /* Dimensions of the HISTORY array incoming */
    int *dim_AGENTS;           /* Dimensions of the AGENTS array incoming */
    int vec_pos;               /* Vector position for making arrays */
    double *paras;             /* Pointer to PARAMETER (interface R and C) */
    double **agents;           /* Array of agents */
    double **history;          /* Array of history */
    int foc;                   /* Index for a focal agent */
    int locus;                 /* Index for the locus of an agent */
    int i, j;                  /* Indices */
    int protected_n;           /* Number of protected R objects */
    int hist_number;           /* Number of rows in the history array */
    int hist_option;           /* number of columns in the history array */
    int agent_number;          /* Total number of agents in the agent array */
    int loci_number;           /* Total number of loci in each agent */
    double *fitness_ptr;       /* Pointer to the fitnesses of agents */
    double *fitness;           /* Fitnesses of agents */
    double *agent_1;           /* Temporary vector for agent 1 strategy */
    double *agent_2;           /* Temporary vector for agent 2 strategy */
    double *payoff1;           /* Temporary vector for agent 1 payoffs  */
    double *payoff2;           /* Temporary vector for agent 2 payoffs  */
    int num_opponents;         /* Number of opponents each agent plays  */
    int opponent;              /* Index for how many opponents encountered   */
    int rounds;                /* Number of rounds that are played per opp */
    int round;                 /* Index for the round being played */
    int pay1;                  /* Payoff for the upper-left matrix (focal) */
    int pay2;                  /* Payoff for the upper-right matrix (focal) */
    int pay3;                  /* Payoff for the lower-left matrix (focal) */
    int pay4;                  /* Payoff for the lower-right matrix (focal) */
    int opp;                   /* Index for the opponent of the focal   */
    int resp_1;                /* Response of the focal agent (choice) */
    int resp_2;                /* Resonponse of the opponent (choice) */
    int mem1;                  /* Round in the furthest memory (3 rounds ago) */
    int mem2;                  /* Round in 2nd furthest memory (2 rounds ago) */
    int mem3;                  /* Round in most recent memory (1 round ago) */
    
    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;

    PROTECT( HISTORY = AS_NUMERIC(HISTORY) );
    protected_n++;
    history_ptr = REAL(HISTORY);
    
    PROTECT( AGENTS = AS_NUMERIC(AGENTS) );
    protected_n++;
    agent_ptr = REAL(AGENTS);
    
    PROTECT( PARAMETERS = AS_NUMERIC(PARAMETERS) );
    protected_n++;
    paras_ptr = REAL(PARAMETERS);
    
    dim_HISTORY   = INTEGER( GET_DIM(HISTORY)  );
    dim_AGENTS    = INTEGER( GET_DIM(AGENTS) );

    /* The C code for the model itself falls under here */
    /* ====================================================================== */
    
    /* Code below remakes the HISTORY matrix for easier use */
    hist_number  = dim_HISTORY[0];
    hist_option  = dim_HISTORY[1];
    history   = malloc(hist_number * sizeof(double *));
    for(i = 0; i < hist_number; i++){
        history[i] = malloc(hist_number * sizeof(double));   
    } 
    vec_pos = 0;
    for(j = 0; j < hist_option; j++){
        for(i = 0; i < hist_number; i++){
            history[i][j] = history_ptr[vec_pos];
            vec_pos++;
        }
    } /* AGENT is now stored as 'agents' */
    
    /* Code below remakes the AGENTS matrix for easier use */
    agent_number = dim_AGENTS[0];
    loci_number  = dim_AGENTS[1];
    agents   = malloc(agent_number * sizeof(double *));
    for(foc = 0; foc < agent_number; foc++){
        agents[foc] = malloc(loci_number * sizeof(double));  
    } 
    vec_pos = 0;
    for(locus = 0; locus < loci_number; locus++){
        for(foc = 0; foc < agent_number; foc++){
            agents[foc][locus] = agent_ptr[vec_pos];
            vec_pos++;
        }
    } /* AGENT is now stored as 'agents' */
    
    /* Allocate memory for the fitness vector */
    fitness = malloc(agent_number * sizeof(double));

    /* Do the fitness game here now */
    /* ====================================================================== */
    
    num_opponents = (int) paras_ptr[0];
    rounds        = (int) paras_ptr[1];
    pay1          = (int) paras_ptr[2];
    pay2          = (int) paras_ptr[3];
    pay3          = (int) paras_ptr[4];
    pay4          = (int) paras_ptr[5];
    
    for(foc = 0; foc < agent_number; foc++){
        fitness[foc] = 0.0;
        opponent     = num_opponents;
        while(opponent > 0){
            do{
                opp = floor( runif(0, 1) * agent_number);
            } while(opp == agent_number);
            
            agent_1  = malloc(rounds * sizeof(double));
            agent_2  = malloc(rounds * sizeof(double));
            payoff1  = malloc(rounds * sizeof(double));
            payoff2  = malloc(rounds * sizeof(double));
            
            /* Special round 1 (not enough history) */
            agent_1[0] = (int) agents[foc][8];
            agent_2[0] = (int) agents[opp][8];
            payoff1[0] = PD(agent_1[0], agent_2[0], pay1, pay2, pay3, pay4);
            payoff2[0] = PD(agent_2[0], agent_1[0], pay1, pay2, pay3, pay4);
            
            fitness[foc] += (double) payoff1[0];
              
            /* Special round 2 (not enough history) */
            resp_1 = (int) history[0][2];
            if(agent_2[0] == 1){
              resp_1 = (int) history[1][2];
            }
            resp_2 = (int) history[0][2];
            if(agent_1[0] == 1){
              resp_2 = (int) history[1][2];
            }            
            agent_1[1] = (int) agents[foc][resp_1];
            agent_2[1] = (int) agents[opp][resp_2];
            payoff1[1] = PD(agent_1[1], agent_2[1], pay1, pay2, pay3, pay4);
            payoff2[1] = PD(agent_2[1], agent_1[1], pay1, pay2, pay3, pay4);
            
            fitness[foc] += (double) payoff1[1];
            
            /* Special round 3 (not enough history) */
            resp_1 = 0;
            while(agent_2[0] != (int) history[resp_1][1] && 
                  agent_2[1] != (int) history[resp_1][2]
            ){
              resp_1++;
            }
            resp_2 = 0;
            while(agent_1[0] != (int) history[resp_2][1] && 
                  agent_1[1] != (int) history[resp_2][2]
            ){
              resp_2++;
            }            
            agent_1[2] = (int) agents[foc][resp_1];
            agent_2[2] = (int) agents[opp][resp_2];
            payoff1[2] = PD(agent_1[2], agent_2[2], pay1, pay2, pay3, pay4);
            payoff2[2] = PD(agent_2[2], agent_1[2], pay1, pay2, pay3, pay4);
            
            fitness[foc] += (double) payoff1[2];
            
            /* Remaining rounds (enough history for complete memory) */
            for(round = 3; round < rounds; round++){
                mem1 = round - 3;
                mem2 = round - 2;
                mem3 = round - 1;
                resp_1 = 0;
                while(agent_2[mem1] != (int) history[resp_1][0] &&
                      agent_2[mem2] != (int) history[resp_1][1] &&
                      agent_2[mem3] != (int) history[resp_1][2]
                ){
                  resp_1++;
                }
                resp_2 = 0;
                while(agent_1[mem1] != (int) history[resp_2][0] &&
                      agent_1[mem2] != (int) history[resp_2][1] &&
                      agent_1[mem3] != (int) history[resp_2][2]
                ){
                  resp_2++;
                }
                agent_1[round] = (int) agents[foc][resp_1];
                agent_2[round] = (int) agents[opp][resp_2];
                payoff1[round] = PD(agent_1[round], agent_2[round],
                                    pay1, pay2, pay3, pay4);
                payoff2[round] = PD(agent_2[round], agent_1[round],
                                    pay1, pay2, pay3, pay4);
                
                fitness[foc] += (double) payoff1[round];
            }
            
            /* Free memory and move on to the next opponent */
            free(agent_1);
            free(agent_2);
            free(payoff1);
            free(payoff2);

            opponent--;
        }
    }
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP FITNESS;
    PROTECT( FITNESS = allocVector(REALSXP, agent_number) );
    protected_n++;
    
    fitness_ptr = REAL(FITNESS);

    vec_pos = 0;
    for(foc=0; foc<agent_number; foc++){
        fitness_ptr[vec_pos] = fitness[vec_pos];
        vec_pos++;
    }
        
    UNPROTECT(protected_n);
    
    /* Free all of the allocated memory used in arrays */
    free(fitness);
    for(foc = 0; foc < agent_number; foc++){
        free(agents[foc]);
    }
    free(agents);
    for(i = 0; i < hist_number; i++){
        free(history[i]);        
    }
    free(history);

    return(FITNESS); 
}
/* ===========================================================================*/
          
