# gamesGA

A genetic algorithm for finding optimal strategies in sequential games characterised by two by two symmetric payoff matrices.

## Installation and use

To install this package, make sure that the `devtools` library is installed.

```
install.packages("devtools")
library(devtools)
```

Use `install_github` to install using `devtools`.

```
install_github("bradduthie/gamesGA")
```

**Example usage:**

To run the genetic algorithm and find strategies that evolve in a classical [Prisoner's dilemma](https://en.wikipedia.org/wiki/Prisoner's_dilemma) scenario, run the following:

```
games_ga(CC = 3, DC = 4, CD = -1, DD = 0)
```

For further assistance, contact brad.duthie@gmail.com.