
# SimulatedAnnealing 

SimulatedAnnealing is a Haskell library, and executable, for solving Travelling Salesmen problems with the Simulated Annealing optimisation method.

## Installation

1. Clone the repo
2. Build the package with Cabal
```bash
cabal build
```

## Libraries
- `TSPLIB95` for parsing files in the TSPLIB95 format
- `SimulatedAnnealing.TSP` for solving TSP problems
- `GTSP` provides an interface between the two

## Executable
The executable has two main subcommands.
### Experiment subcommand
Can run the simulation multiple times and returns a history of the energy of the solution found at each step of each run of the solver. The actual solution values (e.g. tours for TSP) are not returned in the output to keep the size of the output manageable.

See help for more info:
```bash
cabal -v0 run main -- --help experiment
```


### Solve subcommand
Is similiar to the experiment subcommand but can only run the simulation once and returns the solution value, as well as it's energy. The output includes "sol", which is the solution at the end of the simulation, and "bestSol" which is the best solution found throughout the whole simulation.

See help for more info:
```bash
cabal -v0 run main -- --help solve
```

## Example usage of executable
- Run an experiment to get a history of the state of the simulation through each trial
```bash
cabal -v0 run main -- experiment --trials 2 --file ./src/TSPLIB95/a280.tsp --seed 45 --max_temp 5 --max_epochs 700000 > output.txt
```
- Plot the results (requires python 3.9.x)
```bash
python plot.py output.txt
```
- Repeat the above two steps, adjusting the parameters (seed, max_temp, max_epochs, etc) to get the best performance.
- Once you have the best parameters, run the simulation again with those parameters, but using the solve subcommand to get the solution value.
```bash
cabal -v0 run main -- solve --file ./src/TSPLIB95/a280.tsp --seed 45 --max_temp 5 --max_epochs 700000 > output.txt
```
