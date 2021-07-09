
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
- Run an experiment to help choose parameters
```bash
cabal -v0 run main -- experiment --trials 2 --file ./src/TSPLIB95/a280.tsp --seed 45 --max_temp 5 --max_epochs 700000 > output.txt
```
- Plot the results (requires python 3.9.x)
```bash
python plot.py output.txt output.png
```
- With good parameters, solve the tsp
```bash
cabal -v0 run main -- solve --file ./src/TSPLIB95/a280.tsp --seed 45 --max_temp 5 --max_epochs 700000 > output.txt
```
- See help for more info
```bash
cabal -v0 run main -- --help
```