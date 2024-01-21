
# OCamelot

OCamelot is a general-purpose stock library that uses stock CSV data to perform financial calculations (e.g. moving averages), trading strategies, and backtests. The library also comes with the option of graphing the candlestick data for the CSV alongside moving average lines.

Data values are represented as optionals. Empty data values will yield ```None```. Many date formats are acceptable, but they need to be specified when reading the CSV. 

## Documentation
To view the full documentation for OCamelot using odoc, run the following commands in the terminal
```bash
opam install odoc
make doc
open _build/default/_doc/_html/index.html
```


## Installation

Clone this repo, and install the dependencies with the following commands.

```bash
opam update
opam install base
opam install csv
opam install gnuplot
opam install ptime
opam install timedesc
```


    
## Reading a CSV
To read a CSV, run the following command
```
let csv_data =
  CsvReader.read_csv ~date:"Date" ~open_price:"Open" ~high_price:"High"
    ~low_price:"Low" ~close_price:"Close" ~volume:"Volume" ~date_type:"MM/DD/YYYY" "./data/SPY.csv"
```
where ```./data/SPY.csv``` is the user's input CSV data and ```MM/DD/YYYY`` is the format of the dates in the CSV. More information on date formats can be viewed in the documentation. 
## Demo
To run the demo from the terminal, use the following command:

```bash
dune exec examples/demo.exe
```

The demo prints features regarding the input CSV data (e.g. size, head), performs a trading strategy and backtests it, and prompts the user to input different moving averages which are then displayed on a candlestick graph GUI. 

The current available moving averages are simple moving average, exponential moving average, triangular moving average, weighted moving average, and volume-adjusted moving average.

## Authors

- Kai Gangi (keg93)
- Joseph Abramov (ja653)
- Javohir Abdurazzakov (ja688)

