[![Build Status](https://travis-ci.org/EvgenyPetrovsky/synthezator.svg?branch=master)](https://travis-ci.org/EvgenyPetrovsky/synthezator)
[![codecov](https://codecov.io/gh/EvgenyPetrovsky/synthezator/branch/master/graphs/badge.svg)](https://codecov.io/gh/EvgenyPetrovsky/synthezator)


# synthezator

Generate synthetic datasets

## Installation

Install R first from [cran](https://cran.r-project.org). Please install R into local folder. Due to many files R works slowly when it is installed on network drive.

Enter R envinronment, install additional packages this one

```R
install.packages("devtools")
devtools::install_github(repo = "EvgenyPetrovsky/synthezator")
```

## Execution

As an example of how package works you can use included demo rules and demo sets to generate test data

```R
library(synthezator)
synthezator::processRules(rules_df = demo_rules, lovs_df = demo_sets, count = 100)
```

Function `processRules` returns list of dataframes which can be then stored into files.
