# Overview

This repository stores public data and all codes used for this paper.

# Description

## Data
- The raw data are stored under `0_raw_data`.
- Drug product characteristics were manually collected. Medical school characteristics and US zip codes are publicly available. We provide these datasets in this repository.
- The prescription data require data use agreements with IQVIA and are not included in this repository.

## Computational requirements
- All programs were written in R (version 3.6.0) and run on Kellogg Linux Cluster.

## How to run the programs
- First, set the directory to the root of the repository.
- Then, run
```
Rscript codes/0_run_all.R
```
- All working directories, including `1_data` and `2_results` will be automatically created.
- All working datasets should be derived under `1_data`.
- All tables in the paper should be available under `2_results`.
- Users can use `notes/tables.lyx` to see all tables used in the paper.