# Overview

This repository stores public data and all codes used for the paper _Spillovers and Training Effects on Mental Health Prescribing for Children_ by Currie, MacLeod, and Ouyang (2023).

# Description

## Repository structure
Before running any programs, there are four folders under this repository:
- `0_raw_data`: store the raw data, which include
    - Drug product characteristics were manually collected and provided in the repo; 
    - Medical school characteristics are publicly available and provided in the repo; 
    - US zip codes are publicly available and provided in the repo; 
    - Prescription data require data use agreements with IQVIA and are not included in this repo.
- `2_results`: store the results of regressions in .tex format.
- `codes`: store all codes used for this paper.
- `notes`: store a .lyx file that assembles all .tex files into one file and store the .pdf file created by the .lyx file.

After start running the programs, a folder `1_data` will be automatcially created. This folder stores all derived datasets.

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