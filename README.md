# Overview

This replication file stores public data and all codes used for the paper _Spillovers and Training Effects on Mental Health Prescribing for Children_ by Currie, MacLeod, and Ouyang (2024).

# Description

## Non-public data
Prescriptions data and data on characteristics of patients, providers, and products were purchased from IQVIA. IQVIA (formerly known as IMSQuintiles) is a public company specializing in pharmaceutical market intelligence. Due to the Data Use Agreement, we cannot provide the raw data in the replication file. Readers who are interested in purchasing the data should contact Allen Campbell at Allen.Campbell@iqvia.com.

Although we do not provide the raw data, the directories where the data should be stored in order to run programs are provided, together with README files that list the names of the raw data used in our analysis.

## Replication file structure
Before running any programs, there are four folders under this repository:
- `0_raw_data`: store the raw data, which include
    - `iqvia`: Prescriptions data and data on characteristics of patients, providers, and products. See the previous subsection for details.
    - `product`: Additional drug product characteristics were manually collected and provided in the replication file.
    - `school`: Medical school characteristics are publicly available and provided in the replication file.
    - `uszips`: US zip codes are publicly available and provided in the replication file.
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