# Analysis scripts for paper "COVID-induced low power demand and market forces starkly reduce CO2 emissions"

## Introduction

* This repository contains all code to generate the figures of the main paper and SI.
* This code comes with a Two-clause BSD licence (BSD-2) which can be found in the file "LICENCE". 
* To cite the paper, please use the citation as suggested in the file "CITATION", and to specifically refer to this code, please use the doi referring to the linked zenodo entry.

## Preparation

* In order to run the scripts and replicate the graphs, additional data needs to be copied into the folders ENTSO-E_data and other_data (by default, a reduced set of analysis is performed, not requiring the ENTSO-E data).
* Read-me files in both folders contain specific information on file names and sources.
* All data is publicly available (last retrieval January 05, 2021)
* Please note that download of ENTSO-E data via SFTP requires first registering with transparency.entso-e.eu , and requesting a private key via email, which might take up to a few days
* The code uses the following R packages: tidyverse, data.table, readxl, and zoo

## Running the code

* After completing the necessary preparation, you can execute the code to generate the figures by running 
```
Rscript 01_main.R
```
from the command line, or by opening an R session and running
```
source("01_main.R")
```
* By default, this only runs the reduced set of analysis not requiring the ENTSO-E data. In order to run the full analysis, set `full_analysis <- T` in line 17 of file 01_main.R and execute this file.