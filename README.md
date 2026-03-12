# README

## Study

```
Assessing the Health Impact of Indoor Air Purification Using Health Data Obtained From Apple Watch Wearers
```

#### Institutional Review Board Approval
New York University Abu Dhabi: HRPP-2024-148

## Code Availability
This repository contains the code used for data analysis and visualization for this study. To run the analysis, the following pre-requisites are required:

- [R 4.4.3](https://www.r-project.org/)
- [Jupyter Notebook](https://jupyter.org/) (`.ipynb`)
- R kernel for Jupyter (e.g., `IRkernel`). If using VSCode, refer to this [guide](https://www.practicaldatascience.org/notebooks/PDS_not_yet_in_coursera/00_setup_env/jupyter_r_notebooks.html)

All required R packages are pre-specified and automatically installed in [/../util/variables.R](/../util/variables.R)

## Repository Structure
- `notebook/`  
    Contains Jupyter notebooks for data pre-processing, statistical analysis, and visualization generation. Notebooks are numbered to indicate the execution order.

- `R-table/`   
    Will contain temporary cached R tables to improve computational efficiency when running the analysis across different notebooks.

- `raw-data/`  
    Contains the raw datasets of the study. [See data availability statement.](#data-availability)

- `../util/`  
    Contain utility functions shared between different notebooks.

- `visualization/`  
    Will contain visualization outputs generated after running the R code.
    
## Data Availability
The datasets generated and/or analyzed during the current study are available from the corresponding author on reasonable request.

