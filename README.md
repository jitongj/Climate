## System Requirements
- **Software Dependencies**:  
  The software requires R version 4.3 or higher. Ensure that the following R packages are installed:
  | Package           | Version       |
  |-------------------|---------------|
  | gdata             | 2.18.0        |
  | plyr              | 1.8.8         |
  | countrycode       | 1.4.0         |
  | doBy              | 4.6.12        |
  | reshape2          | 1.4.4         |
  | wpp2024           | 1.0.0         |
  | bayesTFR          | 7.4-1         |
  | bayesLife         | 4.0-3         |
  | bayesPop          | 7.0-0         |
  | popReconstruct    | 1.3-0         |
  | rjags             | 4-12          |
  | coda              | 0.19-4        |
  | xtable            | 1.8-4         |
  | ggplot2           | 3.4.0         |
  | gridExtra         | 2.3           |
  | scales            | 1.2.1         |
  | MASS              | 7.3-58.3      |
  | car               | 3.1-1         |
  | readxl            | 1.4.1         |
  | rworldmap         | 1.3-6         |
  | pracma            | 2.3.8         |

  **Note**: Versions are accurate as of February 5, 2025. It's recommended to check for the latest versions before installation.

- **Tested Versions**:  
  This software has been tested on R version 4.3.3.

- **Hardware Requirements**:  
  No special hardware is required to run the software. It can be executed on any standard desktop or laptop computer.

## Installation Guide
1. Download and install R (version 4.3 or higher) from [CRAN](https://cran.r-project.org/).
2. Install the required R packages by running the following command in R:
   ```R
   install.packages(c(
       "gdata", "plyr", "countrycode", "doBy", "reshape2", 
       "wpp2024", "bayesTFR", "bayesLife", "bayesPop", "popReconstruct", 
       "rjags", "coda", "xtable", "ggplot2", "gridExtra", 
       "scales", "MASS", "car", "readxl", "rworldmap", "pracma"
   ))
   
## Demo
1. **Instructions to Run on Data**:
   - Step 1: Run the script `CO2_Projection_wpp2024_updated_to_2024.R` to generate three data files:
     - `poppreds_formatted_2024_2100`
     - `model_results_ar1const_2024`
     - `proj_evals_ar1const_2024`
     These data files will be used in Step 2.
   - Step 2: Depending on the scenario, run one of the following scripts:
     - For NDC-1 (Nationally Determined Contributions First Version):  
       Run `paris_cleanedup_wpp2024_updated_to_2024.R`
     - For NDC-2 (Nationally Determined Contributions Latest Version):  
       Run `paris_cleanedup_wpp2024_updated_to_2024_newndc.R`
   - These scripts will produce the figures and tables included in the manuscript.

2. **Expected Output**:
   - The scripts in Step 2 will output the figures and tables as presented in the paper.

3. **Expected Run Time for Demo on a "Normal" Desktop Computer**:
   - Running `CO2_Projection_wpp2024_updated_to_2024.R` typically takes 10 hours.
   - Running either `paris_cleanedup_wpp2024_updated_to_2024.R` or `paris_cleanedup_wpp2024_updated_to_2024_newndc.R` takes approximately 6 hours each, depending on system performance.

