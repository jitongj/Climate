## System Requirements
- **Software Dependencies**:  
  The software requires R version 4.3 or higher. Ensure that the following R packages are installed:
  | Package           | Version       |
  |-------------------|---------------|
  | gdata             | 3.0.0         |
  | plyr              | 1.8.9         |
  | countrycode       | 1.6.0         |
  | doBy              | 4.6.24        |
  | reshape2          | 1.4.4         |
  | wpp2024           | 1.0-1         |
  | bayesTFR          | 7.4-4         |
  | bayesLife         | 5.3-0         |
  | bayesPop          | 10.0-1.9019   |
  | popReconstruct    | 1.0-6         |
  | rjags             | 4-15          |
  | coda              | 0.19-4.1      |
  | xtable            | 1.8-4         |
  | ggplot2           | 3.5.1         |
  | gridExtra         | 2.3           |
  | scales            | 1.3.0         |
  | MASS              | 7.3-61        |
  | car               | 3.1-3         |
  | readxl            | 1.4.3         |
  | rworldmap         | 1.3-8         |
  | pracma            | 2.4.4         |

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
1.  **Download the Dataset and Modify the Working Directory**:
  - Dowlond the `Simulation` folder for this link and put it under the `NatureData` folder:

2. **Instructions to Run on Data**:
   - Step 1: Modify the working direction in the script `CO2_Projection_wpp2024_updated_to_2024.R` in line 31 to the current stored folder and run it to generate three data files:
     - `poppreds_formatted_2024_2100`
     - `model_results_ar1const_2024`
     - `proj_evals_ar1const_2024`
     These data files will be used in Step 2.
   - Step 2: Depending on the scenario, run one of the following scripts:
     - For NDC-1 (Nationally Determined Contributions First Version):  
       Modify the working direction in the script `paris_cleanedup_wpp2024_updated_to_2024.R` in line 33  and run it.
     - For NDC-2 (Nationally Determined Contributions Latest Version):  
       Modify the working direction in the script `paris_cleanedup_wpp2024_updated_to_2024_newndc.R` in line 33  and run it.
     Figure 4 will be generated in this step.
   - Step 3: Run the script `plot_figure.R` to generate Figure 1, Figure 2, and Figure 3 featured in the paper.
     Additionally, run the script `increase_ndc.R` to produce the data for Table 1.

3. **Expected Output**:
   - The scripts in Step 2 and Step 3 will output the figures and tables as presented in the paper.

4. **Expected Run Time for Demo on a "Normal" Desktop Computer**:
   - Running `CO2_Projection_wpp2024_updated_to_2024.R` typically takes 10 hours.
   - Running either `paris_cleanedup_wpp2024_updated_to_2024.R` or `paris_cleanedup_wpp2024_updated_to_2024_newndc.R` takes approximately 6 hours each, depending on system performance.

5. **A Small Simulation to Demo the Software**:

## Instructions for Use
1. **How to Run the Software on Your Own Data**:
   - If you need to update the input data, you must modify the file `data_medium_wpp2024_updated2024` according to the original format.
   - Ensure that the structure, column names, and data types in the updated file match those in the original file to maintain compatibility with the scripts.
   - Once the data is updated, follow the steps described in the **Demo** section to rerun the scripts.
2. **Reproduction Instructions**：
