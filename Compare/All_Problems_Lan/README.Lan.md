# MATLAB Script for Generating LANCELOT Solver Results

## Overview

This MATLAB script is designed to automate the process of running the LANCELOT solver on a set of optimization problems and collecting the results. The script generates data files (`Lancelot_data_*.mat`) that contain the solver's output, which is used for analysis in our research paper. This script is not intended for general user distribution but is specifically tailored for generating the data presented in our paper.

## Purpose

The primary purpose of this script is to:

1. **Automate the Execution of LANCELOT**: The script runs the LANCELOT solver on multiple optimization problems stored in specific directories.
2. **Collect and Save Results**: The results from each problem are extracted from the solver's output files and saved in MATLAB data files (`Lancelot_data_*.mat`).
3. **Facilitate Research Analysis**: The generated data files are used for further analysis and to produce the results discussed in our research paper.

## Directory Structure

The script assumes the following directory structure:

- **All_Problems**: Contains subdirectories (`Eq`, `IneqGe`, `IneqLe`), each holding a set of optimization problems with their corresponding `*.SIF` files.
- **CUTE/lancelot/bin/sdlan**: Contains the LANCELOT solver executable.

## Script Workflow

1. **Initialization**: The script sets up the necessary paths and options for reading the solver's output.
2. **Problem Execution**:
   - The script iterates over each problem in the `Eq`, `IneqGe`, and `IneqLe` directories.
   - For each problem, it creates a directory, runs the LANCELOT solver, and reads the output from the `SUMMARY.d` file.
3. **Data Collection**:
   - The script extracts key metrics such as the number of iterations (`n`), function evaluations (`Nf`), gradient evaluations (`Ng`), and solver flags (`Flag`).
   - It also captures the objective type, value, and name.
4. **Data Saving**: The collected data is saved in MATLAB `.mat` files (`Lancelot_data_Eq.mat`, `Lancelot_data_IneqGe.mat`, `Lancelot_data_IneqLe.mat`).

## Generated Data Files

The script generates the following data files:

- **Lancelot_data_Eq.mat**: Contains results for equality-constrained problems.
- **Lancelot_data_IneqGe.mat**: Contains results for inequality-constrained problems with $\ge0$ constraints.
- **Lancelot_data_IneqLe.mat**: Contains results for inequality-constrained problems with $\le0$ constraints.

Each `.mat` file contains two variables:

- **Nit**: A matrix containing numerical results (iterations, function evaluations, etc.).
- **Nit_flag**: A cell array containing the objective type, value, and name.

## Usage

1. **Set Up Directories**: Ensure that the directories (`All_Problems`, `CUTE/lancelot/bin/sdlan`) are correctly set up and contain the necessary files.
2. **Run the Script**: Execute the script in MATLAB. The script will automatically run the LANCELOT solver on all problems and save the results in the appropriate `.mat` files.
3. **Analyze Results**: Use the generated `.mat` files for further analysis as needed.

## Important Notes

- **Not for General Use**: This script is specifically designed for generating data for our research paper and is not intended for general user distribution.
- **Dependencies**: The script assumes that the LANCELOT solver and the necessary SIF files are correctly installed and accessible.
- **Customization**: If you need to adapt this script for other purposes, ensure that the directory structure and file paths are correctly configured.

## Contact

For any questions or issues related to this script, please contact the authors of the research paper.
