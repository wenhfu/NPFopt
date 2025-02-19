# README

## Overview

This repository contains MATLAB scripts used to generate numerical results for comparison in a research paper. These scripts are designed to compare the performance of the NPFopt  algorithm with the Lancelot algorithm in solving equality/inequality constrained optimization problems. The scripts are not intended for general user use but are specifically tailored to produce the numerical results presented in the paper.

## File Structure

- `Compare_NPFopt.m`: The main script used to compare the performance of the NPFopt   algorithm and Lancelot under different constraint conditions.
- `All_Problems/`: A folder containing test problems, divided into equality constraints (`Eq`), inequality constraints (`IneqGe` and `IneqLe`).
- `All_Problems_Lan/`: A folder containing the test results for the Lancelot algorithm.
- `output_detailed.txt`: A detailed comparison output file.
- `output_all.txt`: A comparison output file for all problems, output in $\LaTeX$ table format.
- `output_successful.txt`: A comparison output file for successfully solved problems, output in $\LaTeX$ table format.
- `output_failed.txt`: A comparison output file for failed problems, output in $\LaTeX$ table format.
- `Fig.eps`: The generated performance comparison plot.

## Usage

1. **Run the Main Script**: Execute the `Compare_NPFopt.m` script to generate the comparison results. The script will automatically load the test problems, run the NPFopt and Lancelot algorithms, and produce the comparison data and plots.

2. **Check Output Files**: After running the script, the generated comparison results will be saved in `output_detailed.txt`, `output_all.txt`, `output_successful.txt`, and `output_failed.txt`.

3. **View the Plot**: The generated performance comparison plot will be saved as `Fig.eps`.

## Notes

- The scripts in this repository are designed specifically to generate numerical results for the paper and are not intended for general use.
- Before running the scripts, ensure that the CUTEst test set and LANCELOT are installed, and all dependencies are properly configured.

## Dependencies

- MATLAB
- CUTEst test set
- LANCELOT

## Author

- **Wenhao Fu**: Code development and maintenance.
- Last Updated: February 19, 2025
