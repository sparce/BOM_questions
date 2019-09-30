# Working with BOM data

Here are some example solutions for the 
[Data School challenge](https://csiro-data-school.github.io/r/15-Reproducibility/index.html#challenge-putting-it-all-together) 
on reproducibility. The challenge asks us to explore weather measurements from several sites across
Australia as a way of practising data manipulation using the tidyverse. All parts of the exercise 
are able to be completed using the functions covered in the [Introduction to R](https://csiro-data-school.github.io/r/)
course notes.

## Cloning the project

To test out these examples, you can clone this repository in RStudio:

  * Create a new RStudio project (File menu > New Project)
  * Select the 'Version Control' option to checkout this existing project
  * From the next menu, select 'Git' as the type of repository
  * Paste in the URL for this repository (`https://github.com/sparce/BOM_questions.git`)
  * Check that you are happy with the project's name and directory, then select 'Create Project'
  
## Structure

After cloning this repository, you should end up with a directory structure similar to this:

```
BOM_questions/
├── README.md
├── BOM_analysis.R
├── data
│   ├── BOM_data.csv
│   └── BOM_stations.csv
└── results
    ├── q1_record_completeness_by_station.csv
    ├── q2_avg_tempdiff_by_month.csv
    ├── q3_avg_tempdiff_by_state.csv
    └── q4_average_solar_exposure_for_furthest_stations.csv
```

The code for all four parts of the challenge can be found in the `BOM_analysis.R` file. Because the 
raw data files are included in the `data` folder, the analysis script should be able to be run 
without any further setup.

Examples of the data frames used to answer the challenge questions have been written out to csv files
in the `results` folder for review. These results are also saved as variables after running the script 
(`q1_ans`, `q2_ans`, `q3_ans`, `q4_ans`), so can be explored interactively if you prefer.

## Notes

 * There is usually more than one way to solve a problem. You might have taken a different approach
 to what I have here.
 * For teaching purposes, this code is more excessively commented than is usual.
 * The raw data files **are** included in the repository for easy reproducibility. This may not be 
 appropriate if you have very large data files or they should not be publicly accessible. 
 * The result files are *also* included in the repository. Because these files can be completely 
 recreated from the analysis script and the data files, it is not usually necessary to include them
 in the repository. I have done so here to allow people to see my output without needing to run the 
 script themselves.