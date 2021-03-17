# INTERPOL Reviews Analysis - Interactive Web Application

## International Forensic Science Managers Symposium References Analysis Tool 

The use of forensic evidence has become indispensable in many countries and jurisdictions around the world, however the dissemination of research advancements does not necessarily directly or easily reach the forensic science community.

The R-Shiny app provides an analysis of references lists available in the INTERPOL reports with an export functionality of searched records. 

## Using the R-Shiny code

This code was developed using Rstudio 1.4.1106, R 4.0.4 and uses Renv for package management.

To get started, open `Shiny.Rproj` in RStudio. This project file will install the Renv package if not already installed. Package dependancies can then be installed by using the following command: `renv::restore()`

Once installed, select the `app.R` file and then choose "Run app".

Imported files can be changed using the files format already provided. The imported data is in a CSV format and, for speed, is entirely contained in the All_Data_Shiny.csv (all corrections observed in the standalone version already applied)