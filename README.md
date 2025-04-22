# Reference-Intervals-Code-and-Data

A Shiny application for visualizing and exporting reference interval data from clinical laboratories. This tool reads Excel files containing biochemistry or hematology reference intervals and generates interactive plots as well as processed data statistics.

## Features

- **Data Preview**: View the first 100 rows of your selected analyte data in a searchable, paginated table.
- **Customizable Plots**: Generate interval plots for Child Male, Child Female, Adult Male, or Adult Female groups. Adjust plot dimensions interactively and download as SVG.

## Repository Structure

```
├── app.R                      # Main Shiny application script
├── Biochemistry Plot Data.xlsx # Sample biochemistry reference intervals
├── Hematology Plot Data.xlsx   # Sample hematology reference intervals
└── README.md                  # This documentation
```

## Prerequisites

- **R** (version ≥ 4.0)
- **RStudio** (optional, for convenience)

## Installation

1. **Clone the repository**:

   ```bash
   git clone https://github.com/KD-1120/Reference-Intervals-Code-and-Data.git
   cd Reference-Intervals-Code-and-Data
   ```

2. **Install required R packages** (as listed in `app.R`) in an R session:

   ```r
   install.packages(c(
     "shiny", "tidyverse", "readxl", "ggtext", "gridExtra",
     "svglite", "extrafont", "DT", "openxlsx", "patchwork", "viridis"
   ))
   # For font support (run once):
   extrafont::font_import(prompt = FALSE)
   extrafont::loadfonts(device = "win")  # or "pdf"/"postscript" as needed
   ```

   ([github.com](https://github.com/KD-1120/Reference-Intervals-Code-and-Data/blob/main/app.R))

## Running the App

From the project root, launch the Shiny app by executing in R:

```r
library(shiny)
shiny::runApp('app.R')
```

Or open `app.R` in RStudio and click **Run App**.

## Using the App

1. **Upload Data**: Click **Choose Data File** and select an Excel file (`.xlsx`, `.xls`, or `.csv`). You can use the provided sample files.
2. **Select Category**: Choose **Biochemistry** or **Hematology**.
3. **Select Analyte**: Pick the analyte (sheet name) to plot.
4. **Units**: Verify or edit the default unit value.
5. **Plot Type**: Switch between Child Male, Child Female, Adult Male, or Adult Female.
6. **Dimensions**: Adjust **height** and **width** sliders.
7. **View & Download**:
   - **Plot** tab: Preview and **Download Plot as SVG**.

## Data Format

- Each analyte must be a separate sheet named exactly as listed in the dropdown menu (e.g., `Albumin`, `WBC`).
- Sheets should have columns: `Analyzer`, `AM_L.L`, `AM_U.L`, `AF_L.L`, `AF_U.L`, `CM_L.L`, `CM_U.L`, `CF_L.L`, `CF_U.L`, `Gender_male`, `Gender_female`, `Age_adult`, `Age_child`, and an optional `Units` column.

## License

This project is licensed under the MIT License.

## Contact

For questions or feedback, please open an issue on GitHub.

