# README of SI_lengthOfPrimaryCilia

README of the supporting information of the publication "detectCilia: An R package for automated detection and 3D-measurement of primary cilia - Studying the influence of cultivation methods on the lengths of primary cilia".

# Reproducing all figures from the publication

To reproduce all figures from the publication, please run the script `main.R`.

The results are found in "data/automaticDetection/" and "data/manualDetection/". Both directories contain the subfolders "cultivation/" and "resolution/"


## R files

* `automaticCiliaDetection.R` -> Function for running the cilia detection package and get the metadata.
* `CalculateStatistics.R`
* `ciliaQ_addNumber.R`
* `ciliaQ_createTestImages.R`
* `ciliaQ_getResults.R`
* `ciliaQ_imageprojection.R`
* `combineDetectCiliaResults.R` -> Function to combine the results of detectCilia that is saved in each subfolder.
* `combineManualDetectionResults` -> Script for combining all results from manual detection
* `convertColorLayersForACDC.R` -> Calculate z stack projection and convert color layers for ACDC detection.
* `convertXLSXtoCSV.R` -> Converts Microsoft Excel files to csv files
* `plotAutomaticDetection_Cultivation.R` -> 
* `PlotComparisonManualAutomaticACDCDetection_Cultivation.R`
* `PlotComparisonManualAutomaticCiliaQDetection_Cultivation.R`
* `PlotComparisonManualAutomaticDetection_Cultivation.R`
* `PlotComparisonManualAutomaticDetection_Resolution.R`
* `PlotManualDetection_Cultivation.R`

Obsolete:
* `CombineManualResults_cultivation.R`
* `CombineManualResults_resolution.R`


# Steps that are (not) included in main.R


## 1 Manual cilia detection

Please read the manual in the appendix of the publication for manually extracting the number of cilia in a microscopy image as well as determining their lengths.


## 2 Automatic cilia detection

### 2.1 Automatic detection of cilia from resolution images
The function `automaticCiliaDetection.R` is used to automatically read the resolution images (all czi files saved in a directory).

### 2.2 Combine results of automatic detection from resolution images
The function `combineDetectCiliaResults.R` takes all results (csv files) from the automatic detection and combines them into three csv files: 1) cilia information ("summary_cilia.csv"), 2) number of nuclei ("summary_nuclei"), 3) used parameter values ("summary_parameters.csv").

### 2.3 Convert original results from manual detection of resolution images to csv
Use the function `convertXLSXtoCSV.R` to convert all data stored in xlsx files into csv files.

### 2.4 Combine results of manual detection from resolution images
The function `combineManualDetectionResults` is used to combine all results from manual detection into one csv file.

### 2.5 Automatic detection of cilia from cultivation images
The function `automaticCiliaDetection.R` is used to automatically read the resolution images (all czi files saved in a directory).

### 2.6 Combine results of automatic detection from cultivation images
The function `combineDetectCiliaResults.R` takes all results (csv files) from the automatic detection and combines them into three csv files: 1) cilia information ("summary_cilia.csv"), 2) number of nuclei ("summary_nuclei"), 3) used parameter values ("summary_parameters.csv").

### 2.7 Convert original results from manual detection of cultivation images to csv
Use the function `convertXLSXtoCSV.R` to convert all data stored in xlsx files into csv files.


## 3 Plot results

### 3.1

The results are:
 1) all_cilia_total_lengths.png/pdf as well as all_cilia_total_lengths_violin_plot.png/pdf (containing all results of the total cilium lengths without manually filtering),
 2) all_filtered_cilia_total_lengths.png/pdf as well as all_filtered_cilia_total_lengths_violin_plot.pdf.png/pdf (containing the filtered results of the total cilium lengths),
 3) the information whether the results are normally distributed using the Shapiro-Wilk test,
 4) all_filtered_cilia_total_lengths_violin_plot_t_test.png/pdf showing whether the differences are significant (using a t test).


# 3 Comparison of manual and automatic cilia detection

## 2.1 ConvertXLSXtoCSV.R and CombineManualResults.R: Reading and saving manual results from Excel files

The results of the manual detection and length measurements of the cilia are saved as xlsx-files and are in "data/manualDetection/190815_AscDexa/originalFiles/".
This folder contains one Excel file per microscopy image and observer as well as a mapping of Clemens' identification of cilia with Kai's ("cilia_numbers_clemens_kai.xlsx"") as well as the mapping of the automatic detection ("cilia_numbers_clemens_automatic.xlsx").
Nadja has used Kai's detected cilia IDs, but measured the cilia lengths independently.

The Excel files are converted to csv files by running the R script "ConvertXLSXtoCSV.R".
The results are saved in the directory "data/manualDetection/190815_AscDexa/originalFiles_csv/".

The manual results are combined to one table (csv file) by running "CombineManualResults.R".
This script needs the metadata of the czi images (TODO: Save function to obtain it).
The resulting table is saved in the directory "data/manualDetection/190815_AscDexa/".

## 2.2 Plotting manual results (in pixels and layers and not um)

The results of the manual detection of the cilia within the seven test images are plotted by "PlotClemensKaiManualDetection.R"."

## 2.3. Plotting comparison of manual and automatic results

Plotting the comparison of the the manual and automatic detection of the seven test images ("190815_EV38_2_Collagen_ITSwithAsc+Dexa_63x_zstack_6..12.czi") is done with the script "PlotManualAutomaticDetectionComparisonResults.R".

