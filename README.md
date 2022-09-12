# SI_lengthOfPrimaryCilia
Supporting information to "Influence of cultivation methods on the lengths of primary cilia"



# 1 Manual and automatic cilia detection

## 1.1 Manual cilia detection

Please read the manual in the appendix for manually extracting the number of cilia in a microscopy image as well as determining their lengths.


## 1.2 Automatic cilia detection

### 1.2.1 Detect and measure all cilia automatically

Detect all cilia in all czi files saved in a directory by calling the script "AutomaticCiliaDetection.R".

### 1.2.2 Add results from automatic detecion to csv files

The script "CombineDetectCiliaResults.R" takes all results (csv files) from the automatic detection and combines them into three csv files: 1) cilia information ("summary_cilia.csv"), 2) number of nuclei ("summary_nuclei"), 3) used parameter values ("summary_parameters.csv").



# 2 Comparison of manual and automatic cilia detection

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

