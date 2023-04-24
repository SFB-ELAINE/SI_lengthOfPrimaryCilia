# README of SI_lengthOfPrimaryCilia

README of the supporting information of the publication "detectCilia: An R package for automated detection and 3D-measurement of primary cilia - Studying the influence of cultivation methods on the lengths of primary cilia".

# Reproducing all figures from the publication

Run the script `main.R` to reproduce all figures from the publication. Please go through the script step by step as there are some steps that need manual work in other tools. However, all manually derived data are provided such that all plots can be reproduced.

The resulting data are found in "data/automaticDetection/" and "data/manualDetection/". Both directories contain the subfolders "cultivation/" and "resolution/"


## R files

* `ACDC_plotAutomaticDetection.R` -> Contains a function for plotting the ACDC results of the cultivation images (z stack projection).
* `ACDC_readXLSX.R` -> Contains a function for converting the ACDC result (Microsoft Excel file) to a csv file.
* `automaticCiliaDetection.R` -> Contains a function for running the detectCilia package and save the metadata.
* `ciliaQ_getResults.R` -> Contains a function for combining and converting the ciliaQ results (text files) to a csv file.
* `ciliaQ_plotAutomaticDetection.R` -> Contains a function for plotting the ciliaQ results of the cultivation images (z stack projection).
* `combineDetectCiliaResults.R` -> Contains a function for combining the results of detectCilia that is saved in each subfolder.
* `combineManualDetectionResults.R` -> Contains a function for combining all results from manual detection that were converted to csv.
* `convertColorLayersForACDC.R` -> Contains a function for calculating the z stack projection and converting color layers for ACDC detection.
* `convertXLSXtoCSV.R` -> Contains a function for converting Microsoft Excel files to csv files.
* `numberOfNuclei.R` -> Contains a function for printing the number of images and detected nuclei per group.
* `plotAutomaticDetection_Cultivation.R` -> Contains a function for plotting the detectCilia results of the cultivation images.
* `plotComparisonManualAutomaticDetection_Cultivation.R` -> Contains a function for plotting the detectCilia and manual results of the 7 test images.
* `plotComparisonManualAutomaticDetection_Resolution.R`  -> Contains a function for plotting the detectCilia and manual results of the resolution images.
* `plotResultsFromAllTools.R`  -> Contains a function for plotting the detectCilia, ACDC, and ciliaQ results of the cultivation images (horizontal length).
* `plotTestImageResultsFromAllTools.R`  -> Contains a function for plotting the detectCilia, ACDC, ciliaQ, and manual results of the 7 cultivation test images (horizontal length).


## Data

The following data can be found in the directory `data/`.

* `automaticDetection/` -> 
* `manualDetection/` -> 
* `originalData/` -> 

# Steps that are (not) included in main.R


## 1 Manual cilia detection

Please read the manual in the appendix of the corresponding publication for information on manually extracting the number of cilia in a microscopy image as well as determining their lengths.
The following images were manually labelled:

Cultivation images:
* "190815_EV38_2_Collagen_ITSwithAsc+Dexa_63x_zstack_1.czi" to "... _7.czi"

Resolution images
* "210301_EV38_1_Collagen_ITSwithAsc+Dexa+IGF+TGF_63x_z-stack_1_1024x1024.czi" and "..._1_2048x2048.czi"
* "210301_EV38_1_Collagen_ITSwithAsc+Dexa+IGF+TGF_63x_z-stack_2_1024x1024.czi" and "..._2_2048x2048.czi"
* "210301_EV38_1_Collagen_ITSwithAsc+Dexa+IGF+TGF_63x_z-stack_3_1024x1024.czi" and "..._3_4096x4096.czi"
* "210301_EV38_1_Collagen_ITSwithAsc+Dexa+IGF+TGF_63x_z-stack_4_1024x1024.czi", "..._4_2048x2048.czi", and "..._4_4096x4096.czi"
* "210301_EV38_1_Collagen_ITSwithAsc+Dexa+IGF+TGF_100x_z-stack_5_1024x1024.czi"



## 2 Automatic cilia detection

### 2.1 Automatic detection of cilia from resolution images
The function `automaticCiliaDetection()` is used to automatically detect and measure cilia in the resolution images (all czi files saved in the particular directory).

### 2.2 Combine results of automatic detection from resolution images
The function `combineDetectCiliaResults()` takes all results (csv files) from the automatic detection and combines them into three csv files: 1) cilia information ("summary_cilia.csv"), 2) number of nuclei ("summary_nuclei"), 3) used parameter values ("summary_parameters.csv").

### 2.3 Convert original results from manual detection of resolution images to csv
The function `convertXLSXtoCSV()` converts all resolution data stored in xlsx files into csv files.

### 2.4 Combine results of manual detection from resolution images
The function `combineManualDetectionResults()` is used to combine all results from manual detection of cilia in the resolution images into one csv file.

### 2.5 Automatic detection of cilia from cultivation images
The function `automaticCiliaDetection()` is used to automatically detect and measure cilia in the cultivation images (all czi files saved in the particular directory).

### 2.6 Combine results of automatic detection from cultivation images
The function `combineDetectCiliaResults.R` takes all results (csv files) from the automatic detection and combines them into three csv files: 1) cilia information ("summary_cilia.csv"), 2) number of nuclei ("summary_nuclei"), 3) used parameter values ("summary_parameters.csv").

### 2.7 Print the number of detected nuclei
The function `numberOfNuclei()` print a table with the number of images and nuclei in total in every cultivation group.

### 2.8 Convert original results from manual detection of cultivation images to csv
The function `convertXLSXtoCSV()` converts all cultivation data stored in xlsx files into csv files.

### 2.9 Combine results of manual detection from cultivation images
The function `combineManualDetectionResults()` is used to combine all results from manual detection of cilia in the cultivation images into one csv file.



## 3 Plot results

### 3.1 Plot detectCilia results of cultivation images
The function `plotAutomaticDetection_Cultivation()` plots the results obtained by detectCilia of the culitavtion images.

### 3.2 Plot comparison of automated and manual detection of cilia in cultivation test images
The function `plotComparisonManualAutomaticDetection_Cultivation()` plots the results from automated and manual cilia detection and measurements.

### 3.3 Plot comparison of automated and manual detection of cilia in resolution images
The function `plotComparisonManualAutomaticDetection_Resolution()` plots the results from automated and manual cilia detection and measurements.



## 4 Detection of cilia with ACDC

### 4.1 Stack and convert color layers
The function `convertColorLayersForACDC()` stacks converts all czi images into tifs and changes color layers. This is needed because ACDC works with the green layer for cilia instead of the red one.

### 4.2 Analyze the stacked images with ACDC
Please read the Methods section of the corresponding publication for further information. For this step, the tool ACDC is required (the original authors may provide it).

### 4.3 Read and convert ACDC results
The function `ACDC_readXLSX()` reads the results (Excel file) obtained by ACDC and saves it as a csv file.

### 4.4 Plot ACDC results
The function `ACDC_plotAutomaticDetection()` plots the ACDC results.



## 5 Detection of cilia with ciliaQ

### 5.1 Analyze the stacked images with ciliaQ
Please read the Methods section of the corresponding publication for further information. For this step, the tool ciliaQ is required (see https://github.com/hansenjn/CiliaQ).

### 5.2 Read and convert ciliaQ results
The function `ciliaQ_getResults()` reads the results (text files) obtained by ciliaQ, combines them, and saves it as a csv file.

### 5.3 Plot ciliaQ results
The function `ciliaQ_plotAutomaticDetection()` plots the ciliaQ results.



## 6 Comparison of detectCilia, ACDC, and ciliaQ (and manual)

### 6.1 Plot results of horizontal cilia lenght of detectCilia, ACDC, and ciliaQ
The function `plotResultsFromAllTools()`plots results of the cultivation images from detectCilia, ACDC, and ciliaQ.

# 6.2 Plot results of horizontal cilia length of detectCilia, ACDC, and ciliaQ of 7 test images only ####
The function `plotTestImageResultsFromAllTools()`plots results (horizontal length) of the 7 cultivation test images from the manual measurements as well as from detectCilia, ACDC, and ciliaQ.