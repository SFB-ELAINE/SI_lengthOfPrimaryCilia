This directory contains all files of manual detection of the cilia.

- We have marked the lowest ("z_lower") as well as highest ("z_upper") layer where some pixels of a cilium could be identified.
- We have also measured the horizontal length (longest extension) of the cilia from z-projections (max intensity method).

- Kai and Nadja have both worked with the same set of cilia markings (therefore they identified the same cilia).
- Clemens and Simone have both worked with the same but different set of cilia markings (therefore they identified the same cilia).

- I (Kai) have added a column with zstack layers to Clemens' results because I couldn't verify his results. (Clemens might do another check.)

This directory:
- contains the result of "CombineManualResults.R" -> "df_manual_results_de.csv"/"df_manual_results_en.csv"
- contains "df_metadata_de.csv"/"df_metadata_en.csv" contains the metadata from the "readCzi" execution

Directory "originalFiles":
- contains the original xlsx files which are converted to csv files and moved of directory up.
- "cilia_numbers_clemens_kai.xlsx" contains the numbers of cilia in both readings (Kai/Nadja and Clemens/Simone)

Directory "originalFiles_csv":
- contains all files from "originalFiles" converted to csv files
