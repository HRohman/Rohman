Figure 3:
The script "Figure3.R" compiles Figure 3. It runs in R version 3.5.1 and requires the following R libraries: dplyr, reshape2, tools, ggplot2.
The data for Figure 3 are contained in the folder "Figure3Data". Files in this folder are plain-text, comma-separated. The naming convention is "strain replicate", so "B2.01 01b" contains data for strain B2-01, replicate 01b. Columns in these files are as follows:
ROI: Region of Interest
ROI Number: The individual ROI, selected from the image based on thresholding. This typically is a single cellular cluster.
Mean: Mean 8-bit brightness value for pixels within the ROI
Min: Minimum 8-bit brightness value for pixels within the ROI
Max: Maximum 8-bit brightness value for pixels within the ROI
IntDen: The product of the area of the ROI and its mean 8-bit brightness value
RawIntDen: The sum of the brightness values for all pixels in the ROI

Figure 4:
The script "Figure4.R" compiles Figure 4. It runs in R version 3.5.1 and requires the following R libraries: dplyr, reshape2, ggplot2.
The data for Figure 4 are contained in the plain-text, comma-separated file "Figure4Data.csv". Columns in this file are as follows:
Strain: Strain identifier
TechRep: Technical replicate
ClusterID: Unique, arbitrary identifier per cluster
Num.Cells.Progeny: The number of cells observed in a propagule after splitting from parent cluster
Num.Cell.Parent: The number of cells observed in the parent cluster after releasing a propagule (determined at same time-step as Num.Cells.Progeny)
RepTime.sec: The time at which reproduction (propagule splitting from parent cluster) occurred in the time-lapse. Initially this was reported in time-step, which was then converted to seconds.
EntersField.sec: Time at which the cluster entered the field of view

Figure 5:
Data for Figure 5 are contained in the file "Figure5Data.csv". The file format is plain-text, comma-separated. Columns in this file are as follows:
Strain: Strain idenitifier
nopred1: Absorbance reading for technical replicate 1, predator absent
nopred2: Absorbance reading for technical replicate 2, predator absent
nopred3: Absorbance reading for technical replicate 3, predator absent
nopred4: Absorbance reading for technical replicate 4, predator absent
pred1: Absorbance reading for technical replicate 1, predator present
pred2: Absorbance reading for technical replicate 2, predator present
pred3: Absorbance reading for technical replicate 3, predator present
pred4: Absorbance reading for technical replicate 4, predator present
Time(h): Number of hours post-inoculation at which the absorbance readings were made
MeanDiff: Mean difference between predator absent replicates and predator present replicates
