# Data

The main data used in this article is [Occurences_Tb_TempPeriods_FINAL.csv](https://github.com/oleon12/Tbrasiliensis_USrange/edit/main/Data/Occurences_Tb_TempPeriods_FINAL.csv); the other file is just a variation. This CSV file contains different columns with the following information:

- **Identification:** The identification of each record, including the GBIF ID number (*ID*) and the species name (*Sp*).
- **Space:** The coordinate columns in decimal format (*Lon*, *Lat*), along with the country (*Country*), and the specific region (East, West, and Middle U.S.) where each point overlaps (*Region*).
- **Time:** The date when the record was taken. This includes *Date*, *Year*, *Month*, and *Day*. Likewise, it includes the time period or decade of each point (*cat*), which is also represented by a sequence of numbers (*cat2*).
- **Temperature:** All temperature variables used in the study. The average annual temperature for each year (*TempAvgY2*), the average temperature of the warmest months (*Temp.AvgW2*), and the coldest months each year (*Temp.AvgC2*). Furthermore, the same information extracted for each pixel given the coordinates of each point (*TempAvgY*, *TempAvgW*, and *TempAvgC*).

</br>

---

<p align="center">
  <img src="Table_Head.png" alt="Data head" width="1500">
</p>

---

*Additional columns not noted above can be deprecated. They were created during the pre-analysis phase and were never used in the final version.*

---
Additionally, the results from the centroid analysis are included here. [Centroids_Data.csv](Centroids_Data.csv) corresponds to the normalized distance between two consecutive centroids. These values are evaluated for each region (East, West, and Middle U.S.), while the U.S.-wide results are in [Centroids_Data_US.csv](Centroids_Data_US.csv). We also include the coordinates for each period of time in the files ending with [_Coords.csv](Centroids_Data_Coords.csv) and [_CoordsUS.csv](Centroids_Data_CoordsUS.csv).

<br>

## WAV files

In this [folder](https://github.com/oleon12/Tbrasiliensis_USrange/tree/main/Data/WAV)  you'll find the WAV audio files used to create the Figure 2.
