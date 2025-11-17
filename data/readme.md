# Data folder

Following UChicago's guidelines, data should be stored in Box, not in GitHub. However, this folder should have the same structure as the Box folder, saving metadata such as data dictionaries and codebooks instead of the actual data tables. This can be done in Stata using `iesave` and in R using `skimr`.

## Suggested folder structure


<table>
<tr>
<th align="center">
<img width="441" height="1px">
<p> 
<small>
Option 1: organize by stage of data processing <br>
(recommended so encrypted folder can have more limited access)
</small>
</p>
</th>
<th align="center">
<img width="441" height="1">
<p> 
<small>
Option 2: organize by data source
</p>
</th>
</tr>
<tr>
<td>

```
data
|_ encrypted
   |_ data-source1-identified.csv
   |_ data-source1-identified.rds
   |_ data-source2-identified.csv
   |_ data-source2-identified.rds
   |_ data-source3-identified.csv
   |_ data-source3-identified.rds
|_ raw-deidentified
   |_ data-source1-deidentified.rds
   |_ data-source2-deidentified.rds
   |_ data-source3-deidentified.rds
|_ clean
   |_ data-source1-clean.rds
   |_ data-source2-clean.rds
   |_ data-source3-clean.rds
|_ tidy
   |_ data-source1-level1-tidy.rds
   |_ data-source1-level2-tidy.rds
   |_ data-source3-level1-tidy.rds
   |_ data-source3-level2-tidy.rds
|_ constructed
|_ analysis 
```
  
</td>
<td>
  
```
data
|_ data-source1
   |_ data-source1-identified.csv
   |_ data-source1-identified.rds
   |_ data-source1-deidentified.rds
   |_ data-source1-clean.rds
   |_ data-source1-level1-tidy.rds
   |_ data-source1-level2-tidy.rds
   |_ data-source1-level1-constructed.rds
   |_ data-source1-level2-constructed.rds
|_ data-source2
   |_ data-source2-identified.csv
   |_ data-source2-identified.rds
   |_ data-source2-clean.rds
   |_ data-source2-constructed.rds
|_ data-source3
   |_ data-source3-identified.csv
   |_ data-source3-identified.rds
   |_ data-source3-clean.rds
   |_ data-source3-level1-tidy.rds
   |_ data-source3-level2-tidy.rds
   |_ data-source3-level1-constructed.rds
   |_ data-source3-level2-constructed.rds
|_ analysis
   |_ data-for-model1.rds
   |_ data-for-model2.rds
   |_ data-for-model3.rds
```
  
</td>
</table>

### `encrypted`/`identified` data

This is the "raw" data. That is, the original data you have collected or received, *exactly* as it was received. Note that a separate folder/dataset is only required if the original data contains personally identifiable information or other sensitive data, so access to this data can be restricted in compliance with the IRB.  

If the data was received in a format that is not easily handled by statistical software, such as CSV, you should also save a version of the data in a format that the statistical software you are using can read natively (for example .dta in Stata and .rds in R). This will reduce the time it takes to load the data.

### `deidentified` data

This is a deidentified version of the original data, that is, the original data stripped of direct identifiers. The point of storing de-identified data is to reduce the need to use files that contain confidential information, reducing both the risk of leaking sensitive data and the time and work required to decrypt the data before using it.

### `clean` data

The clean version of the data set contains exactly the same information as the original data, but in a format that is optimized for use in statistical software. This means, for example, turning categorical variables into factors or labeled values, turning survey codes into missing values, and creating variable labels.

### `tidy` data

When a single data table contains multiple units of observation (levels), it typically contains information in a *wide* format. However, although this may be an efficient format to transfer information on multiple units of observation in a single file, it is not an efficient format to analyze data. So the data needs to be tidied (or "normalized") before it can be analyzed. This means reshaping the data until each column represents one variable, each row represents one observation, and each table (file) represents one type of observational unit.

For example, if you collected survey data that has both household-level and household member-level information, then you will create two data tables, one called `survey-household-tidy` and one called `survey-household-member-tidy`. If you only have data on one unit of observation, then your clean data will already be tidy, and this folder is not necessary.

For more information on tidying data, see [Hadley Wickham's paper](https://vita.had.co.nz/papers/tidy-data.pdf).

### `constructed` data

Note that the differences between the data in the different stages of processing above are only related to its format, and not its content. Some confidential information has been removed, some data has been labeled, and it may have been reshaped. But there are no meaningful changes to individual data points between the original data and its tidy version. The `constructed` data, on the other hand, incorporates research decisions through the creation of new indicators or the modification of values. This includes, for example, constructing an index based on a set of variables, winsorizing or trimming observations, and creating subsamples. This data is typically still in a tidy format.

### `analysis` data

The `analysis` data sets are those that will be used as inputs for analysis scripts. These are typically created by combining multiple constructed tables and are not necessarily in a tidy format. You may, for example, combine multiple units of observation so that values for higher levels are used as controls in analysis conducted at a lower level of observation (e.g. controlling for district-level mortality when the unit of analysis is an individual). All data sets that need to be included in a reproducibility package should be stored in this folder.
