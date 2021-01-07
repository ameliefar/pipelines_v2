##### Table of Contents (general user) 
[Load the pipeline package](#load)  

[Pipeline documentation](#docs)

[Run the pipelines for yourself](#run)  


##### Table of Contents (developers) 
[Primary data storage](#storage)  

[Recommended workflow](#workflow)

[Dealing with new data](#newdata)  

# SPI-Birds pipeline: Introduction (for the general user)

Welcome to the SPI-Birds pipeline package. This README will give you an introduction on how to load the package, how to find out details about each pipeline, and how to use the package for creating bird data following the [SPI-Birds community data standard](https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf) and generating standard quality checks.

<a name="load"/>

## Load the pipeline package

The pipeline package can be installed in R using the following code from the `devtools` package:

```
devtools::install_github("SPI-Birds/pipelines")
library(pipelines)
```

This will install all pipelines and quality check code on your computer and load the code into your session of R. Individual pipelines are build as separate functions for each data owner (where one data owner can administer multiple populations). Each function is given the name `format_X()` where *X* is the letter code for the data owner. The codes for different data owners and corresponding populations are described in the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf). *Note* in cases where a data owner administers one population, the unique 3 letter population ID code and the data owner code are identical.

<a name="docs"/>

## Pipeline documentation

To process each set of primary data into the structure described in the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.1.0.pdf) it is often necessary to make assumptions about how each variable is interpreted. All assumptions made during the pipeline process are described in the help documentation for a given function. This can be accessed using the `?` in R. For example, to read about the assumptions made when processing data from the NIOO, you can use the code:

```
?format_NIOO
```

<a name="run"/>

## Run the pipelines for yourself

Each set of primary data is in a slightly different format. Therefore, to run all pipelines successfully, your system will require additional software and drivers (in addition to R).

### Setup your computer to run pipelines

Pipelines for some populations require additional software and software drivers. These setup instructions describe how to install the required software for a Windows 10 64-bit operating system (on which the pipelines were built). The setup procedure should be similar for other Windows 64-bit systems, but will differ for non-Windows operating systems. If you are unsure which version of Windows is running on your computer, check 'System Type' in 'System Information'. To run the pipelines for all populations a users system must have:

- Microsoft Access Driver (/*.mdb, /*.accdb)
- Python 3
- Python libraries *pandas* and *pypxlib*

---

#### Windows 10 64-bit

##### Microsoft Access Driver

Firstly, you must check that you are running a 64-bit version of R. Open an R session and see whether you have 64-bit or 32-bit installation.

![](https://github.com/LiamDBailey/SPIbirds/blob/master/inst/extdata/README_imgs/R_version.jpg)

If you do not have a 64-bit version you will need to install one [here](https://www.r-project.org/).

---

Once you have a 64-bit version of R, search for 'ODBC' in the Windows taskbar. There will be two version (32-bit and 64-bit) ***select the 64-bit version***. This will open the 'ODBC Data Source Administrator (64-bit)' window.

In the new window check for 'Microsoft Access Driver'. If this already exists you can skip to the Python stage.

If 'Microsoft Access Driver' does not exist click 'Add' to install a new driver on your system.

![](https://github.com/LiamDBailey/SPIbirds/blob/master/inst/extdata/README_imgs/ODBC_driver.jpg)

---

Select 'Microsoft Access Driver (/*.mdb, /*.accdb)' and click finish.

***If 'Microsoft Access Driver (/*.mdb, /*.accdb)' does not appear, you will need to download the 64-bit driver [here](https://www.microsoft.com/en-US/download/details.aspx?id=13255)***

In the next window, you ***must*** add a 'Data Source Name'. Everything else can be left blank.

![](https://github.com/LiamDBailey/SPIbirds/blob/master/inst/extdata/README_imgs/Add_name.jpg)

Check if this driver is installed and recognised by R using the function `odbcListDrivers()` in the `odbc` package. Note that you will need to open a new session of R before the driver will appear.

##### Python 3

To install Python, we recommend using the [Anaconda distribution](https://www.anaconda.com/distribution/). ***Make sure to download the 3.X version of Python***. The Anaconda distribution comes with some libraries (including *pandas*) pre-loaded.

Once installed, open the 'Anaconda prompt' and type:

`pip install pypxlib`

This will install the *pypxlib* library on your system.

Restart your computer before running the pipelines.

#### MikTex

To generate the pdf quality check report on Windows you will need to have installed [`MikTex`](https://miktex.org/). If MikTex is not installed, only the html version of the qualit check report can be created.

#### Mac

Work in progress..

#### Troubleshooting

If you are still unable to run the pipelines following these setup instructions try these troubleshooting techniques:

- Restart your computer before running pipelines to ensure R recognises the newly installed software and drivers.

- If R does not recognise Python's *pandas* module, try installing it using ```reticulate::py_install("pandas")```.

- Download the newest version of R [here](https://www.r-project.org/).

- Update all R packages.

---

### Primary data naming conventions

All pipelines assume the primary data is stored using a standard naming protocol: `X_PrimaryData_Y`. Where *X* is the data owner code (described above) and *Y* is additional information used to distinguish between multiple primary data files. The exact naming convention of primary data for each pipeline is described in the help. *Note:* All primary data files for a given pipeline should be stored in a single folder.

### Running the pipelines

Once your computer is set up and primary data follow the correct naming protocol you can run the pipeline function. R will ask you to select the folder where the primary data are stored. You can decide on the output create by the pipeline using the argument `output_type`, which can be either "csv" (as separate .csv files, the default) or "R" (as an R object).

```
format_NIOO(output_type = "R")
```

# SPI-Birds pipeline: Introduction (for the SPI-Birds team)

This provides additional information for the SPI-Birds team, which isn't relevant for the general user.

<a name="storage"/>

## Primary data storage

All primary data are stored within a single folder on the NIOO SPI-Birds computer. Primary data is stored following the standard naming approach described above `X_PrimaryData_Y`. In addition, you should ensure that:

* The names of primary data file(s) for each population should not be changed. When updating with data from new years, make sure the name of the new data is the same as the old data following the naming protocol described above.
* Data from each population should have its own folder. The folder name should include the letter code associated with the data owner (e.g. HAR for Harjavalta, NIOO for NIOO).
* All populations folders should be stored in the same location.

<a name="workflow"/>

## Create data in the standard format

Each year when primary data are updated all pipelines will be re-run for all populations. This is done using the function `run_pipelines()`. This function has 4 arguments:

* path: The location of the folder where all population data is stored. Can be left blank and R will prompt you to find the folder.
* PopID: The population code(s) for the populations where you want to run pipelines.
* Species: The species code(s) for the species you want to use (e.g. PARMAJ for great tit). See the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.1.0.pdf) for all species codes.
* output_type: Should the data be output in R or as separate .csv files

For example, the below code will format great tit data from Harjavalta (Finland) and Choupal (Portugal) and output .csv files in the location where the data are stored.

```r
run_pipelines(path = "C:\\all_data", PopID = c("HAR", "CHO"), Species = "PARMAJ", output_type = "csv")
```

**Note:** If you select a PopID/Species combination that does not exist, this population will be skipped.

In most cases, we will want to run all pipelines together. In this case, the arguments *PopID* and *Species* can be ignored.

## Recommended workflow

*THIS WORKFLOW IS IN BETA. CHANGES NEED TO BE MADE TO STREAMLINE THE PROCESS*

1. Run pipelines for populations of interest using `run_pipelines` return an R object. If you are just outputting the standard format for a user request there is no need to save the output as an .RDA file.

```r
standard_format <- run_pipelines(PopID = "HOG", output_type = "R", save = FALSE)
```

2. Run the quality check on the newly created object. Generally, we will just want to pdf output. The file should be created in the working directory.

```r
quality_check(standard_format, output_format = "pdf")
```

3. For now, we need to run `run_pipelines` again to output the 4 .csv files to send to the user.

```r
run_pipelines(PopID = "HOG", output_type = "csv")
```

<a name="newdata"/>

## Dealing with new data

When we get updated data (or get data for a new population) we need to set up the archiving folder on the N drive using the `archive` function.

### New population

1. Create a new folder in N:\Dep.AnE\SPI_Birds\data. It should follow the syntax `<OWNERID>_<PopName>_<Country>`
2. Rename files.
    - Primary data should follow the syntax `<OWNERID>_PrimaryData`. If there are multiple primary data files provide different suffixes to differentiate them (e.g. `<OWNERID>_PrimaryData_GTData`
    - Population meta-data should follow the syntax `<OWNERID>_MetaData`
    - All other data that is not meta-data or primary data can be named in any way, but should always start with `<OWNERID>_`
3. Create initial archive. The below code will generate a `ArchiveMetaData.txt` file and generate an archive folder for the new population. **Important**: Make sure you specify that this is the initial archive with `initial = TRUE`.
```
archive(data_folder = "N:\Dep.AnE\SPI_Birds\data", OwnerID = <OWNERID>, new_data_date = <DATE WHEN DATA WERE RECEIVED>, initial = TRUE)
```

### Updated population

1. Rename new files to match existing data files (i.e. with the syntax `<OWNERID>_PrimaryData`). **Important**: Files should have the **exact** same name, otherwise the pipelines may break. If you do need to use new file names (and rework the pipeline) you will be given a prompt to continue.
2. Decide if we are dealing with a 'minor' update (e.g. fix typos) or a 'major' update (e.g. new year of data).
2. Run archiving code:
```
archive(data_folder = "N:\Dep.AnE\SPI_Birds\data", OwnerID = <OWNERID>, update_type = <"major"/"minor">,
        new_data_path = <LOCATION OF NEW FILES. Consider using choose.files()>,
        new_data_date = <DATE WHEN DATA WERE RECEIVED>, initial = FALSE)
```
