# GlobalMix-sensor-analysis

**Contributors** <br/>
Machi Shiiba <sup>1</sup>, Moses C Kiti <sup>1</sup>, Obianuju Aguolu <sup>2</sup>, Noureen Ahmed <sup>3</sup>, Charfudin Sacoor <sup>4</sup>, Ivalda Macicame <sup>5</sup>, Edgar Jamisse <sup>4</sup>, Corssino Tchavana <sup>4</sup>, Orvalho Augusto <sup>4</sup>, Americo Jose <sup>5</sup>, Migdalia Wamba <sup>5, 6</sup>, Nilzio Cavele <sup>5, 6</sup>, Azucena Bardaji <sup>7</sup>, Herberth Maldonado <sup>8</sup>, Rajan Srinivasan <sup>9</sup>, Venkata Raghava <sup>9</sup>, Momin Kazi <sup>10</sup>, Raheel Allana <sup>10</sup>, Fauzia Malik <sup>3</sup>, Benjamin A Lopman <sup>1</sup>, Saad B Omer <sup>3</sup>, Kristin N Nelson <sup>1</sup>

*<sup>1</sup> Emory University Rollins School of Public Health, Atlanta, GA, USA* <br/>
*<sup>2</sup> The Ohio State University, Columbus, Ohio, USA* <br/>
*<sup>3</sup> Peter O'Donnell Jr. School of Public Health, UT Southwestern Medical Center, Dallas, TX, USA* <br/>
*<sup>4</sup> Manhiça Health Research Institute, Manhiça, Mozambique* <br/>
*<sup>5</sup> Polana Caniço Health Research Centre, Maputo, Mozambique* <br/>
*<sup>6</sup> Instituto Nacional de Saúde, Maputo, Mozambique* <br/>
*<sup>7</sup> Barcelona Institute for Global Health, Barcelona, Spain* <br/>
*<sup>8</sup> Universidad del Valle de Guatemala, Guatemala City, Guatemala* <br/>
*<sup>9</sup> Christian Medical College, Vellore, Tmil Nadu, India* <br/>
*<sup>10</sup> The Aga Khan University, Karachi, Pakistan* <br/>

*Correspondence to Ben Lopman (blopman@emory.edu)*

# Description of study
**Aim** <br>
Compare how effectively paper-based contact diaries and wearable proximity sensors capture contacts relevant for disease transmission in low- and middle-income household settings

The protocol that contains the detailed data collection procedure is explained in Aguolu et al (2024).<sup>1</sup>

# Description of repository
This repository contains data, scripts, and questionnaires.
Folders are arranged as follows.
1. Guatemala
2. India
3. Mozambique
4. Pakistan
5. Scripts
6. Questionnaires

Each country folder contains 'participant', 'participant_full', 'contact', 'contact_full', 'sensor' datasets, which contain information about the study participants and their reported contacts in diary and in sensor. 
The scripts folder has scripts used for the analysis of the "Comparison of contact diaries and wearable proximity sensors in measuring household contacts in low- and middle-income countries". The Questionnaires folder contains English version of survey questionnaires used at each site.

The datasets are named as follows;
- country-code_participant.RDS
- country-code_participant_full.RDS
- country-code_contact.RDS
- country-code_contact_full.RDS
- country-code_sensor.RDS

Country codes are **gt** (Guatemala), **in** (India), **mo** (Mozambique), **pa** (Pakistan).

# System requirements
- The code is written in R version 4.5.0 using RStudio version 2023.06.1. <br/>
- The following packages are used to run the code: [tidyr, dplyr, ggplot2, knitr, gridExtra, eulerr, grid, ggpubr, purrr, here]. <br/>
- The code has been developed and tested on Windows 11. The code should be compatible with Windows and Mac operating systems. <br/>
- No non-standard hardware is required to run the code.


# Installation guide
**Installing the latest version of R**
1. Go to the Comprehensive R Archive Network: https://cran.r-project.org/
2. Download the version for your operating system (e.g. Click Download R for Windows/macOS)
3. Follow the instructions provided

This should take about 3 minutes.

**Installing RStudio**
1. Go to the download RStudio website: https://posit.co/downloads/
2. Download RStudio for your operating system.
3. Follow the instructions provided

This should take about 2 minutes.

**Installing the R packages**
After installing R and RStudio, you can install packages using the following code.
```
install.packages(c("tidyr", "dplyr", "ggplot2", "knitr", "gridExtra", "eulerr", "grid", "ggpubr", "purrr", "here"))
```
This should take about 5 minutes.

# Instructions for running the scripts
1. Load the packages and run the functions in the "Summary funtion and figures" file.
2. Run the code in the "(countrycode)_main_analysis" file.
3. If needed, code for creating multipanel summary figures is in the "Summary function and figures" file, which can be used after running all the country's main analysis code.

Each file should take about 20 seconds to run all the code. All the outputs corresponding to the Figure numbers and table numbers are available in main text and supplemental materials.

## Reference
1. Aguolu OG, Kiti MC, Nelson K, et al. Comprehensive profiling of social mixing patterns in resource poor countries: A mixed methods research protocol. PLOS ONE. 2024;19(6):e0301638. doi:10.1371/journal.pone.0301638
