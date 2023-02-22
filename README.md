# Trends in hospital admissions due to injecting-related infections in England

## Introduction

## Contents

1. [SQL queries](https://github.com/danlewer/irid_trends/tree/main/sql_queries) for extracting data from Hospital Episode Statistics
2. [R code](https://github.com/danlewer/irid_trends/blob/main/r_code/process_raw_HES.R) used to analyse raw HES data (ie. requires HES data that is not saved in this repository to maintain confidentiality)
3. [Non-identifiable summary tables](https://github.com/danlewer/irid_trends/tree/main/summary_tables) of HES data, generated using R code
4. [Other inputs](https://github.com/danlewer/irid_trends/tree/main/input_data) (eg. population estimates, numbers of drug-related deaths)
5. [R code](https://github.com/danlewer/irid_trends/blob/main/r_code/final_outputs.R) used to generate final outputs and [draw a map](https://github.com/danlewer/irid_trends/blob/main/r_code/map.R). This code can be run using the data provided in this repository, and reads CSV files directly from Github (ie. you do not need to download data).

## Office for National Statistics sources

- **Drug-related deaths in England & Wales**: By region and substance: [Deaths related to drug poisoning by selected substances](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsrelatedtodrugpoisoningbyselectedsubstances); by local authority: [Drug-related deaths by local authority, England and Wales](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/drugmisusedeathsbylocalauthority)

