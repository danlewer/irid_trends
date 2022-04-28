# Trends in hospital admissions due to injecting-related infections in England

## Introduction

## Contents

1. [SQL query](https://github.com/danlewer/irid_trends/blob/main/HES_query.sql) for extracting relevant hospital episodes
2. R code used to analyse raw HES data (ie. requires HES data that is not saved in this repository to maintain confidentiality)
3. [Non-identifiable summary tables](https://github.com/danlewer/irid_trends/tree/main/summary_tables) of HES data, generated using R code
4. [Other inputs](https://github.com/danlewer/irid_trends/tree/main/input_data) (eg. population estimates, numbers of drug-related deaths)
5. R code used to generate final outputs - this code can be run using the data provided in this repository, and reads CSV files directly from Github (ie. you do not need to download data).

## Office for National Statistics sources

### Population estimates

From Nomis: [Population estimates - local authority based by single year of age](https://www.nomisweb.co.uk/datasets/pestsyoala) 

### Local authority code lookup (eg. London Borough of Hackney = '00AM' or 'E09000012')

Derived from an [lookup file](https://www.ons.gov.uk/file?uri=/census/2011census/consultationsusersandlocalpartners/censusadvisorygroups/censusgeneralcag/categorisationoflsoasfor2011censustcm77269651.xls) between LSOAs and local authority / region.

### Drug-related deaths in England & Wales

By region and substance: [Deaths related to drug poisoning by selected substances](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsrelatedtodrugpoisoningbyselectedsubstances)

By local authority: [Drug-related deaths by local authority, England and Wales](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/drugmisusedeathsbylocalauthority)

