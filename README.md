# SuccessFactorsInNationalTeamFootball

This repository provides the code used for the thesis 'Success Factors in National Team Football: A detailed Analysis of the UEFA EURO 2020'. All the relevant files can be found in the 'master' branch.

If the creation of the dataset is to be repeated, you need to run the script 'Scraper_main.py' in the WebScraping Folder first, to create the necessary dataset. This has to be done for the 2020 and 2016 folder seperately. It has to be mentioned, that a repeated creation of the dataset might lead to differing results, and even the code not working at all, due to reconstruction of the respective websites that are being scraped.

Otherwise, you could copy the already provided dataset into the WebScraping folder for the respective year, and run the analysis with that.
For this, you need to access the 'Data' folder and copy the 'Data_complete.csv' file into './WebScraping/2020' and the 'Data_complete_2016.csv' file into './WebScraping/2016'.

To conduct the analysis, you need to run the 'AdditionalVariables_PreProcessing.R' script first, to get the full dataset. Then all the other scripts can be run.
