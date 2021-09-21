## BYU WBB Ratings Model modelBuild directory

This folder contains the data retrieval and model building process for our ratings model. So far, our data has been obtained from Synergy Sports by hand and plugged into a Box Plus-Minus calculator obtained from Basketball Reference's [BPM Explanation](https://www.basketball-reference.com/about/bpm2.html) using the [shared Google Sheet](https://docs.google.com/spreadsheets/d/1PhD9eo3IqzpQo21-yVJPQzYjpXl_h-ZonIKqGEKBqwY/edit#gid=307166562).
Any calculalations that were needed to be done followed the formulas given on Basketball Reference's [Glossary](https://www.sports-reference.com/cbb/about/glossary.html#team_id) and currently we are using ORtg, DRtg and NRtg instead of ORtg/A, DRtg/A, NRtg/A for the BPM calculations.
These plug-ins and subsequent calculations are contained in _bpmCalculation.xlsx_. For now, _dataClean.Rmd_, _modelTest.Rmd_, and _modelTrain.Rmd_ are blank, pending approval of the calculations. 

