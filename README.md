To install the package:

devtools::install\_github("LobsterScience/at.sea.lobster")



Load package with library(at.sea.lobster)





About:



at.sea.lobster package is used for entry of At-Sea-Sampling (lobster) data. The main working function for inputting data is input.trip() which provides an R-Shiny based GUI to enter data by trip.





Creating / Finding a Trip:



To start a new trip, enter input.trip() in your R console. After choosing a folder to put your trip data file in, this will open the data entry GUI. You must first enter the Boarding Date and Vessel Registration Number (VRN) then click the Create/Find Trip button to begin entering data for your trip (if the folder you chose already contains a trip data file with this info, then the trip will be accessed to continue editing).





Entering / Checking Data:


Entering a trip creates a local .db (SQL database) file named by the vessel registration number and the boarding date. The file is stored in a location of the user's choosing.
.db files are SQL-based and work like an Oracle table stored directly on the user's hard drive. SQL language or RSQLite can be used for downstream processing of the data tables.
After data entry, the .db file will contain four relational tables: TRIP\_INFO, SET\_INFO, TRAP\_INFO, FISH\_INFO
Data tables can be checked in R with the check.table() function (ex. check.table("fish")). The function defaults to the last trip worked with in input.trip() but the user can manually choose a different trip file by changing the choose.trip argument to TRUE (example: check.table("fish", choose.trip = T)  ).





Exporting Data as CSV:



The data is meant to be maintained in SQL format (the .db files) for integration with the central database. However, if the user needs csv files of the data tables they can easily create them with the export.tables() function. Like check.table(), this function defaults to the last trip file edited in input.trip() but if the user wants to manually choose a trip file, they can use the choose.trip argument (example: export.tables("fish", choose.trip = T) ). To export multiple tables set tables = c('fish','trap'...) or to export all 4 tables for the chosen trip, set tables = "all". To export the data as a single merged table (all 4 tables merged into one for a given trip) use export.tables(merge.tables = T)





Handling Data Errors:



If a fish row has been entered incorrectly, the user can simply select that trap in the GUI and edit the row, then click "Save and Next Trap" to save the change. However, if any of the upstream information (Set Number) is incorrect for a trap number, then that trap must be deleted (using the "Delete Trap" button). Deleting a trap will remove all information for that trap from the database. You can then choose the correct set number and re-enter the data for the trap, then click "Save and Next Trap" to save.









