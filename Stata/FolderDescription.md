####Description of project folders  

===

The use of the folders created typically is as follows:  
__Datain__ - raw data or unprocessed data from which derived data is created  
__Dataout__   - derived data create in Stata or R  
__Excel__   -  any .xls, .xlsx, or .csv file that supports the project.. Often used for quick data vizualizations  
__Export__  - used to score cuts of data that are exported to other individuals or statistical programs (R, Python)  
__GIS__   - all GIS related data, documentation, and geodatabases. May need it's own sub-folder structure   
__Graph__ - graphics related to the project  
__Log__   - Stata log files or R log files documenting work flow    
__Output__  - Analysis output, such as estout or outreg2 files or statistical tables  
__PDF__   - For storing survey documentation, completed PDFs, or other project related documents that in this format  
__Programs__  - used to store customized programs created in Stata or R. These can be called in Stata or "sourced" in R  
__Python__  - Python files that execute geoprocessing, spatial or geostatistical analysis  
__Rawdata__   - All raw project data in any form. Data in the folder are to remain in their original form  
__Stata__   - Folder housing all of the do files for the project  
__Word__  - all Word related documents stored here 

*Additional folders can be created as needed for each project. This will generally be done using the 00_SetupGlobalsFolder.do file.*
