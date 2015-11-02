/*-------------------------------------------------------------------------------
# Name:		99_CleanUp
# Purpose:	Remove empty project folders from main project directory
# Author:	Tim Essam, Ph.D.
# Created:	2015/09/10
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

local maclist $pathdo $pathlog $pathgraph $pathxls ////
$pathreg $pathgis $pathexport $pathRin $pathProg
display "`maclist'"

/* Create loop to do the following:
1) Check if requested folder exists
2) Check the folder and return the count of files
3) If the count equals zero, delete the folder
4) Report in red when a folder is deleted.
*/
foreach x of local maclist {
	confirmdir `x' 
		if `r(confirmdir)'!=170 {
			cd `x'
			local list:dir . files "*"
			local numfiles : word count `list'
			*di in yellow "`numfiles' files found in `x'"
			cd $path	
				if `numfiles'==0 {
					rmdir `x' 
					display in red "`x' folder is empty. Removing."
		}
	}
}
*end
