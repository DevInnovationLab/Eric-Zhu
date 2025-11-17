/*******************************************************************************

	TEMPLATE RANDOM TREATMENT ASSIGNMENT CODE
	
	This code is meant to be used as a starting point for randomization code.
	It needs to be edited to fit your project data and experimental design.
	
	Last update: Oct 21, 2024

*******************************************************************************/

**# Stable settings ============================================================

	local version 15.1 // Set to lowest version used across team members
//	sysdir set PLUS "${ado}" // Location of ado files in repository -- this code requires the packages randtreat and ietoolkit
	ieboilstart, versionnumber(`version')
	
	local id 		make
	local strata	foreign
	local secluster	foreign
	local tmt		treatment
	local balance	price mpg rep78 headroom trunk weight

**# Load data ==================================================================

	sysuse auto, clear

	local seed = 682398	 // go to bit.ly/stata-random to get a new random seed
	isid `id', sort // replace with data set's unique identifier(s)

**# Randomization ==============================================================
	
	* See help files for all randtreat options before running next line of code!!
	randtreat, ///
		gen(`tmt') ///
		strata(`strata') ///
		misfits(global) ///
		setseed(`seed') 

**# Save treatment assignment data =============================================

	iesave "treatment_asignment.dta", replace idvars(`id') report version(`version') userinfo

	* Export IDs and treatment assignment for version control tracking
	export delimited `id' `strata' `tmt' /// add any other relevant ANONYMIZED variables
		using savetex, replace // ONLY REPLACE IF YOU HAVE VERSION CONTROL SETUP. otherwise, timestamp the file
		

**# Run balance checks =========================================================

	* See help filed for all options and formatting
	iebaltab `balance', ///
		groupvar(`tmt') ///
		fixedeffect(`strata') ///
		vce(cluster `secluster') ///
		ftest ///
		savetex("balance.tex")
		
//--------------------------------------------------------------- End of do-file