#rstudioapi::restartSession()
rm(list =ls())

# Housekeeping ------------------------------------------------------------
par(mar=c(1,1,1,1))           #1 plot per plot window
options(scipen = 999)         #Eliminate auto rounding
options(width = 80)           #Code width
options(max.print = 200)      #Limited for quicker run time
options(pkgType = "source")   #
memory.size(max=TRUE)         #Devote maximum RAM amount to Rs
Sys.setenv(TZ = "GMT")        #Setting GMT as default
TZ <- "GMT"                   #enabling future use of global timezone call
set.seed(1776)                #


# ** CM & Version ------------------------------------------------------------------------------------------
Client.Matter.Info<-"Jax Lab - Keen (108321-008)"
Version.Number<-"Removal Analysis v1"
Client.Matter.Info.AND.Version.Number<-paste(Client.Matter.Info, "-", Version.Number)



#** Working Directory --------------------------------------------------------------------------------------
WD<-file.path("A:/Cases_Github", gsub("\\s|[[:punct:]]", "-", Client.Matter.Info))



# Packages Enablement -----------------------------------------------------
require(checkpoint)
setSnapshot("2020-12-10")
#checkpoint("2020-04-30", project=WD, forceProject = TRUE) ##NOTE: must be YYYY-MM-DD

pkgs<-rev(c("dplyr",        ## for data handling
            "stringr",      ## for strings
            "stringdist",   ## for string distance/fuzzy match
            "tidyr",        ## for data structures
            "lubridate",    ## for date handling
            "data.table",   ## for data structures and reading/writing
            "ggplot2",      ## for internal visualizations
            "plotly",       ## for interactive visualizations
            "ggrepel",      ## for labeling within ggplot2
            "readxl",       ## for ingesting xlsx files
            "openxlsx",     ## for exporting objects to a finalized excel template
            "pdftools",     ## for PDF extraction
            #"caret",       ## for machine learning modeling
            "shiny",        ###### for web app development
            "svDialogs",    ## for prompting user for mistype inputs
            "pryr",         ## for faster object size print outs & memory management!
            "devtools",     ## for certain functions lacking in base
            "httr",         ### Direct access to websites/URLs
            "tictoc",       ## for tracking calculation times
            "beepr"))      ## for placing audio chimes to signal completed calculations


#as.data.frame(lapply(pkgs, install.packages, dependencies = TRUE))
#install.packages(pkgs, dependencies=TRUE)
as.data.frame(lapply(pkgs, require, character.only=TRUE)) #Reversed for 1st package priority
sessionInfo()




# * --------------------------------------------------------------------------------------------------------
# File Checks ----------------------------------------------------------------------------------------------

# ** Custom Functions --------------------------------------------------------

#source_url("https://raw.githubusercontent.com/PGundy/Useful-Custom-R-Functions/master/Custom%20Functions.R")

WD.Code<-file.path(WD, "Code")
dir.create(WD.Code, showWarnings = F)

if ("Custom Functions from Master Branch of PGundy's Github.R" %in% list.files(WD.Code) ){
  source(file.path(WD.Code, "Custom Functions from Master Branch of PGundy's Github.R"))
}else{
  
  download.file("https://raw.githubusercontent.com/PGundy/Useful-Custom-R-Functions/master/Custom%20Functions.R",
                file.path(WD.Code, "Custom Functions from Master Branch of PGundy's Github.R"))
  
  source(file.path(WD.Code, "Custom Functions from Master Branch of PGundy's Github.R"))
}





# ** File Name ---------------------------------------------------------------------------------------------
File.Name<-paste0(Client.Matter.Info.AND.Version.Number, ".R")                     ## File.Name used in backup sys
File.Name2<-str_remove(File.Name, "\\.R$")                                   ## File.Name used in code versions
WC.FILE <- (function() { attr(body(sys.function()), "srcfile")})()$filename  ## Pulls the file that is open

      ##Below checks to see if the current WD & file are the same as the file being backed up.
      ### The comparison is done on all non-punct characters to avoid Regex issues
      ifelse( str_remove(WC.FILE, ".*\\/")==File.Name,
              #File names match!
              "Versioning code will work normally",
              #Fix the definition of File.Name OR change the name of the active file!
              stop("Please update the variable 'File.Name' to correctly match the FILE variable"))
      
      
      
      
      
      
      
#** Named Plaintiff --------------------------------------------------------------------------------------
Named.Plaintiff<-data.frame(Employee.ID=(""),
                            Employee.Name=c(""),
                            Named.Plaintiff="Named Plaintiff")
      
      
# * --------------------------------------------------------------------------------------------------------
      
      
      
      
      
      
      
      
      
      
      
      
      
# Models ---------------------------------------------------------------------------------------------
      
# * Model Options ------------------------------------------------------------------------------------------

  #Versions of Models available
  list.dirs(path=file.path("M:/099999-000166/Analytics/Analytics Source R Code","Ranked Choice Model"), 
      recursive=F)
  #Models within v1.50
  list.files(path=file.path("M:/099999-000166/Analytics/Analytics Source R Code","Ranked Choice Model/v1.50"))
  
  
  
  

# * Picking Models -----------------------------------------------------------------------------------------

# * TC -----------------------------------------------------------------------------------------------------
M.Drive.TC.Analysis.Model<-file.path("M:/099999-000166/Analytics/Analytics Source R Code/Ranked Choice Model",
                             "v1.50", 
                             "v1.50 TC Analysis.R")
      
      Case.Specific.TC.Analysis.Model<-paste(Client.Matter.Info,
                                             "-USING MODEL-", #Below pulls the core parts of the model name
                                             str_remove(M.Drive.TC.Analysis.Model, ".*\\/") )
      
      
# * SH -----------------------------------------------------------------------------------------------------
M.Drive.SH.Analysis.Model<-file.path("M:/099999-000166/Analytics/Analytics Source R Code/Ranked Choice Model", 
                             "v1.50", 
                             "v1.50 SH Analysis.R")
     
     Case.Specific.SH.Analysis.Model<-paste(Client.Matter.Info,
                                            "-USING MODEL-", #Below pulls the core parts of the model name
                                            str_remove(M.Drive.SH.Analysis.Model, ".*\\/") )



# * EMPL ---------------------------------------------------------------------------------------------------
M.Drive.EMPL.Analysis.Model<-file.path("M:/099999-000166/Analytics/Analytics Source R Code/Ranked Choice Model", 
                             "v1.50", 
                             "v1.50 EMPL Analysis.R")
     
     Case.Specific.EMPL.Analysis.Model<-paste(Client.Matter.Info,
                                            "-USING MODEL-", #Below pulls the core parts of the model name
                                            str_remove(M.Drive.EMPL.Analysis.Model, ".*\\/") )



# ** Model Inputs ---------------------------------------------------------------------------------------------
# *** Filing.Date ---------------------------------------------------------------------------------------------

  Filing.Date<-ymd("2021-05-06")
     Filing.Date<-ifelseC(is.na(as.Date(Filing.Date)), ymd("2000-01-01"), Filing.Date)
     Filing.Date
  ResetLength<-4



# *** Export File Names ---------------------------------------------------------------------------------------
  
  
Compiled.TC.Data.RDS.format<-paste(Client.Matter.Info.AND.Version.Number, "- Timecard Pre-Analysis.RDS")
Compiled.PR.Data.RDS.format<-paste(Client.Matter.Info.AND.Version.Number, "- Payroll Pre-Analysis.RDS")
Compiled.CEN.Data.RDS.format<-paste(Client.Matter.Info.AND.Version.Number, "- Census Pre-Analysis.RDS")
  
Export.File.TC.RDS.format<-paste(Client.Matter.Info.AND.Version.Number, "- Timecard Collapse (pre-sh, tc).RDS")
Export.File.TC.CSV.format<-paste(Client.Matter.Info.AND.Version.Number, "- Timecard Collapse (pre-sh, tc).CSV")

Export.File.TC.OT.RDS.format<-paste(Client.Matter.Info.AND.Version.Number, "- Timecard Overtime Table (tc OT).RDS")
Export.File.TC.OT.CSV.format<-paste(Client.Matter.Info.AND.Version.Number, "- Timecard Overtime Table (tc OT).CSV")
Export.File.TC.OT.XLSX.format<-paste(Client.Matter.Info.AND.Version.Number, "- Timecard Overtime Table (tc OT).XLSX")

Export.File.SH.RDS.format<-paste(Client.Matter.Info.AND.Version.Number, "- Shift Collapse (sh).RDS")
Export.File.SH.CSV.format<-paste(Client.Matter.Info.AND.Version.Number, "- Shift Collapse (sh).CSV")

Export.File.EMPL.CSV.format<-paste(Client.Matter.Info.AND.Version.Number, "- Employee Collapse (EMPL).CSV")



# * --------------------------------------------------------------------------------------------------------















# Directory Builder ----------------------------------------------------------------------------------------
# * Directory Creation ----------------------------------------------------------------------------#


# ** dir: Version ---------------------------------------------------------------------------------#
# Analysis Version
WD.Analysis<-file.path(WD, "Analysis")
WD.Analysis.Version<-file.path(WD, "Analysis", Version.Number)
dir.create(file.path(WD.Analysis.Version), recursive = T, showWarnings = F)




# ** dir: Code ------------------------------------------------------------------------------------#
#Code
#Any and all versions of code saved here
WD.Code<-file.path(WD, "Code")
dir.create(WD.Code, showWarnings = F)

# Code file within the versioned folder
file.copy(file.path(WD.Code, File.Name), 
          file.path(WD.Analysis.Version, paste(Version.Number, "of", File.Name)),
          overwrite = T)





# ** dir: Raw Data -------------------------------------------------------------------------------#
#Raw Data
#Data from Client
WD.Raw.Data<-(file.path(WD, "Raw Data"))
dir.create(WD.Raw.Data, showWarnings = F)


WD.Raw.Data.pr<-(file.path(WD.Raw.Data, "Payroll"))
dir.create(WD.Raw.Data.pr, showWarnings = F)

WD.Raw.Data.tc<-(file.path(WD.Raw.Data, "Timecard"))
dir.create(WD.Raw.Data.tc, showWarnings = F)

WD.Raw.Data.cen<-(file.path(WD.Raw.Data, "Census"))
dir.create(WD.Raw.Data.cen, showWarnings = F)


dir.create(file.path(WD.Raw.Data, "Zips"), showWarnings = F)
dir.create(file.path(WD.Raw.Data, "To Be Sorted"), showWarnings = F)


# ** dir: Compiled -------------------------------------------------------------------------------#
#Compiled Data
# For use across ALL versions
WD.Compiled.Data.AGNOSTIC<-(file.path(WD.Analysis, "~ Agnostic Compiled Data"))
dir.create(WD.Compiled.Data.AGNOSTIC, recursive = T, showWarnings = F)


#Data prepared for analysis
WD.Compiled.Data<-(file.path(WD.Analysis.Version, "Compiled Data"))
dir.create(WD.Compiled.Data, recursive = T, showWarnings = F)

WD.Compiled.Data.pr<-(file.path(WD.Analysis.Version, "Compiled Data", "Payroll"))
dir.create(WD.Compiled.Data.pr, recursive = T, showWarnings = F)

WD.Compiled.Data.tc<-(file.path(WD.Analysis.Version, "Compiled Data", "Timecard"))
dir.create(WD.Compiled.Data.tc, recursive = T, showWarnings = F)

WD.Compiled.Data.cen<-(file.path(WD.Analysis.Version, "Compiled Data", "Census"))
dir.create(WD.Compiled.Data.cen, recursive = T, showWarnings = F)




# ** dir: Prod Data -----------------------------------------------------------------------------#

#Production Data
#Data to be produced
WD.Production.Data<-(file.path(WD.Analysis.Version, "Production Data"))
dir.create(WD.Production.Data, recursive = T, showWarnings = F)




# **Other Dirs -----------------------------------------------------------------------------------#

#Emails
#Any emails containing data, analysis requests, case info, ect.
dir.create(file.path(WD, "Emails"), showWarnings = F)


#Legal Docs
#Any legal documentation saved during case
dir.create(file.path(WD, "Legal Docs"), showWarnings = F)


#Visualization Data
#Data to be visualized in tableau
WD.Visualization.Data<-(file.path(WD.Analysis.Version, "Visualization Data"))
dir.create(WD.Visualization.Data, recursive = T, showWarnings = F)


#For tableau PDFs
dir.create(file.path(WD.Visualization.Data, "Exports"), recursive = T, showWarnings = F)


#Tables and Summaries
#Data containing exposure calculations
WD.Tables.and.Summaries.Data<-(file.path(WD.Analysis.Version, "Tables and Summaries Data"))
dir.create(WD.Tables.and.Summaries.Data,recursive = T, showWarnings = F)


#QC Data
#Data specifically intended for QC purposes
WD.QC.Data<-(file.path(WD.Analysis.Version, "QC Data"))
dir.create(WD.QC.Data,recursive = T, showWarnings = F)


# * --------------------------------------------------------------------------------------------------------




# ******************* ------------------------------------------------------------------
# Weekperiod ------------------------------------------------------
######################################################################################!
##### Copy of Calendar from 1984 for aid in start workweeks on correct day of the week
######################################################################################!
#Sunday   #Monday   #Tuesday   #Wednesday   #Thursday   #Friday   #Saturday
#1st      #2nd      #3rd       #4th         #5th        #6th      #7th
#8th      #9th      #10th      #11th        #12th       #13th     #14th
#15th     #16th     #17th      #18th        #19th       #20th     #21st
#22nd     #23rd     #24th      #25th        #26th       #27th     #28th
#29th     #30th     #31st
######################################################################################!


#NOTE: this may change based on workweek start day 
#Changed to match payroll pay periods 
Weekperiod<-data.frame(Date=ymd("1984 Jan 02") + ddays((1:(52*50))*7)) # Mon-Sun
Weekperiod$Pay.Period.Weekly.Start<-(Date=ymd("1984 Jan 02") + ddays((1:(52*50))*7)) #weekly

#DayPeriod                  #NOTE: this should always be "1984 Jan 01"
Dayperiod<-data.frame(Date=ymd("1984 Jan 02") + ddays(1:(52*50*7))) 
Weekperiod<-full_join(Weekperiod, Dayperiod) %>% arrange (Date)

#Weekly
Weekperiod$Pay.Period.Weekly.Start<-fillNA(Weekperiod$Pay.Period.Weekly.Start)
Weekperiod$Pay.Period.Weekly.End<-date(Weekperiod$Pay.Period.Weekly.Start+(ddays(7)-dseconds(1)))

#Biweekly                       #NOTE: Biweekly should ALWAYS be weekperiod start + 7 days
Biweekyperiod<-data.frame(Date=ymd("1984 Jan 02") + ddays(1:(52*50))*14)
Biweekyperiod$Pay.Period.Biweekly.Start<-(Date=ymd("1984 Jan 02") + ddays(1:(52*50))*14)
Weekperiod<-full_join(Weekperiod, Biweekyperiod) %>% arrange (Date)

#Payperiod (bi-weekly)
Weekperiod$Pay.Period.Biweekly.Start<-fillNA(Weekperiod$Pay.Period.Biweekly.Start)
Weekperiod$Pay.Period.Biweekly.End<-date(Weekperiod$Pay.Period.Biweekly.Start+(ddays(14)-dseconds(1)))


rm(Dayperiod)


# ******************* ------------------------------------------------------------------









# Load Data -----------------------------------------------------------------------------------------------------
# * Census Load -------------------------------------------------------------------------------------------------

list.files(WD.Raw.Data.cen, recursive = T)


{
  cen<-read_excel(
    file.path(WD.Raw.Data.cen,
              "2021.05.26/All CA Hourly Workers since 11.5.2016(henry.chung@jax.org).xlsx"))
  names(cen)<-colNamesCleaner(cen)
  
  cen<-cen %>% 
    rename(EEID=Employee.ID) %>% 
    mutate(Employee.Name=paste(Legal.Last.Name, Legal.First.Name, sep=", "))
  
  glimpse(cen)
}

# *** -----------------------------------------------------------------------------------------------------------



# Removal Analysis ----------------------------------------------------------------------------------------------

table(cen$Pay.Rate.Type, cen$Pay.Frequency) ## 556 Hourly emps all paid biweekly


SOL.4yr.Start<-Filing.Date-years(4)-days(182)
SOL.PAGA.Start<-Filing.Date-years(1)-days(182)

cen_REMOVAL_STATS<-cen %>% 
  select(Worker.Type,
         EEID, Employee.Name,
         Time.Type, Pay.Frequency, Pay.Rate.Type, Pay.Rate,
         Original.Start.Date, Adjusted.Start.Date, Termination.Date, Previous.Termination.Date ) %>% 
  mutate(Original.Start.Date=as.Date(Original.Start.Date), 
         Adjusted.Start.Date=as.Date(Adjusted.Start.Date), 
         Termination.Date=as.Date(Termination.Date), 
         Previous.Termination.Date=as.Date(Previous.Termination.Date)) %>% 
  mutate(Is.Terminated=!is.na(Termination.Date),
         Has.Period2=(Original.Start.Date!=Adjusted.Start.Date),
         
         Period1_Start=Original.Start.Date,
         Period1_End=ifelseC(!is.na(Previous.Termination.Date), Previous.Termination.Date, Termination.Date),
          ## Censor to SOL
          Period1_Start=ifelseC(Period1_Start<SOL.4yr.Start, SOL.4yr.Start, Period1_Start),
          Period1_End=ifelseC(Period1_End<SOL.4yr.Start, SOL.4yr.Start, Period1_End),
         
         
         Period2_Start=ifelseC(Has.Period2, Adjusted.Start.Date, NA_Date_),
         Period2_End=ifelseC(Has.Period2, Termination.Date, NA_Date_),
          ## Censor to SOL
          Period2_Start=ifelseC(Period2_Start<SOL.4yr.Start, SOL.4yr.Start, Period2_Start),
          Period2_End=ifelseC(Period2_End<SOL.4yr.Start, SOL.4yr.Start, Period2_End),
         
         
         Period1_Start=replace_na(Period1_Start, ymd("2021-05-31") ),
         Period1_End=replace_na(Period1_End, ymd("2021-05-31") ),
         Period2_Start=replace_na(Period2_Start, ymd("2021-05-31") ),
         Period2_End=replace_na(Period2_End, ymd("2021-05-31") ),
         
         
         Period1_Start=ifelseC(Period1_Start==Period1_End, NA_Date_, Period1_Start),
         Period1_End=ifelseC(Period1_Start==Period1_End, NA_Date_, Period1_End),
         Period2_Start=ifelseC(Period2_Start==Period2_End, NA_Date_, Period2_Start),
         Period2_End=ifelseC(Period2_Start==Period2_End, NA_Date_, Period2_End),
         
         " "="",
         EMP.Start=pmin(Period1_Start, Period2_Start, na.rm=TRUE),
         EMP.End=pmax(Period1_End, Period2_End, na.rm=TRUE) ) %>% 
         
         filter(!is.na(EMP.Start) & !is.na(EMP.End)) %>% 
  mutate(
    Emp.Active=ifelse(EMP.End==ymd("2021-05-31"), "Active", "Termed"),
    EMP.Termed=Is.Terminated,
    EMP.Termed.3yr.SOL=ifelse(EMP.Termed,
                              ifelse(EMP.End>Filing.Date-years(3)-days(182), TRUE, FALSE),
                              NA),
    
    ## 4yr SOL
    Total_Work_Weeks=ceiling(as.numeric(difftime(EMP.End, EMP.Start, units="weeks"))),
    Total_Pay_Periods=ceiling(Total_Work_Weeks/2),
    
    
    ## 1yr SOL
    Within.1yr.SOL=pmax(EMP.Start, EMP.End)>(SOL.PAGA.Start),
    Period1_Start_1yrSOL=ifelseC(EMP.Start<(SOL.PAGA.Start), SOL.PAGA.Start, EMP.Start), 
    Period1_End_1yrSOL=ifelseC(EMP.End<(SOL.PAGA.Start), SOL.PAGA.Start, EMP.End),
    
    
    Total_Work_Weeks_1yrSOL=ceiling(as.numeric(difftime(Period1_End_1yrSOL, Period1_Start_1yrSOL, units="weeks"))),
    Total_Pay_Periods_1yrSOL=ceiling(Total_Work_Weeks_1yrSOL/2) )




cen_REMOVAL_STATS %>% glimpse()
cen_REMOVAL_STATS$Total_Pay_Periods_1yrSOL %>% tableRAW()


fwrite.DF.to.csv.as.char(cen_REMOVAL_STATS,
                         file.path(WD.Compiled.Data,
                                   paste(Client.Matter.Info,
                                         "Data used for Removal Analysis Figures.csv")))


cen_REMOVAL_STATS_Class_Totals<-cen_REMOVAL_STATS %>% 
  group_by(Time.Type, Pay.Frequency, Pay.Rate.Type) %>% 
  summarize(EEID_Count=n_distinct(EEID),
            Avg_Pay_Rate=round(mean(Pay.Rate), 2),
            
            Full_4yr_SOL_PP_Count=sum(Total_Pay_Periods),
            
            Meal.Penalty_1_Violation_per_PP=sum(round(Total_Pay_Periods * Avg_Pay_Rate)),
            Rest.Penalty_1_Violation_per_PP=sum(round(Total_Pay_Periods * Avg_Pay_Rate)),
            OT.30min_Per_WW=sum(round(Total_Work_Weeks * (Avg_Pay_Rate*1.5) * 0.5)),
            
            
            
            Emps_Termed_within_3yr_SOL=sum(EMP.Termed.3yr.SOL, na.rm=TRUE),
            Waiting_Time_Penalty_assume_8hr_Shift=sum(ifelse(EMP.Termed.3yr.SOL==TRUE,
                                                             round(Avg_Pay_Rate * 30 * 8),
                                                             NA), na.rm=TRUE),
            
            Wage_Statement_Penalty__50.100.4000cap=sum(
              ifelse(
                case_when( Total_Pay_Periods_1yrSOL==0 ~ 0,
                           Total_Pay_Periods_1yrSOL==1 ~ 50,
                           Total_Pay_Periods_1yrSOL>1 ~ (Total_Pay_Periods_1yrSOL*100)-50) >4000,
              4000,
              case_when( Total_Pay_Periods_1yrSOL==0 ~ 0,
                         Total_Pay_Periods_1yrSOL==1 ~ 50,
                         Total_Pay_Periods_1yrSOL>1 ~ (Total_Pay_Periods_1yrSOL*100)-50)) ),
            
            
            EEIDs_within_PAGA=n_distinct(ifelse(Within.1yr.SOL==TRUE, EEID, NA), na.rm=TRUE),
            PAGA_Period_PP_Count=sum(Total_Pay_Periods_1yrSOL),
            PAGA_Meal_100_per_PP=sum(Total_Pay_Periods_1yrSOL*100),
            PAGA_Rest_100_per_PP=sum(Total_Pay_Periods_1yrSOL*100),
            PAGA_WageStatement_100_per_PP=sum(Total_Pay_Periods_1yrSOL*100) )


cen_REMOVAL_STATS_Class_Totals %>% glimpse()


## 3x violation rate (~30%) -- totals out to 5.4m
(cen_REMOVAL_STATS_Class_Totals$Meal.Penalty_1_Violation_per_PP*3 + 
cen_REMOVAL_STATS_Class_Totals$Rest.Penalty_1_Violation_per_PP*3 + 
cen_REMOVAL_STATS_Class_Totals$Waiting_Time_Penalty_assume_8hr_Shift + 
cen_REMOVAL_STATS_Class_Totals$Wage_Statement_Penalty__50.100.4000cap)


## 2 violation rate (~20%) + attorneys fees of 25% -- totals out to
(cen_REMOVAL_STATS_Class_Totals$Meal.Penalty_1_Violation_per_PP*2 + 
cen_REMOVAL_STATS_Class_Totals$Rest.Penalty_1_Violation_per_PP*2 + 
cen_REMOVAL_STATS_Class_Totals$Waiting_Time_Penalty_assume_8hr_Shift + 
cen_REMOVAL_STATS_Class_Totals$Wage_Statement_Penalty__50.100.4000cap) * 1.25

## 2 violation rate (~20%) + attorneys fees of 25% -- totals out to $7.39m
(cen_REMOVAL_STATS_Class_Totals$Meal.Penalty_1_Violation_per_PP*2 + 
cen_REMOVAL_STATS_Class_Totals$Rest.Penalty_1_Violation_per_PP*2 + 
cen_REMOVAL_STATS_Class_Totals$OT.30min_Per_WW + 
cen_REMOVAL_STATS_Class_Totals$Waiting_Time_Penalty_assume_8hr_Shift + 
cen_REMOVAL_STATS_Class_Totals$Wage_Statement_Penalty__50.100.4000cap) * 1.25

## 1 violation rate (~10%) + attorneys fees of 25% -- totals out to $5.73m
(cen_REMOVAL_STATS_Class_Totals$Meal.Penalty_1_Violation_per_PP*1 + 
cen_REMOVAL_STATS_Class_Totals$Rest.Penalty_1_Violation_per_PP*1 + 
cen_REMOVAL_STATS_Class_Totals$OT.30min_Per_WW + 
cen_REMOVAL_STATS_Class_Totals$Waiting_Time_Penalty_assume_8hr_Shift + 
cen_REMOVAL_STATS_Class_Totals$Wage_Statement_Penalty__50.100.4000cap) * 1.25


  
  
cat("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n See the 'TODOs'")
stop()

















# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# * ----------------------------------------------------------------------------------------------------
# Timecard Analysis ------------------------------------------------------------------------------------




# ** TC MODEL -------------------------------------------------------------------------------------------

stop(paste("Are you sure you want to run the following TC.Analysis.Mode?", Case.Specific.TC.Analysis.Model))

if(!(Case.Specific.TC.Analysis.Model %in% list.files(WD.Code) )){
  
  file.copy(M.Drive.TC.Analysis.Model, #Base model selected earlier!
            #This should be a copy of model off the M-drive now copied to the local drive
            file.path(WD.Code, Case.Specific.TC.Analysis.Model), #The case specific model saved locally
            overwrite = F)  
  ## NOTE: This is false because if the file doesn't exist AND it requires overwrite then we want it to break!!
}

{
  ##TODO: Change this to pull the model code off github, and save it locally.
  
  # Source the local copy of the file pulled off the M-drive
  source(file.path(WD.Code, Case.Specific.TC.Analysis.Model)) #Source the local model
  #THEN...
  # Copy the TC model script into the version folder
  file.copy(file.path(WD.Code, Case.Specific.TC.Analysis.Model), 
            file.path(WD.Analysis.Version, paste("Copy of", Case.Specific.TC.Analysis.Model)),
            overwrite = T)
  }


tc %>% dim()

# *** TC Extra --------------------------------------------------------------------------------------------

# *** TC EXPORT --------------------------------------------------------------------------------------------

tc %>% 
  rename_at_EXPORT() %>%
  fwrite.DF.to.csv.as.char(.,
                           file.path(WD.Compiled.Data.tc, Export.File.TC.CSV.format))

# *** ------------------------------------------------------------------------------------------------------















# ** SH MODEL -----------------------------------------------------------------------------------------------

stop(paste("Are you sure you want to run the following SH.Analysis.Mode?", Case.Specific.SH.Analysis.Model))

if(!(Case.Specific.SH.Analysis.Model %in% list.files(WD.Code) )){
  
  file.copy(M.Drive.SH.Analysis.Model, #Base model selected earlier!
            #This should be a copy of model off the M-drive now copied to the local drive
            file.path(WD.Code, Case.Specific.SH.Analysis.Model), #The case specific model saved locally
            overwrite = F)  
  ## NOTE: This is false because if the file doesn't exist AND it requires overwrite then we want it to break!!
}

{
  ##TODO: Change this to pull the model code off github, and save it locally.
  
  # Source the local copy of the file pulled off the M-drive
  source(file.path(WD.Code, Case.Specific.SH.Analysis.Model)) #Source the local model
  #THEN...
  # Copy the SH model script into the version folder
  file.copy(file.path(WD.Code, Case.Specific.SH.Analysis.Model), 
            file.path(WD.Analysis.Version, paste("Copy of", Case.Specific.SH.Analysis.Model)),
            overwrite = T)
}



sh %>% dim()


# *** SH + INFO -------------------------------------------------------------------------------------------

# *** SH Extra  --------------------------------------------------------------------------------------------

# *** SH Export  --------------------------------------------------------------------------------------------

sh %>% 
  rename_at_EXPORT() %>% 
  fwrite.DF.to.csv.as.char(.,
                           file.path(WD.Compiled.Data.tc, Export.File.SH.CSV.format ))

# *** ------------------------------------------------------------------------------------------------------













# ** EMPL MODEL -----------------------------------------------------------------------------------------------

stop(paste("Are you sure you want to run the following EMPL.Analysis.Model?", Case.Specific.EMPL.Analysis.Model))

if(!(Case.Specific.EMPL.Analysis.Model %in% list.files(WD.Code) )){
  
  file.copy(M.Drive.EMPL.Analysis.Model, #Base model selected earlier!
            #This should be a copy of model off the M-drive now copied to the local drive
            file.path(WD.Code, Case.Specific.EMPL.Analysis.Model), #The case specific model saved locally
            overwrite = F)  
  ## NOTE: This is false because if the file doesn't exist AND it requires overwrite then we want it to break!!
}

{
  ##TODO: Change this to pull the model code off github, and save it locally.
  
  # Source the local copy of the file pulled off the M-drive
  source(file.path(WD.Code, Case.Specific.EMPL.Analysis.Model)) #Source the local model
  #THEN...
  # Copy the EMPL model script into the version folder
  file.copy(file.path(WD.Code, Case.Specific.EMPL.Analysis.Model), 
            file.path(WD.Analysis.Version, paste("Copy of", Case.Specific.EMPL.Analysis.Model)),
            overwrite = T)
}



glimpse(empl)


# *** EMPL + INFO ----------------------------------------------------------------------------------------------

#tc.INFO<-tc %>% group_by(Employee.ID) #%>% 
  ##This is the place to summarize employee info from timecard and merge it into empl!!
  # summarize(Job.Title=ifelse(uniqueN(Job.Title)>1, "Multiple Titles", Job.Title),
  #           Location=ifelse(uniqueN(Location)>1, "Multiple Locations", Location) )

#empl<-left_join(empl, tc.INFO)



# *** EMPL Extra ----------------------------------------------------------------------------------------------


# *** EMPL Export ------------------------------------------------------------------------------------------

empl %>% 
  rename_at_EXPORT() %>% 
  fwrite.DF.to.csv.as.char(.,
                           file.path(WD.Compiled.Data.tc, Export.File.EMPL.CSV.format ))

empl %>% select(matches("Overall")) %>% distinct() %>% lapply(., round, 2)

# *** ------------------------------------------------------------------------------------------







# * -----------------------------------------------------------------------------------------------
# Payroll Analysis --------------------------------------------------------------------------------
# ***  --------------------------------------------------------------------------------------------
# * -----------------------------------------------------------------------------------------------
# Census Analysis ---------------------------------------------------------------------------------
# *** ---------------------------------------------------------------------------------------------









# * --------------------------------------------------------------------------------------------------------
# * --------------------------------------------------------------------------------------------------------
# * --------------------------------------------------------------------------------------------------------
# * --------------------------------------------------------------------------------------------------------
# * --------------------------------------------------------------------------------------------------------
# * --------------------------------------------------------------------------------------------------------
# Future Goals... ---------------------------------------------------------------------------------------------


# ********** ----------------------------------------------------------------------------------------------
# ********** ----------------------------------------------------------------------------------------------
# Xref.Table!! ------------------------------------------------------------------------------------

n_distinct(tc$EEID)
n_distinct(pr$EEID)
n_distinct(cens$EEID)



Xref.Table<-data.frame(EEID=append(tc$EEID, 
                                   append(pr_RAW$EEID, 
                                          "No Census File Right now."
                                          ## census$EEID
                                          )) ) %>% 
  distinct() %>% 
  mutate_all(as.character) %>% 
  left_join(., tc %>% 
              rename(Employee.Name.tc=Employee.Name) %>% 
              group_by(EEID) %>% 
              mutate(Location.tc=list.collapse(Location, Clean = TRUE),
                     Position.tc=list.collapse(Position, Clean = TRUE) ) %>% 
              ungroup() %>% 
              select(EEID, contains("Employee.Name."), Location.tc, Position.tc) %>% 
              distinct() ) %>% 
  left_join(., pr_RAW %>% 
              rename(Employee.Name.pr=Employee.Name) %>% 
              group_by(EEID) %>% 
              mutate(Location.pr=list.collapse(Location, Clean = TRUE),
                     Position.pr=list.collapse(Job.Title, Clean = TRUE) ) %>% 
              ungroup() %>% 
              select(EEID, contains("Employee.Name."), Location.pr, Position.pr) %>% 
              distinct() ) %>% 
  #left_join(., census %>% 
  #            rename(Employee.Name.cen=Employee.Name) %>% 
  #            select(EEID, contains("Employee.Name.")) %>% 
  #            distinct() ) %>% 
  mutate(Employee.Name=Employee.Name.tc,
         Employee.Name=ifelse(is.na(Employee.Name), Employee.Name.pr, Employee.Name),
         #Employee.Name=ifelse(is.na(Employee.Name), Employee.Name.cen, Employee.Name),
         Employee.Name=str_to_upper(Employee.Name),
         
         Location=Location.tc,
         Location=ifelse(is.na(Location), Location.pr, Location),
         #Location=ifelse(is.na(Location), Location.cen, Location),
         Location=str_to_upper(Location),
         
         Position=Position.tc,
         Position=ifelse(is.na(Position), Position.pr, Position),
         #Position=ifelse(is.na(Position), Position.cen, Position),
         Position=str_to_upper(Position),
         
         
         In.tc=ifelse(EEID %in% unique(tc$EEID), TRUE, FALSE),
         In.pr=ifelse(EEID %in% unique(pr_RAW$EEID), TRUE, FALSE),
         #In.census=ifelse(EEID %in% unique(census$EEID), TRUE, FALSE),
         #In.How.Many=In.tc+In.pr+In.census,
         #In.ALL=ifelse(In.tc==TRUE & In.pr==TRUE & In.census==TRUE, TRUE, FALSE),
         
         ## Missing From One
         #Missing.From="ERROR",
         #Missing.From=ifelse(In.tc==F & In.pr==T & In.census==T, "1. Timecard", Missing.From),
         #Missing.From=ifelse(In.tc==T & In.pr==F & In.census==T, "1. Payroll", Missing.From),
         #Missing.From=ifelse(In.tc==T & In.pr==T & In.census==F, "1. Census", Missing.From),
         #
         #Missing.From=ifelse(In.tc==F & In.pr==F & In.census==T, "2. Timecard & Payroll", Missing.From),
         #Missing.From=ifelse(In.tc==F & In.pr==T & In.census==F, "2. Timecard & Census", Missing.From),
         #Missing.From=ifelse(In.tc==T & In.pr==F & In.census==F, "2. Payroll & Census", Missing.From),
         #
         #Missing.From=ifelse(In.tc==F & In.pr==F & In.census==F, "0. NONE", Missing.From),
         #Missing.From=ifelse(In.tc==T & In.pr==T & In.census==T, "0. ALL", Missing.From), 
         #Missing.From=factor(Missing.From)
         ) %>% 
  select(EEID, Employee.Name, Location, Position, starts_with("In."), everything())


Xref.Table %>% 
  fwrite.DF.to.csv.as.char(.,
                           file.path(WD.Compiled.Data,
                                     paste(Client.Matter.Info.AND.Version.Number,
                                           "INITIAL Xref Table (tc & pr) v0.csv")))


## View(Xref.Table)

