#' IST Dataset
#'
#' The International Stroke Trial Dataset
#'
#' Obtained from
#' Sandercock, Peter; Niewada, Maciej; Czlonkowska, Anna. (2011).
#' International Stroke Trial database (version 2), \[dataset\].
#' University of Edinburgh. Department of Clinical Neurosciences.
#' \doi{https://doi.org/10.7488/ds/104}
#' Under ODC-by licence
#'
#' @format A data frame with 19435 rows and 112 columns:
#' \describe{
#'   \item{AGE}{Randomisation data: Age in years}
#'   \item{CMPLASP}{Other data and derived variables: Compliant for aspirin}
#'   \item{CMPLHEP}{Other data and derived variables: Compliant for heparin}
#'   \item{CNTRYNUM}{Other data and derived variables: Country code}
#'   \item{COUNTRY}{Other data and derived variables: Abbreviated country code}
#'   \item{DALIVE}{Recurrent stroke within 14 days:
#'   Discharged alive from hospital}
#'   \item{DALIVED}{Recurrent stroke within 14 days:
#'   Date Discharged alive from hospital}
#'   \item{DAP}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Non trial antiplatelet drug (Y/N)}
#'   \item{DASP14}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Aspirin given for 14 days
#'   or till death or discharge (Y/N)}
#'   \item{DASPLT}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Discharged on long term aspirin (Y/N)}
#'   \item{DAYLOCAL}{Randomisation data: Estimate of local day of week
#'   (assuming RDATE is Oxford)}
#'   \item{DCAA}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Calcium antagonists (Y/N)}
#'   \item{DCAREND}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Carotid surgery (Y/N)}
#'   \item{DDEAD}{Other events within 14 days:
#'   Other events within 14 days: Dead on discharge form}
#'   \item{DDEADC}{Other events within 14 days: Cause of death
#'   (1-Initial stroke/2-Recurrent stroke (ischaemic or unknown
#'   /3-Recurrent stroke (haemorrhagic)/4-Pneumonia
#'   /5-Coronary heart disease/6-Pulmonary embolism
#'   /7-Other vascular or unknown/8-Non-vascular/0-unknown)}
#'   \item{DDEADD}{Date of dead on discharge form (yyyy/mm/dd);
#'   NOTE: this death is not necessarily within 14 days of randomisation}
#'   \item{DDEADX}{Other events within 14 days: Comment on death}
#'   \item{DDIAGHA}{Final diagnosis of initial event: Haemorrhagic stroke}
#'   \item{DDIAGISC}{Final diagnosis of initial event: Ischaemic stroke}
#'   \item{DDIAGUN}{Final diagnosis of initial event: Indeterminate stroke}
#'   \item{DEAD1}{Indicator variables for specific causes of death:
#'   Initial stroke}
#'   \item{DEAD2}{Indicator variables for specific causes of death:
#'   Reccurent ischaemic/unknown stroke}
#'   \item{DEAD3}{Indicator variables for specific causes of death:
#'   Reccurent haemorrhagic stroke}
#'   \item{DEAD4}{Indicator variables for specific causes of death:
#'   Pneumonia}
#'   \item{DEAD5}{Indicator variables for specific causes of death:
#'   Coronary heart disease}
#'   \item{DEAD6}{Indicator variables for specific causes of death:
#'   Pulmonary embolism}
#'   \item{DEAD7}{Indicator variables for specific causes of death:
#'   Other vascular or unknown}
#'   \item{DEAD8}{Indicator variables for specific causes of death:
#'   Non vascular}
#'   \item{DGORM}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Glycerol or manitol (Y/N)}
#'   \item{DHAEMD}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Haemodilution (Y/N)}
#'   \item{DHH14}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Medium dose heparin given for 14 days
#'   etc in pilot (combine with above)}
#'   \item{DIED}{Other data and derived variables: Indicator variable for death
#'   (1=died; 0=did not die)}
#'   \item{DIVH}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Non trial intravenous heparin (Y/N)}
#'   \item{DLH14}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Low dose heparin given for 14 days
#'   or till death/discharge (Y/N)}
#'   \item{DMAJNCH}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Major non-cerebral haemorrhage (Y/N)}
#'   \item{DMAJNCHD}{Data collected on 14 day/discharge form
#'   about treatments given in hospital:}
#'   \item{DMAJNCHX}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: }
#'   \item{DMH14}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Date of Major non-cerebral haemorrhage
#'   (yyyy/mm/dd)}
#'   \item{DNOSTRK}{Final diagnosis of initial event: Not a stroke}
#'   \item{DNOSTRKX}{Final diagnosis of initial event: Comment on Not a stroke}
#'   \item{DOAC}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Other anticoagulants (Y/N)}
#'   \item{DPE}{Other events within 14 days: Pulmonary embolism}
#'   \item{DPED}{Other events within 14 days:
#'   Date of Pulmonary embolism (yyyy/mm/dd)}
#'   \item{DPLACE}{Other events within 14 days: Discharge destination
#'   (A-Home
#'   /B-Relatives home
#'   /C-Residential care
#'   /D-Nursing home
#'   /E-Other hospital departments
#'   /U-Unknown)}
#'   \item{DRSH}{Recurrent stroke within 14 days: Haemorrhagic stroke}
#'   \item{DRSHD}{Recurrent stroke within 14 days:
#'   Date of Haemorrhagic stroke (yyyy/mm/dd)}
#'   \item{DRSISC}{Recurrent stroke within 14 days: Ischaemic recurrent stroke}
#'   \item{DRSISCD}{Recurrent stroke within 14 days:
#'   Date of Ischaemic recurrent stroke (yyyy/mm/dd)}
#'   \item{DRSUNK}{Recurrent stroke within 14 days: Unknown type}
#'   \item{DRSUNKD}{Recurrent stroke within 14 days:
#'   Date of Unknown type (yyyy/mm/dd)}
#'   \item{DSCH}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Non trial subcutaneous heparin (Y/N)}
#'   \item{DSIDE}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Other side effect (Y/N)}
#'   \item{DSIDED}{Data collected on 14 day/discharge
#'   form about treatments given in hospital: Date of Other side effect}
#'   \item{DSIDEX}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Comment of Other side effect}
#'   \item{DSTER}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Steroids (Y/N)}
#'   \item{DTHROMB}{Data collected on 14 day/discharge form
#'   about treatments given in hospital: Thrombolysis (Y/N)}
#'   \item{DVT14}{Indicator variables for specific causes of death:
#'   Indicator of deep vein thrombosis on discharge form}
#'   \item{EXPD14}{Other data and derived variables:
#'   Predicted probability of death at 14 days}
#'   \item{EXPD6}{Other data and derived variables:
#'   Predicted probability of death at 6 month}
#'   \item{EXPDD}{Other data and derived variables:
#'   Predicted probability of death/dependence at 6 month}
#'   \item{FAP}{Data collected at 6 months: On antiplatelet drugs}
#'   \item{FDEAD}{Data collected at 6 months: Dead at six month follow-up (Y/N)}
#'   \item{FDEADC}{Data collected at 6 months: Cause of death (1-Initial stroke
#'   /2-Recurrent stroke (ischaemic or unknown)
#'   /3-Recurrent stroke (haemorrhagic)
#'   /4-Pneumonia
#'   /5-Coronary heart disease
#'   /6-Pulmonary embolism
#'   /7-Other vascular or unknown
#'   /8-Non-vascular
#'   /0-unknown)}
#'   \item{FDEADD}{Data collected at 6 months: Date of death;
#'   NOTE: this death is not necessarily within 6 months of randomisation}
#'   \item{FDEADX}{Data collected at 6 months: Comment on death}
#'   \item{FDENNIS}{Data collected at 6 months:
#'   Dependent at 6 month follow-up (Y/N)}
#'   \item{FLASTD}{Data collected at 6 months: Date of last contact}
#'   \item{FOAC}{Data collected at 6 months: On anticoagulants}
#'   \item{FPLACE}{Data collected at 6 months:
#'   Place of residance at 6 month follow-up (
#'   A-Home
#'   /B-Relatives home
#'   /C-Residential care
#'   /D-Nursing home
#'   /E-Other hospital departments
#'   /U-Unknown)}
#'   \item{FRECOVER}{Data collected at 6 months:
#'   Fully recovered at 6 month follow-up (Y/N)}
#'   \item{FU1_COMP}{Other data and derived variables:
#'   Date discharge form completed}
#'   \item{FU1_RECD}{Other data and derived variables:
#'   Date discharge form received}
#'   \item{FU2_DONE}{Other data and derived variables:
#'   Date 6 month follow-up done}
#'   \item{H14}{ndicator variables for specific causes of death:
#'   Cerebral bleed/heamorrhagic stroke within 14 days;
#'   this is slightly wider definition than DRSH an is used for analysis of
#'   cerebral bleeds}
#'   \item{HOSPNUM}{Randomisation data: Hospital number}
#'   \item{HOURLOCAL}{Randomisation data: Local time – hours}
#'   \item{HTI14}{Indicator variables for specific causes of death:
#'   Indicator of haemorrhagic transformation within 14 days}
#'   \item{ID14}{Other data and derived variables:
#'   Indicator of death at 14 days}
#'   \item{ISC14}{Indicator variables for specific causes of death:
#'   Indicator of ischaemic stroke within 14 days}
#'   \item{MINLOCAL}{Randomisation data: Local time – minutes}
#'   \item{NCB14}{Indicator variables for specific causes of death:
#'   Indicator of any non-cerebral bleed within 14 days}
#'   \item{NCCODE}{Other data and derived variables:
#'   Coding of compliance
#'   (see Table 3) \doi{10.1186/1745-6215-13-24}}
#'   \item{NK14}{Indicator variables for specific causes of death:
#'   Indicator of indeterminate stroke within 14 days}
#'   \item{OCCODE}{Other data and derived variables:
#'   Six month outcome (
#'   1-dead
#'   /2-dependent
#'   /3-not recovered
#'   /4-recovered
#'   /8 or 9 – missing status}
#'   \item{ONDRUG}{Data collected on 14 day/discharge form
#'   about treatments given in hospital:
#'   Estimate of time in days on trial treatment}
#'   \item{PE14}{Indicator variables for specific causes of death:
#'   Indicator of pulmonary embolism within 14 days}
#'   \item{RASP3}{Randomisation data:
#'   Aspirin within 3 days prior to randomisation (Y/N)}
#'   \item{RATRIAL}{Randomisation data:
#'   Atrial fibrillation (Y/N); not coded for pilot phase - 984 patients}
#'   \item{RCONSC}{Randomisation data:
#'   Conscious state at randomisation
#'   (F - fully alert, D - drowsy, U - unconscious)}
#'   \item{RCT}{Randomisation data:
#'   CT before randomisation (Y/N)}
#'   \item{RDATE}{Randomisation data:
#'   Date of randomisation}
#'   \item{RDEF1}{Randomisation data:
#'   Face deficit (Y/N/C=can't assess)}
#'   \item{RDEF2}{Randomisation data:
#'   Arm/hand deficit (Y/N/C=can't assess)}
#'   \item{RDEF3}{Randomisation data:
#'   Leg/foot deficit (Y/N/C=can't assess)}
#'   \item{RDEF4}{Randomisation data:
#'   Dysphasia (Y/N/C=can't assess)}
#'   \item{RDEF5}{Randomisation data:
#'   Hemianopia (Y/N/C=can't assess)}
#'   \item{RDEF6}{Randomisation data:
#'   Visuospatial disorder (Y/N/C=can't assess)}
#'   \item{RDEF7}{Randomisation data:
#'   Brainstem/cerebellar signs (Y/N/C=can't assess)}
#'   \item{RDEF8}{Randomisation data:
#'   Other deficit (Y/N/C=can't assess)}
#'   \item{RDELAY}{Randomisation data:
#'   Delay between stroke and randomisation in hours}
#'   \item{RHEP24}{Randomisation data:
#'   Heparin within 24 hours prior to randomisation (Y/N)}
#'   \item{RSBP}{Randomisation data:
#'   Systolic blood pressure at randomisation (mmHg)}
#'   \item{RSLEEP}{Randomisation data:
#'   Symptoms noted on waking (Y/N)}
#'   \item{RVISINF}{Randomisation data:
#'   Infarct visible on CT (Y/N)}
#'   \item{RXASP}{Randomisation data:
#'   Trial aspirin allocated (Y/N)}
#'   \item{RXHEP}{Randomisation data:
#'   Trial heparin allocated (M/L/N) \\[M is coded as H=high in pilot\\]}
#'   \item{SET14D}{Other data and derived variables:
#'   Know to be dead or alive at 14 days (1=Yes, 0=No);
#'   this does not necessarily mean that we know outcome at 6 monts
#'   – see OCCODE for this}
#'   \item{SEX}{Randomisation data: M=male; F=female}
#'   \item{STRK14}{Indicator variables for specific causes of death:
#'   Indicator of any stroke within 14 days}
#'   \item{STYPE}{Randomisation data: Stroke subtype (TACS/PACS/POCS/LACS/other}
#'   \item{TD}{Other data and derived variables:
#'   Time of death or censoring in days}
#'   \item{TRAN14}{Indicator variables for specific causes of death:
#'   Indicator of major non-cerebral bleed within 14 days}
#'   ...
#' }
#'
#' @author Sandercock P et al. \email{Peter.Sandercock@@ed.ac.uk}
#' @references \doi{10.7488/ds/104}
"IST"