#' Evaluates the imported patients' data for the selected DDI criteria at once.
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @param selected (Character vector) (optional) (default: 'all') the selected criteria to export given in the form of c("DDI1, "DDI6"). Valid options 'all' and DDI1 to DDI68.
#' @param excel_path (Character) (optional) (default: NULL) the path that the excel file can be read from. If NULL a file choose window will be displayed so that you can choose the excel file.
#' @param exclude (Character) (optional) (default: NULL) a vector of criteria that you want to exclude. Example: c("DDI1, "DDI6"). Valid options DDI1 to DDI68.
#' @param excel_out (Boolean) (optional) (default: TRUE) output excel file with the evaluated data.
#' @param single_excel (Boolean) (optional) (default: TRUE) if true outputs only 1 excel file with multiple columns instead of multiple files (one for each criterion)
#' @param export_data_path (Character) (optional) (default: NULL (a popup message to choose dir will be displayed)) the path for excel file output.
#' @param suppressNA (Boolean) (optional) (default: TRUE) set this to FALSE if you want to know for which patients have NAs and for which variable. By default all NAs will be ignored so that the algorithm can distinguish between patients who meet the criterion and those who do not.
#' @param excel_sheet (Character) (optional) (default: 1) the number of excel worksheet that the patient id and the ATC codes are stored.
#' @param excel_col_data (Character) (optional) (default: 'med_gen__decod') the column name of the column that stores the ATC codes data in the provided excel file.
#' @param excel_col_pid (Character) (optional) (default: 'usubjid') the column name that specifies the patient id in the excel you provided for the data.
#' @param excel_col_daily_dosage (Character) (optional) (default: 'daily_dosage') the column name that specifies the daily dosage so that some DDI can take into account the dosage.
#' @param excel_col_med_unit (Character) (optional) (default: med_unit) the column name that specifies the medicine unit (e.g. mg or g) that is used to calculate the daily dosage.
#' @param show_only_meet (Boolean) (optional) (default: FALSE) set to TRUE if you want to have exported in the excel only the patients that meet the conditions for this criterion
#' @param show_only_sum (Boolean) (optional) (default: FALSE) set to TRUE if you want to show only the number of number of criteria that are met for each patient and a boolean value of 0 if no criterion is met and 1 if at least one is met.
#'
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#'
#' @export

eval_crit <- function(selected = 'all', excel_path = NULL, exclude = NULL, excel_out = TRUE, single_excel = TRUE, export_data_path = NULL, suppressNA = TRUE, excel_sheet = 1, excel_col_data = 'med_gen__decod', excel_col_pid = 'usubjid', excel_col_daily_dosage = 'daily_dosage', excel_col_med_unit = 'med_unit', show_only_meet = FALSE, show_only_sum = FALSE) {

    # checking the excel_path. If NULL a file choose message will be displayed.
    excel_path <- chk_file(excel_path)
    cat('You selected the following excel file: ', excel_path)

    if (excel_out) {
        # choosing an export path for the excel file that contains the evaluated patients' data.
        export_data_path <- choose_export_path(export_data_path)
    }

    # importing the patients' data from the excel file
    data <- import_excel_data(path = excel_path, worksheet = excel_sheet, var_col = excel_col_data, include_missing = suppressNA, ignore_na = suppressNA, excel_col_pid = excel_col_pid )
    data <- import_excel_data(current_data = data, path = excel_path, worksheet = excel_sheet, var_col = excel_col_daily_dosage, include_missing = TRUE, ignore_na = suppressNA )
    data <- import_excel_data(current_data = data, path = excel_path, worksheet = excel_sheet, var_col = excel_col_med_unit, include_missing = TRUE, ignore_na = suppressNA )

    pdata <- data[[1]]
    missing_data_patients <- data[[2]]

    # call the validate helper function so that the selected and exclude argument contains only valid criteria
    selected <- unlist(validate_crit(selected, exclude))

    # initializing data.frame to keep data for all evaluated criteria
    all_criteria <- data.frame()

    # creating the structure for the data.frame if single_excel output is needed
    all_criteria_colnames <- c('patient')

    # iteration over all selected criteria
    for (j in 1:length(selected)) {

        # the variable to keep the final dataframe of patients:
        # 0 marks the patient that does not fulfill the criterion if the user decided to keep the argument show_only_meet = FALSE
        # 1 marks the patient that fulfills the criterion and
        # 2 marks the patient with missing data if the user decides to not suppress the NAs
        evaluated_patients <- data.frame(patients = character(0), status = numeric(0), missing_variables = character(0))

        # get the selected criterion
        crit <- selected[j]

        # get all boolean expressions based on the chosen criterion
        bool_exp <- get_bool_exp(crit)

        # get boolean expresions and concomitant
        boolean1 <- unlist(bool_exp[[1]])
        boolean2 <- unlist(bool_exp[[2]])
        concomitant <- unlist(as.logical(bool_exp[[3]]))
        drugs_amount <- unlist(as.numeric(bool_exp[[4]]))
        # we have the daily dosage check as a list ATC_CODE[[1]] AMOUNT[[2]] CHECK[[3]]
        daily_dosage_check <- unlist(bool_exp[[5]])

        # iteration over all patients
        for (i in 1:length(pdata)) {

            # getting patient id
            pid <- names(sapply(pdata[i], names))

            # checking if the patient id is in the list of missing data
            if (is.na(match( pid, names(sapply(missing_data_patients, names))))){

                # we get the unique pdata because an ATC code may appear more than once, thus generating false positives.
                unique_patient_atcs <- unique(unlist(pdata[[i]][1]))

                # if not concomitant we check both the atc codes against both the boolean1 and boolean2 expressions
                if (!concomitant) {

                    # checking if against boolean1 and boolean2
                    if ( any(grepl(boolean1, unique_patient_atcs, ignore.case = T)) &
                         any(grepl(boolean2, unique_patient_atcs, ignore.case = T))
                    ) {
                        # inserting the record to the data.frame evaluated_patients
                        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 1, missing_variables = ''))
                    } else {
                        # inserting the record to the data.frame evaluated_patients
                        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))
                    }

                } else {
                    # in the concomitant case we check if patient receives at least drugs amount (e.g. 2) of the ATC codes of boolean1 expression

                    # we assume that dosage problem always exist so that for DDI that we do not take into account dosage, there will be no effect in the evaluation.
                    dosage_problem <- TRUE

                    if (!is.na(daily_dosage_check[[1]])) {
                        # we have to check for all the daily dosages...
                        for (k in 1:length(daily_dosage_check[[1]])) {

                            atc_code <- daily_dosage_check[[1]][k]
                            thresold <- as.numeric(daily_dosage_check[[2]][k])
                            unit <- daily_dosage_check[[3]][k]
                            comparison <- daily_dosage_check[[4]][k]

                            # we do not use the unique patient atcs as we may want to check multiple dosages
                            patient_atcs <- unlist(pdata[[i]][1])
                            patient_daily_dosages <- unlist(pdata[[i]][2])
                            patient_med_units <- unlist(pdata[[i]][3])

                            index <- grep(atc_code, patient_atcs, ignore.case = T)
                            if (length(index) > 0) {

                                # checking if we compare using the same units
                                if (any(patient_med_units[index] != unit)) {
                                    warning(paste('Patient', pid, 'has different medicine units than', unit, '. Please change the unit to', unit, '. If you do not change the unit the daily dosage will not be taken into account'))
                                } else {
                                    # first we remove any NA values
                                    patient_daily_dosages <- patient_daily_dosages[!is.na(patient_daily_dosages)]
                                    if (comparison == 'greater') {
                                        # we have the opposite operator in the comparison
                                        if (any(patient_daily_dosages <= thresold)) {
                                            dosage_problem <- FALSE
                                        }
                                    } else if (comparison == 'lower') {
                                        # we have the opposite operator in the comparison
                                        if (any(patient_daily_dosages >= thresold)) {
                                            dosage_problem <- FALSE
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if (length(which(grepl(boolean1, unique_patient_atcs, ignore.case = T))) >= drugs_amount & dosage_problem) {
                        # inserting the record to the data.frame evaluated_patients
                        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 1, missing_variables = ''))
                    } else {
                        # inserting the record to the data.frame evaluated_patients
                        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))
                    }
                }

            } else { # patient has missing data
                # inserting the record to the data.frame evaluated_patients
                evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 2, missing_variables = paste(missing_data_patients[[pid]], collapse = ', ')))
            }
        }

        # storing the results
        fulfill_count <- length(which(evaluated_patients$status == 1))
        total_count <- fulfill_count + length(which(evaluated_patients$status == 0))
        missing_count <- length(which(evaluated_patients$status == 2))

        # printing results to the console
        if (suppressNA) {
            cat(crit,': ', fulfill_count, 'patients out of', total_count + missing_count, 'patients meet the conditions for the criterion.\n')
        } else {
            cat(crit,': ', fulfill_count, 'patients out of', total_count, 'patients meet the conditions for the criterion.', missing_count, 'patients have missing data. \n')
        }

        if (j == 1) {
            all_criteria <- names(pdata)
        }

        # keeping the results to data.frames
        all_criteria_colnames <- cbind(all_criteria_colnames, crit)
        all_criteria <- cbind(all_criteria, as.data.frame(evaluated_patients)$status)


        if (excel_out & !single_excel) {
            # export the evaluated list of patients to excel file
            write_xlsx(evaluated_patients, path = paste0( export_data_path, '/', crit, '.xlsx'), col_names = TRUE)
        }
    }

    all_criteria <- as.data.frame(all_criteria)
    all_criteria <- setNames(all_criteria, all_criteria_colnames)

    ddi.found <- ddi.count <- c()
    for (i in 1:nrow(all_criteria)) {
        ddi.count[i] <- sum(all_criteria[unlist(selected)][i,] == 1)

        if (ddi.count[i] > 0) {
            ddi.found[i] <- 1
        } else {
            ddi.found[i] <- 0
        }
    }

    sumdata <- data.frame(all_criteria[,1], ddi.count, ddi.found)
    names(sumdata)[1] <- 'patient'

    if (single_excel & excel_out) {
        # generate the single excel file containing all criteria
        if (show_only_sum) {
            write_xlsx(sumdata, path = paste0( export_data_path, '/DDI_critetia_sum.xlsx'), col_names = TRUE)
            cat('The following excel file has been exported: ', export_data_path, '/', 'DDI_criteria.xlsx')
        } else {
            write_xlsx(all_criteria, path = paste0( export_data_path, '/DDI_critetia.xlsx'), col_names = TRUE)
            cat('The following excel file has been exported: ', export_data_path, '/', 'DDI_criteria.xlsx')
        }
    }

    # return evaluated data
    invisible (list(all_criteria = all_criteria, sumdata = sumdata)) # instead of return as we do not want to be printed
}
