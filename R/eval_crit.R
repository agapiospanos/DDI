#' Evaluates the imported patients' data for the selected DDI criteria at once.
#'
#' @author
#' Agapios Panos <panosagapios@gmail.com>
#'
#' @param selected (Character vector) the selected criteria to export given in the form of c("DDI1, "DDI6")
#' @param excel_path (Character) (optional) (default: NULL) the path that the excel file can be read from. If NULL a file choose window will be displayed so that you can choose the excel file.
#' @param exclude (Character) (optional) (default: NULL) a vector of criteria that you want to exclude. Example: c("DDI1, "DDI6").
#' @param excel_out (Boolean) (optional) (default: TRUE) output excel file with the evaluated data.
#' @param single_excel (Boolean) (optional) (default: TRUE) if true outputs only 1 excel file with multiple columns instead of multiple files (one for each criterion)
#' @param export_data_path (Character) (optional) (default: NULL (a popup message to choose dir will be displayed)) the path for excel file output.
#' @param suppressNA (Boolean) (optional) (default: TRUE) set this to FALSE if you want to know for which patients have NAs and for which variable. By default all NAs will be ignored so that the algorithm can distinguish between patients who meet the criterion and those who do not.
#' @param excel_sheet (Character) (optional) (default: 1) the number of excel worksheet that the patient id and the ATC codes are stored.
#' @param excel_col_data (Character) (optional) (default: 'med_gen__decod') the column name of the column that stores the ATC codes data in the provided excel file.
#' @param excel_col_pid (Character) (optional) (default: 'usubjid') the column name that specifies the patient id in the excel you provided for the data.
#' @param show_only_meet (Boolean) (optional) (default: FALSE) set to TRUE if you want to have exported in the excel only the patients that meet the conditions for this criterion
#'
#' @importFrom writexl write_xlsx
#' @importFrom stats setNames
#'
#' @export

eval_crit <- function(selected, excel_path = NULL, exclude = NULL, excel_out = TRUE, single_excel = TRUE, export_data_path = NULL, suppressNA = TRUE, excel_sheet = 1, excel_col_data = 'med_gen__decod', excel_col_pid = 'usubjid', show_only_meet = FALSE) {

    # checking the excel_path. If NULL a file choose message will be displayed.
    excel_path <- chk_file(excel_path)
    cat('You selected the following excel file: ', excel_path)

    # choosing an export path for the excel file that contains the evaluated patients' data.
    export_data_path <- choose_export_path(export_data_path)

    # importing the patients' data from the excel file
    data <- import_excel_data(path = excel_path, worksheet = excel_sheet, var_col = excel_col_data, include_missing = suppressNA, ignore_na = suppressNA, excel_col_pid = excel_col_pid )
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
        boolean1 <- bool_exp[1]
        boolean2 <- bool_exp[2]
        concomitant <- as.logical(bool_exp[3])

        # iteration over all patients
        for (i in 1:length(pdata)) {

            # getting patient id
            pid <- names(sapply(pdata[i], names))

            # checking if the patient id is in the list of missing data
            if (is.na(match( pid, names(sapply(missing_data_patients, names))))){

                # if not concomitant we check both the atc codes against both the boolean1 and boolean2 expressions
                if (!concomitant) {

                    # checking if against boolean1 and boolean2
                    if ( any(grepl(boolean1, unlist(pdata[[i]][1]), ignore.case = T)) &
                         any(grepl(boolean2, unlist(pdata[[i]][1]), ignore.case = T))
                    ) {
                        # inserting the record to the data.frame evaluated_patients
                        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 1, missing_variables = ''))
                    } else {
                        # inserting the record to the data.frame evaluated_patients
                        evaluated_patients <- rbind(evaluated_patients, data.frame(patients = pid, status = 0, missing_variables = ''))
                    }

                } else {
                    # in the concomitant case we check if patient receives at least 2 of the ATC codes of boolean1 expression
                    if (length(which(grepl(boolean1, unlist(pdata[[i]][1]), ignore.case = T))) > 1) {
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

        if (excel_out) {
            if (single_excel) {
                if (j == 1) {
                    all_criteria <- names(pdata)
                }

                # keeping the results to data.frames
                all_criteria_colnames <- cbind(all_criteria_colnames, crit)
                all_criteria <- cbind(all_criteria, as.data.frame(evaluated_patients)$status)
            } else {
                # export the evaluated list of patients to excel file
                write_xlsx(evaluated_patients, path = paste0( export_data_path, '/', crit, '.xlsx'), col_names = TRUE)
            }
        }
    }

    if (single_excel & excel_out) {
        # generate the single excel file containing all criteria
        all_criteria <- as.data.frame(all_criteria)
        all_criteria <- setNames(all_criteria, all_criteria_colnames)
        write_xlsx(all_criteria, path = paste0( export_data_path, '/DDI_critetia.xlsx'), col_names = TRUE)
        cat('The following excel file has been exported: ', export_data_path, '\\', 'DDI_criteria.xlsx')
    }

    # return evaluated data
    invisible (list(all_criteria)) # instead of return as we do not want to be printed
}
