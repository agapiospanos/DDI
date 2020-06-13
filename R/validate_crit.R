#' Validation for the selected and exclude argument for the names of criteria specified
#'
#' @param selected a vector of the selected criteria for which you want to perform the patient data evaluation.
#' @param exclude a vector of the criteria that you want to exclude from the evaluation.
#'
#'

validate_crit <- function(selected, exclude) {

    # valid criteria
    valid_crit <- c('all', 'ALL')

    # adding the rest valid criteria DDI1 - DDI69
    for (i in 1:69) {
        valid_crit[i + 2] <- paste0('DDI',i)
    }

    # checking if selected is not defined
    if (is.null(selected) | is.na(selected)) {
        stop('You have to define the selected argument. The following criteria are valid for this argument.')
    }

    # getting any possibly not valid criteria in selected argument
    not_valid <- selected[which(selected %in% valid_crit == F)]
    if (length(not_valid) > 0) {
        stop('The following criteria which are defined in the selected argument are not valid ', paste(not_valid, sep = ', ') , '. Here is a list of all the valid criteria you can specify in the selected argument: ', paste(valid_crit, sep = ', '))
    }

    # valid criteria for exclude argument (basically all criteria except from 'all' and 'ALL')
    valid_excl <- valid_crit[3:length(valid_crit)]

    # getting any possibly not valid criteria in exclude argument
    not_valid_excl <- exclude[which(exclude %in% valid_crit)]
    if (length(not_valid_excl) > 0) {
        stop('The following criteria which are defined in the exclude argument are not valid', not_valid_excl, 'Here is a list of all the valid criteria you can specify in the exclude argument', valid_excl)
    }

    # removing excluded from selected criteria in case all are selected
    if (selected == 'all' | selected == 'ALL') {

        # setting to selected all the criteria except 'all' and 'ALL'
        selected <- valid_crit[3:length(valid_crit)]

        # removing excluded criteria from the selected
        selected <- setdiff(selected, exclude)
    }

    invisible(list(selected))
}
