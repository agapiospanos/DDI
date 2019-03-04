#' Helper function to get the correspoding to that criterion boolean expressions for the grepl function
#'
#' @param criterion (Character) The specific criterion for which you need the boolean expressions.
#'
#' @return Returns a list of 2 boolean expressions to pass as arguments to the grepl function.

get_bool_exp <- function(criterion) {

    concomitant <- FALSE
    bool1 <- bool2 <- NULL

    switch(criterion,
           DDI1 = {
               bool1 <- 'C01AA05'
               bool2 <- 'C01BD01'
           },
           DDI2_3 = {
               bool1 <- 'C01AA05'
               bool2 <- 'C08DA01|C09BB10|C08DA51|C08DB01|C05AE03'
           },
           DDI6 = {
               bool1 <- 'C01AA05'
               bool2 <- 'J01FA01|S01AA17|D10AF02|D10AF52|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA10|S01AA26|J01RA07|J01FA15'
           },
           DDI12 = {
               bool1 <- '^B01AA|^B01AE|^B01AF'
               bool2 <- '^M01A|^N02BA'
           },
           DDI16 = {
               bool1 <- '^B01AA'
               bool2 <- '^J01FA|S01AA17|D10AF02|D10AF52|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|S01AA26|J01RA07|J01RA04'
           },
           DDI24 = {
               bool1 <- 'C03DB01|C03DB02|C03DA04|C03DA01|^C03E|^C09|^M01A|^J01E'
               bool2 <- NA
               concomitant <- TRUE
           }
           )

    invisible(c(bool1, bool2, concomitant))
}
