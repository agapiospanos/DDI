#' Helper function to get the correspoding to that criterion boolean expressions for the grepl function
#'
#' @param criterion (Character) The specific criterion for which you need the boolean expressions.
#'
#' @return Returns a list of 2 boolean expressions to pass as arguments to the grepl function.

get_bool_exp <- function(criterion) {

    concomitant <- FALSE
    bool1 <- bool2 <- NA
    drugs_amount <- NA
    daily_dosage_check <- NA

    CYP3A4_inhibitors <- 'C08DB01|C08DA01|C09BB10|C08DA51|C01EB18|C01BD07|J01FA01|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA06|J01FA15|J02AB02|J02AC02|J02AC01|J02AC05|J01RA07|J02AC04|J02AC03|J05AE03|J05AR23|J05AP52|J05AR10|J05AP53|J05AG02|J05AE02|J05AE04|J05AE01|J05AR15|V03AX03|J05AR14|J05AR22|J05AR18|J05AR09|J05AP02|J05AP03|A02BA01|A02BA51|L01EA03|L01ED01|L01XE07|L01XE01|L01XE11|A04AD12|N05BA03|N06AB08|N06AX06|C03XA02|C10AX12'

    switch(criterion,
           DDI1 = {
               bool1 <- 'C01AA05'
               bool2 <- 'C01BD01'
           },
           DDI2 = {
               bool1 <- 'C01AA05'
               bool2 <- 'C08DA01|C09BB10|C08DA51|C08DB01|C05AE03'
           },
           DDI3 = {
               bool1 <- 'C01AA05'
               bool2 <- 'C01BC03'
           },
           DDI4 = {
               bool1 <- 'C01AA05'
               bool2 <- 'C01BA01|C01BA51|C01BA71'
           },
           DDI5 = {
               bool1 <- 'C01AA05'
               bool2 <- 'J01FA01|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA06|J01FA10|J01RA07|J01FA15'
           },
           DDI6 = {
               bool1 <- 'C01AA05'
               bool2 <- '^C03A|^C03B|^C03C|^C07B|^C07C|^C07D|^C08G|^C09BA|C09BX01|C09BX03|^C09DA|C09DX01|C09DX03|C09DX06|C09XA52|C09XA54|C10BX13'
           },
           DDI7 = {
               bool1 <- '^B01AA'
               bool2 <- '^C10AB|C10BA03|C10BA04'
           },
           DDI8 = {
               bool1 <- '^B01AA'
               bool2 <- 'A02BA01|A02BA51'
           },
           DDI9 = {
               bool1 <- '^B01AA'
               bool2 <- 'J01XD01|P01AB01|P01AB51|A02BD08|J01RA03|J01RA10|A02BD03|A02BD02|A02BD01|A02BD11|J01RA04'
           },
           DDI10 = {
               bool1 <- '^B01AA'
               bool2 <- 'C01BD01'
           },
           DDI11 = {
               bool1 <- '^B01AA|^B01AE|^B01AF'
               bool2 <- '^M01AA|^M01AB|^M01AC|^M01AE|^M01AG|^M01AH|M01AX01|^N02BA|B01AC06|B01AC56|A01AD05|M01BA03|C10BX08|C10BX12|C10BX06|C07FX04|N02AJ07|N02AJ02|C07FX03|N02AJ18|C10BX02|C10BX05|C10BX01|C10BX04|C07FX02|N02AJ08|N02AJ14|N02AJ19|L01XX33'
           },
           DDI12 = {
               bool1 <- '^B01AA|^B01AE|^B01AF'
               bool2 <- '^B01AC|^N02BA|A01AD05|M01BA03|C10BX08|C10BX12|C10BX06|C07FX04|N02AJ07|N02AJ02|C07FX03|N02AJ18|C10BX02|C10BX05|C10BX01|C10BX04|C07FX02|N02AJ08|N02AJ14|N02AJ19|L01XX33'
           },
           DDI13 = {
               bool1 <- '^B01AA'
               bool2 <- '^J01E|J04AM08|J01RA02'
           },
           DDI14 = {
               bool1 <- '^B01AA'
               bool2 <- '^J01M|J01RA09|J01RA10|J01RA11|J01RA12|J01RA13'
           },
           DDI15 = {
               bool1 <- '^B01AA'
               bool2 <- '^J01FA|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01RA07|J01RA04'
           },
           DDI16 = {
               bool1 <- 'B01AE07'
               bool2 <- 'J02AB02|J02AC02|C01BD07|C01BD01|L04AD01|C08DA01|C09BB10|C08DA51|C01BA01|C01BA51|C01BA71|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA01|J05AE03|J05AR23|J05AP52|J05AR10|J05AP53'
           },
           DDI17 = {
               bool1 <- 'B01AF03'
               bool2 <- 'J02AB02|J02AC02|C01BD07|C01BD01|L04AD01|C08DA01|C09BB10|C08DA51|C01BA01|C01BA51|C01BA71|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA01|J05AE03|J05AR23|J05AP52|J05AR10|J05AP53'
           },
           DDI18 = {
               bool1 <- 'B01AF01'
               bool2 <- 'J02AB02|J02AC02|J02AC03|J02AC04|J02AC01|C08DB01|C01BD07|C01BD01|L04AD01|C08DA01|C09BB10|C08DA51|C01BA01|C01BA51|C01BA71|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA01|J05AE03|J05AR23|J05AP52|J05AR10|J05AP53'
           },
           DDI19 = {
               bool1 <- 'B01AF02'
               bool2 <- 'J02AB02|J02AC02|J02AC03|J02AC04|J02AC01|C08DB01|C01BD07|C01BD01|L04AD01|C08DA01|C09BB10|C08DA51|C01BA01|C01BA51|C01BA71|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA01|J05AE03|J05AR23|J05AP52|J05AR10|J05AP53'
           },
           DDI20 = {
               bool1 <- '^B01AC|^N02BA|A01AD05|M01BA03|C10BX08|C10BX12|C10BX06|C07FX04|N02AJ07|N02AJ02|C07FX03|N02AJ18|C10BX02|C10BX05|C10BX01|C10BX04|C07FX02'
               bool2 <- '^M01AA|^M01AB|^M01AC|^M01AE|^M01AG|^M01AH|M01AX01|N02AJ08|N02AJ14|N02AJ19|L01XX33'
           },
           DDI21 = {
               bool1 <- 'C03DB01|C03DB02|C03DA04|C03DA01|^C03E|^C09|^M01AA|^M01AB|^M01AC|^M01AE|^M01AG|^M01AH|M01AX01|^J01E|J04AM08|J01RA02'
               bool2 <- NA
               concomitant <- TRUE
               drugs_amount <- 2
           },
           DDI22 = {
               bool1 <- '^C09|^C03D|^C03E'
               bool2 <- '^A12B'
           },
           DDI23 = {
               bool1 <- '^C09'
               bool2 <- '^M01AA|^M01AB|^M01AC|^M01AE|^M01AG|^M01AH|M01AX01|N02AJ08|N02AJ14|N02AJ19|L01XX33'
           },
           DDI24 = {
               bool1 <- '^C03|^C07B|^C07C|^C07D|^C08G|^C09BA|C09BX01|C09BX03|^C09DA|C09DX01|C09DX03|C09DX06|C09XA52|C09XA54|C10BX13'
               bool2 <- '^M01AA|^M01AB|^M01AC|^M01AE|^M01AG|^M01AH|M01AX01|N02AJ08|N02AJ14|N02AJ19|L01XX33'
           },
           DDI25 = {
               bool1 <- '^C10AA|A10BH51|A10BH52|^C10BA|^C10BX'
               bool2 <- 'C10AB04'
           },
           DDI26 = {
               bool1 <- 'C10AA05|C10BX08|C10BX03|C10BA05|C10BX15|C10BX12|C10BX06|C10BX11|C10AA01|C10BX01|C10BA02|C10BA04|C10BX04|A10BH51|C10AA02|C10BA01'
               bool2 <- 'C08DA01|C09BB10|C08DA51|C08DB01'
           },
           DDI27 = {
               bool1 <- 'C10AA01|C10BX01|C10BA02|C10BA04|C10BX04|A10BH51'
               bool2 <- 'C08CA01|C09XA53|C09XA54|C08GA02|C10BX03|C10BX11|C07FB07|C09DB07|C09DX06|C09DB05|C09BB03|C09DB06|C07FB13|C07FB12|C09DB02|C09DX03|C09BB04|C09BX01|C09BB07|C09BX03|C10BX09|C10BX07|C10BX14|C09DB04|C09DB01|C09DX01'
           },
           DDI28 = {
               bool1 <- 'C10AA05|C10BX08|C10BX03|C10BA05|C10BX15|C10BX12|C10BX06|C10BX11|C10AA01|C10BX01|C10BA02|C10BA04|C10BX04|A10BH51|C10AA02|C10BA01'
               bool2 <- 'C01BD01'
           },
           DDI29 = {
               bool1 <- 'C10AA05|C10BX08|C10BX03|C10BA05|C10BX15|C10BX12|C10BX06|C10BX11|C10AA01|C10BX01|C10BA02|C10BA04|C10BX04|A10BH51|C10AA02|C10BA01'
               bool2 <- 'J01FA01|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA06|J01FA15'
           },
           DDI30 = {
               bool1 <- '^C08|^C07FB|^C09BB|C09BX01|C09BX03|^C09DB|C09DX01|C09DX03|C09DX06|C09XA53|C09XA54|C10BX03|C10BX07|C10BX09|C10BX11|C10BX14'
               bool2 <- CYP3A4_inhibitors
           },
           DDI31 = {
               bool1 <- 'C01BA03'
               bool2 <- 'J01FA01|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA15'
           },
           DDI32 = {
               bool1 <- '^C07|C09BX02|C09DX05|^S01ED'
               bool2 <- 'C08DA01|C09BB10|C08DA51|C08DB01'
           },
           DDI33 = {
               bool1 <- 'C01BA02'
               bool2 <- 'C01BD01'
           },
           DDI34 = {
               bool1 <- 'C01BA02'
               bool2 <- 'J01EA01|J01EE01|J01EE02|J01EE03|J01EE04|J01EE05|J01EE07|J04AM08'
           },
           DDI35 = {
               bool1 <- 'C03CA01|C03CB01|C03EB01'
               bool2 <- 'C03CC01'
           },
           DDI36 = {
               bool1 <- '^N02A|^N03|^N05|^N06A|^N06C'
               bool2 <- NA
               concomitant <- TRUE
               drugs_amount <- 3
           },
           DDI37 = {
               bool1 <- 'N05BA12|N05BA01|N05CD05|N05CF02|N05CF01'
               bool2 <- CYP3A4_inhibitors
           },
           DDI38 = {
               bool1 <- '^N06AB|N06CA03'
               bool2 <- '^N06AF|^N06AG|^N04BD|J01XX08|N05AE04|^N05AN|^N06AA|N06AX16|N06AX21|N06AX17|N06AX11|N06AX25|N06AX05|N06AX03|N06AX18|N06AX26|R05DA09|N07XX59|N01AH01|N02AB03|N01AH51|N02AB02|N02AG03|N02AB52|N02AB72|N02AX02|N02AJ14|N02AJ15|N02AJ13|N07BC02|N02AC52|N02AD01|^N02CC|A08AA10|A08AA02|G04BX14'
           },
           DDI39 = {
               bool1 <- '^M01AA|^M01AB|^M01AC|^M01AE|^M01AG|^M01AH|M01AX01|^N02BA|B01AC06|B01AC56|A01AD05|M01BA03|C10BX08|C10BX12|C10BX06|C07FX04|N02AJ07|N02AJ02|C07FX03|N02AJ18|C10BX02|C10BX05|C10BX01|C10BX04|C07FX02|N02AJ08|N02AJ14|N02AJ19|L01XX33'
               bool2 <- '^N06AB|N06CA03|N06AX16|N06AX21'
           },
           DDI40 = {
               bool1 <- 'N06AB03|N06CA03'
               bool2 <- '^N06AA|N06CA01'
           },
           DDI41 = {
               bool1 <- '^N05AN'
               bool2 <- '^M01AA|^M01AB|^M01AC|^M01AE|^M01AG|^M01AH|M01AX01|^N02BA|B01AC06|B01AC56|A01AD05|M01BA03|C10BX08|C10BX12|C10BX06|C07FX04|N02AJ07|N02AJ02|C07FX03|N02AJ18|C10BX02|C10BX05|C10BX01|C10BX04|C07FX02|N02AJ08|N02AJ14|N02AJ19|L01XX33'
           },
           DDI42 = {
               bool1 <- '^N05AN'
               bool2 <- '^C03|^C07B|^C07C|^C07D|^C08G|^C09BA|C09BX01|C09BX03|^C09DA|C09DX01|C09DX03|C09DX06|C09XA52|C09XA54|C10BX13'
           },
           DDI43 = {
               bool1 <- '^N05AN'
               bool2 <- '^C09|C10BX04|C10BX06|C10BX07|C10BX10|C10BX11|C10BX12|C10BX13|C10BX14|C10BX15'
           },
           DDI44 = {
               bool1 <- '^N06AF|^N06AG|^N04BD|J01XX08'
               bool2 <- '^R01BA|^C01CA|^R03C|^R01AA|^R01AB|N06BA04|N06BA11|A08AA10|A10BX06|N06AX12|A08AA62|N06BA01|N06BA02|N06BA12|N06BA03|^R03AA|^R03AC'
           },
           DDI45 = {
               bool1 <- '^N06AF|^N06AG|J01XX08'
               bool2 <- 'N04BA01|N04BA02|N04BA03'
           },
           DDI46 = {
               bool1 <- '^N06AF|^N06AG|^N04BD|J01XX08'
               bool2 <- 'N02AB02|N02AG03|N02AB52|N02AB72|N01AH01|N02AB03|N01AH51'
           },
           DDI47 = {
               bool1 <- '^N06AF|^N06AG|^N04BD|J01XX08'
               bool2 <- '^N06A|^N06C'
           },
           DDI48 = {
               bool1 <- 'N03AF01'
               bool2 <- 'C08DA01|C09BB10|C08DA51|C08DB01'
           },
           DDI49 = {
               bool1 <- 'N03AF01'
               bool2 <- 'J01FA01|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA06|J01FA15'
           },
           DDI50 = {
               bool1 <- '^N06DA|^N07AA'
               bool2 <- '^C07|C09BX02|C09DX05|^S01ED|^N05AN|C01BG07|C01BA03|C01BC04|C01BB01|C05AD01|N01BB02|N01BB52|C01BB02|C01BC03|C01BA13|C01BA01|C01BA51|C01BA71|C01BD01|C01BD07|C01BD04|C01BD05|C07AA07|C07FX02|C07BA07|C01EB10|C01BG11|C01AA05|C01EB17|C01EB18|C02AC01|N02CX02|C02LC01|C02LC51|C02AC05|C02LC05|^C02AB|^C02LB|C02AC02|C02AC06|C02AA02|C02LA01|C02LA51|C02LA71|C02AA52|C08DB01|C08DA01|C09BB10|C08DA51|N04AA02|D11AX21|S01EA05|P01BC02|N07BC02|P01BF02|N02AC52|N02AB03|N01AH01|N01AH51|S01GB09|N07AX01|N03AB05|N02CA51|N02CA01|L04AA27|H01CB05|L04AX12|A04AD12'
           },
           DDI51 = {
               bool1 <- 'R03DA04|R03DB04|R03DA54|R03DA74'
               bool2 <- 'A02BA01|A02BA51'
           },
           DDI52 = {
               bool1 <- 'R03DA04|R03DB04|R03DA54|R03DA74'
               bool2 <- '^J01|J01RA09|J01RA10|J01RA11|J01RA12|J01RA13'
           },
           DDI53 = {
               bool1 <- 'R03DA04|R03DB04|R03DA54|R03DA74'
               bool2 <- 'J01FA01|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA06|J01FA15'
           },
           DDI54 = {
               bool1 <- 'R03DA04|R03DB04|R03DA54|R03DA74'
               bool2 <- 'N06AB08'
           },
           DDI55 = {
               bool1 <- 'L04AX01|L01BB02|L01BB03'
               bool2 <- 'M04AA01|M04AA51'
           },
           DDI56 = {
               bool1 <- '^H02|^M01BA|^N02CB'
               bool2 <- '^M01AA|^M01AB|^M01AC|^M01AE|^M01AG|^M01AH|M01AX01|^N02BA|B01AC06|B01AC56|A01AD05|M01BA03|C10BX08|C10BX12|C10BX06|C07FX04|N02AJ07|N02AJ02|C07FX03|N02AJ18|C10BX02|C10BX05|C10BX01|C10BX04|C07FX02|N02AJ08|N02AJ14|N02AJ19|L01XX33'
           },
           DDI57 = {
               bool1 <- 'A03BA01|A03CB03|A04AD01|N05CM05|A04AD51|A03BB03|A03CB01|^A03B|^A03CB|^A03DB|A06AB30|A03CA02|A03AA07|S01FA05|A03BA03|A03CB31|A03AB05|A03CA34|G04BD04|G04BD02|G04BD08|G04CA53|G04BD07|G04BD11|G04BD09|G04BD10|R06AX07|N04AA01|N04AC01|R06AB01|R06AB51|R06AA08|R06AB04|R06AB54|R06AA04|R06AA54|R06AX02|R06AB06|R06AB56|R06AB02|R06AB52|R06AA02|R06AA52|R06AA09|R06AA59|N05BB01|N05BB51|R06AE05|R06AE55|R06AD02|R06AD52|R06AC01|C01BA03|N06AA09|N06CA01|N06AA17|N06AA04|N06AA01|N06AA12|N06AA02|N06AA10|N06AA06|N06AB05|N05AA01|N05AB03|N05AB06|N05AC02|N05AH02|N05AH01|N05AH03|M03BX08|M03BC01|M03BC51'
               bool2 <- NA
               concomitant <- TRUE
               drugs_amount <- 2
               daily_dosage_check <- list( c('N06AA12'), c(6), c('mg'), c('greater') )
           },
           DDI58 = {
               bool1 <- 'L04AD01'
               bool2 <- 'J04AB02|J04AM02|J04AM07|J04AM05|J04AM06'
           },
           DDI59 = {
               bool1 <- '^C04AE|^G02AB|^G02AC|^N02CA'
               bool2 <- 'J01FA01|J01FA09|A02BD06|A02BD07|A02BD09|A02BD05|A02BD04|A02BD11|J01FA06|J01FA15'
           },
           DDI60 = {
               bool1 <- 'L01BA01|L04AX03'
               bool2 <- 'J01EA01|J01EE01|J01EE02|J01EE03|J01EE04|J01EE05|J01EE07|J04AM08'
           },
           DDI61 = {
               bool1 <- 'G04BE03|G04BE08|G04BE09|G04BE10|G04BE11'
               bool2 <- '^C01DA'
           },
           DDI62 = {
               bool1 <- 'L02BA01'
               bool2 <- '^B01AA'
           },
           DDI63 = {
               bool1 <- 'L02BA01'
               bool2 <- 'N06AB04|N06AB10'
           },
           DDI64 = {
               bool1 <- 'L02BA01'
               bool2 <- 'N06AB03|N06AC03|N06AB05|N06AX12|A08AA62'
           },
           DDI65 = {
               bool1 <- 'S01EC01|^C03A|^C03B|^C03C|^C07B|^C07C|^C07D|^C08G|^C09BA|C09BX01|C09BX03|^C09DA |C09DX01|C09DX03|C09DX06|C09XA52|C09XA54|C10BX13|A01AB04|A07AA07|J02AA01|^H02|^M01BA|^N02CB|^J01G|^A06AB|A06AG02|A06AD18|A06AG07|B05CX02|V04CC01|^R03AA|^R03AC|R03DA04|R03DB04|R03DA54|R03DA74|N06BC01|V04CG01'
               bool2 <- NA
               concomitant <- TRUE
               drugs_amount <- 2
           },
           DDI66 = {
               bool1 <- '^N06AB|N06CA03'
               bool2 <- '^C03A|^C03B|^C03C|^C03E|^C07B|^C07C|^C07D|^C08G|^C09BA|C09BX01|C09BX03|^C09DA|C09DX01|C09DX03|C09DX06|C09DX07|C09XA52|C09XA54|C10BX13'
           }

    )

    invisible(list(bool1, bool2, concomitant, drugs_amount, daily_dosage_check))
}
