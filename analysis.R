library(survival)
ibrary(timeROC)
dat <- read.csv("~/Desktop/TEAM METHODS/DM1/DM1 data - envoi stats 23 12 2021.csv", sep=";")

fix_year <- function(my_string)
#A function that fixes two digits years
{
max(unlist(gregexpr('/', my_string)))
last_slash_pos <- max(unlist(gregexpr('/', my_string)))
temp <- substr(my_string, last_slash_pos+1, nchar(my_string))
if(nchar(temp)==2) {
        fixed_year <- paste0("20", temp)
        res <- paste0(substr(my_string, 1, last_slash_pos), fixed_year)
        } else if(nchar(temp)==4){
        res <- my_string
        } else if(nchar(temp)==0){
        res <- my_string
        print("Empty string provided. Results is returned unchanged")        
        } else {
        res <- my_string
        print("Year appears neither two nor four digits.. Results is returned unchanged")
        }
return(res)
}


dat$date_baseline <- as.POSIXct(sapply(dm_dat$date.baseline, fix_year), format="%d/%m/%Y")
dat$date_mace <- as.POSIXct(sapply(dm_dat$mace_date, fix_year), format="%d/%m/%Y")
dat$date_non_cv_death <- as.POSIXct(sapply(dm_dat$deces_autre_date, fix_year), format="%d/%m/%Y")
dat$date_cv_death <- as.POSIXct(sapply(dm_dat$mort_subite_date, fix_year), format="%d/%m/%Y")
dat$date_end <- as.POSIXct(sapply(dm_dat$date_fin_suivi, fix_year), format="%d/%m/%Y")

dat$date_first_event <- pmin(dat$date_mace, dat$date_non_cv_death, dat$date_end, na.rm = TRUE)
dat$event_type <- apply(cbind(dat$date_mace, dat$date_non_cv_death, dat$date_end), 1, which.min)
dat$event_type[dat$event_type==3] <- 0 # recode censored patients to zero
dat$time_to_event <- as.numeric(with(dat, difftime(date_first_event, date_baseline, units = "days")) / (365.25 /12)) # in month

comp_dat <- dat[complete.cases(dat[, c("pr", "qrs", "hv")]),]

ROC.hv<-timeROC(T=comp_dat$time_to_event,
                   delta=comp_dat$event_type,
                   weighting="marginal",
                   marker=comp_dat$hv,
                   cause=1,
                   times=seq(from=6, to=20*12, by=1),
                   iid=FALSE)

ROC.qrs<-timeROC(T=comp_dat$time_to_event,
                delta=comp_dat$event_type,
                weighting="marginal",
                marker=comp_dat$qrs,
                cause=1,
                times=seq(from=6, to=20*12, by=1),
                iid=FALSE)

ROC.pr<-timeROC(T=comp_dat$time_to_event,
                 delta=comp_dat$event_type,
                 weighting="marginal",
                 marker=comp_dat$pr,
                 cause=1,
                 times=seq(from=6, to=20*12, by=1),
                 iid=FALSE)


plot(ROC.hv, time=18)

plotAUCcurve(ROC.hv, FP=1, conf.int = TRUE)
plotAUCcurve(ROC.qrs, FP=1, add = TRUE, col = "red", conf.int = FALSE)
plotAUCcurve(ROC.pr, FP=1, add = TRUE, col = "blue", conf.int = FALSE)


ROC.hv<-timeROC(T=comp_dat$time_to_event,
                delta=comp_dat$event_type,
                weighting="marginal",
                marker=comp_dat$hv,
                cause=1,
                times=seq(from=6, to=20*12, by=24),
                iid=TRUE)

ROC.qrs<-timeROC(T=comp_dat$time_to_event,
                 delta=comp_dat$event_type,
                 weighting="marginal",
                 marker=comp_dat$qrs,
                 cause=1,
                 times=seq(from=6, to=20*12, by=24),
                 iid=TRUE)

compare(ROC.hv, ROC.qrs)$p_values_AUC_1<.05

ROC.hv$AUC_1 - ROC.qrs$AUC_1
