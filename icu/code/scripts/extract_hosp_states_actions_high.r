#!/apps/R-3.2.3/lib64/R/bin/Rscript

setwd("/home/pl2669/Desktop/ICU_adm_structural/hospital snapshots/2hr snapshots")
stable_time = read.csv("/home/pl2669/Desktop/ICU_adm_structural/data/stable_DT.csv", as.is=TRUE)

hosp_list = c(2, 3, 5, 7, 8, 9, 10, 11,13,14,15,16,17,19,20,21,22,24,25,27,28)

for (index in 1:21) {
    hosp = hosp_list[index]

    data_ED = read.csv(paste("ED_LAPS2_top_10percentile/HOSP_", hosp, "_ED_2hr_snapshots.csv", sep=""))
    data_ED$list_time = strptime(data_ED$list_time, "%m/%d/%Y %H:%M", tz="EST")
    data_ICU = read.csv(paste("ICU/HOSP_", hosp, "_ICU_2hr_snapshots.csv", sep=""))
    data_ICU$list_time = strptime(data_ICU$list_time, "%m/%d/%Y %H:%M", tz="EST")

    ## Drop the first month for warm-up occupancy
    start = max(strptime(stable_time[index, 1], "%Y-%m-%d %H:%M", tz="EST"), min(data_ICU$list_time))
    end = min(strptime(stable_time[index, 2], "%Y-%m-%d %H:%M", tz="EST"), max(data_ICU$list_time))
    data_ED = subset(data_ED, list_time >= start & list_time <= end)
    data_ICU = subset(data_ICU, list_time >= start & list_time <= end)

    ## Eliminate all zero states, as they are not informative
    list_rows = which(rowSums(data_ED[, 2:12]) > 0)
    data_ED = data_ED[list_rows, ]
    data_ICU = data_ICU[list_rows, ]

    ## Find out which index is the states and actions (low=[85,97); high=[97,100])
    #print(data_ICU[,2:5])
    #print(data_ED[, 2:9])
    
    #(high_ED, ICU_all, externals_{low, other_externals})
    states = cbind(rowSums(data_ED[,10:12]), rowSums(data_ICU[, c("ICU_ED_Medical", "ICU_ED_Surgical", "ICU_non_ED_Medical", "ICU_non_ED_Surgical")]), rowSums(data_ICU[, c("ICU_ED_Medical", "ICU_non_ED_Medical", "ICU_non_ED_Surgical")]))
    actions = cbind(rowSums(data_ED[,seq(from=40,to=44,by=2)]), rowSums(data_ED[,seq(from=41,to=45,by=2)]))

    write.table(states, paste("/home/pl2669/Desktop/ccp/extracted_data/states_actions_ICU_include_all/no_dropping_zeros/high/HOSP_",hosp,"_observed_states.txt", sep=""), row.names=F, col.names=F)

    write.table(actions, paste("/home/pl2669/Desktop/ccp/extracted_data/states_actions_ICU_include_all/no_dropping_zeros/high/HOSP_",hosp,"_observed_actions.txt", sep=""), row.names=F, col.names=F)
}
