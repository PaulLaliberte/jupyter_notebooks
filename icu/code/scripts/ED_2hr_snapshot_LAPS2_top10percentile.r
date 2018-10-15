#!/apps/R-3.2.3/lib64/R/bin/Rscript

h = commandArgs(TRUE)
h = as.numeric(h)

hosp_list = c(2,3,5,7,8,9,10,11,13,14,15,16,17,19,20,21,22,24,25,27,28)
bedhx = read.csv("/home/pl2669/Desktop/ICU_adm_structural/data/bedhx_board_stable.csv")
bedhx_direct = read.csv("/home/pl2669/Desktop/ICU_adm_structural/data/bedhx_direct_stable.csv")
# Consider the first ED stay of each hospitalization,
# Later stay typically due to transfer of hospitals and are decided pathways, exclude transported in from out of KP

# Restrict to each hospital
hosp_list = sort(unique(bedhx$HOSP))
bedhx_direct = subset(bedhx_direct, HOSP == hosp_list[h])
bedhx = subset(bedhx, HOSP == hosp_list[h])

bedhx$entry_DT = strptime(bedhx$entry_DT, "%Y-%m-%d %H:%M:%S", tz="EST")
bedhx$exit_DT = strptime(bedhx$exit_DT, "%Y-%m-%d %H:%M:%S", tz="EST")
bedhx_direct$entry_DT = strptime(bedhx_direct$entry_DT, "%Y-%m-%d %H:%M:%S", tz="EST")

start = strptime(format(min(bedhx$entry_DT, bedhx_direct$entry_DT), "%m/%d/%Y 01:00"), "%m/%d/%Y %H:%M", tz="EST")
end = strptime(format(max(bedhx$exit_DT, bedhx_direct$entry_DT), "%m/%d/%Y 23:00"), "%m/%d/%Y %H:%M", tz="EST") + 60*60
list_time = seq(from=start, to=end, by=120*60)
n_min = length(list_time)

# Matrix to store every hour snapshot of each hospital's ED
mat_ED = matrix(0, nrow = n_min, ncol = 44)

rownames(mat_ED) = list_time
colnames(mat_ED) = paste("ED_", c(paste(rep(c("LAPS2","arrival"), each=11), c(110,122,125,128,131,135,140,145,152,161,177), c(122,125,128,131,135,140,145,152,161,177,"max"), sep="_"), 
								paste(rep(c("admit","reroute"), 11), rep(c(110,122,125,128,131,135,140,145,152,161,177), each=2), rep(c(122,125,128,131,135,140,145,152,161,177,"max"), each=2), sep="_")), sep="")

for(t in 1:(n_min-2)){
	# How many patients are already in the ED at sharp time t or who entered and exited in (t,t+1]
	index1 = which((bedhx$entry_DT <= list_time[t] & bedhx$exit_DT > list_time[t]) | (bedhx$entry_DT > list_time[t] & bedhx$exit_DT <= list_time[t+1]))

	# How many patients not waited in the ED and admitted directly in (t, t+1]
	index2 = which(bedhx_direct$entry_DT > list_time[t] & bedhx_direct$entry_DT <= list_time[t+1])
	
	# How many patients arrive in (t, t+1]
	index3 = which((bedhx$entry_DT > list_time[t] & bedhx$entry_DT <= list_time[t+1] & bedhx$exit_DT > list_time[t+1]) | (bedhx$entry_DT > list_time[t+1] & bedhx$exit_DT <= list_time[t+2]))
	index4 = which(bedhx_direct$entry_DT > list_time[t+1] & bedhx_direct$entry_DT <= list_time[t+2])

	
	if(length(index1) > 0 & length(index2) > 0){
		mat_ED[t,1] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 110 & c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) < 122)
		mat_ED[t,2] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 122 & c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) < 125)
		mat_ED[t,3] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 125 & c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) < 128)
		mat_ED[t,4] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 128 & c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) < 131)
		mat_ED[t,5] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 131 & c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) < 135)
		mat_ED[t,6] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 135 & c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) < 140)
		mat_ED[t,7] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 140 & c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) < 145)
		mat_ED[t,8] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 145 & c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) < 152)
		mat_ED[t,9] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 152 & c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) < 161)
		mat_ED[t,10] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 161 & c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) < 177)
		mat_ED[t,11] = sum(c(bedhx$LAPS2[index1], bedhx_direct$LAPS2[index2]) >= 177)
				
		mat_ED[t,23] = sum(bedhx$LAPS2[index1] >= 110 & bedhx$LAPS2[index1] < 122 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 110 & bedhx_direct$LAPS2[index2] < 122 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,24] = sum(bedhx$LAPS2[index1] >= 110 & bedhx$LAPS2[index1] < 122 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 110 & bedhx_direct$LAPS2[index2] < 122 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,25] = sum(bedhx$LAPS2[index1] >= 122 & bedhx$LAPS2[index1] < 125 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 122 & bedhx_direct$LAPS2[index2] < 125 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,26] = sum(bedhx$LAPS2[index1] >= 122 & bedhx$LAPS2[index1] < 125 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 122 & bedhx_direct$LAPS2[index2] < 125 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,27] = sum(bedhx$LAPS2[index1] >= 125 & bedhx$LAPS2[index1] < 128 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 125 & bedhx_direct$LAPS2[index2] < 128 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,28] = sum(bedhx$LAPS2[index1] >= 125 & bedhx$LAPS2[index1] < 128 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 125 & bedhx_direct$LAPS2[index2] < 128 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,29] = sum(bedhx$LAPS2[index1] >= 128 & bedhx$LAPS2[index1] < 131 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 128 & bedhx_direct$LAPS2[index2] < 131 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,30] = sum(bedhx$LAPS2[index1] >= 128 & bedhx$LAPS2[index1] < 131 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 128 & bedhx_direct$LAPS2[index2] < 131 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,31] = sum(bedhx$LAPS2[index1] >= 131 & bedhx$LAPS2[index1] < 135 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 131 & bedhx_direct$LAPS2[index2] < 135 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,32] = sum(bedhx$LAPS2[index1] >= 131 & bedhx$LAPS2[index1] < 135 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 131 & bedhx_direct$LAPS2[index2] < 135 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,33] = sum(bedhx$LAPS2[index1] >= 135 & bedhx$LAPS2[index1] < 140 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 135 & bedhx_direct$LAPS2[index2] < 140 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,34] = sum(bedhx$LAPS2[index1] >= 135 & bedhx$LAPS2[index1] < 140 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 135 & bedhx_direct$LAPS2[index2] < 140 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,35] = sum(bedhx$LAPS2[index1] >= 140 & bedhx$LAPS2[index1] < 145 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 140 & bedhx_direct$LAPS2[index2] < 145 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,36] = sum(bedhx$LAPS2[index1] >= 140 & bedhx$LAPS2[index1] < 145 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 140 & bedhx_direct$LAPS2[index2] < 145 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,37] = sum(bedhx$LAPS2[index1] >= 145 & bedhx$LAPS2[index1] < 152 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 145 & bedhx_direct$LAPS2[index2] < 152 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,38] = sum(bedhx$LAPS2[index1] >= 145 & bedhx$LAPS2[index1] < 152 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 145 & bedhx_direct$LAPS2[index2] < 152 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,39] = sum(bedhx$LAPS2[index1] >= 152 & bedhx$LAPS2[index1] < 161 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 152 & bedhx_direct$LAPS2[index2] < 161 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,40] = sum(bedhx$LAPS2[index1] >= 152 & bedhx$LAPS2[index1] < 161 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 152 & bedhx_direct$LAPS2[index2] < 161 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,41] = sum(bedhx$LAPS2[index1] >= 161 & bedhx$LAPS2[index1] < 177 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 161 & bedhx_direct$LAPS2[index2] < 177 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,42] = sum(bedhx$LAPS2[index1] >= 161 & bedhx$LAPS2[index1] < 177 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 161 & bedhx_direct$LAPS2[index2] < 177 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,43] = sum(bedhx$LAPS2[index1] >= 177 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
						+ sum(bedhx_direct$LAPS2[index2] >= 177 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,44] = sum(bedhx$LAPS2[index1] >= 177 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
						+ sum(bedhx_direct$LAPS2[index2] >= 177 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
	
	}else if(length(index1) > 0){
		mat_ED[t,1] = sum(bedhx$LAPS2[index1] >= 110 & bedhx$LAPS2[index1] < 122)
		mat_ED[t,2] = sum(bedhx$LAPS2[index1] >= 122 & bedhx$LAPS2[index1] < 125)
		mat_ED[t,3] = sum(bedhx$LAPS2[index1] >= 125 & bedhx$LAPS2[index1] < 128)
		mat_ED[t,4] = sum(bedhx$LAPS2[index1] >= 128 & bedhx$LAPS2[index1] < 131)
		mat_ED[t,5] = sum(bedhx$LAPS2[index1] >= 131 & bedhx$LAPS2[index1] < 135)
		mat_ED[t,6] = sum(bedhx$LAPS2[index1] >= 135 & bedhx$LAPS2[index1] < 140)
		mat_ED[t,7] = sum(bedhx$LAPS2[index1] >= 140 & bedhx$LAPS2[index1] < 145)
		mat_ED[t,8] = sum(bedhx$LAPS2[index1] >= 145 & bedhx$LAPS2[index1] < 152)
		mat_ED[t,9] = sum(bedhx$LAPS2[index1] >= 152 & bedhx$LAPS2[index1] < 161)
		mat_ED[t,10] = sum(bedhx$LAPS2[index1] >= 161 & bedhx$LAPS2[index1] < 177)
		mat_ED[t,11] = sum(bedhx$LAPS2[index1] >= 177)
				
		mat_ED[t,23] = sum(bedhx$LAPS2[index1] >= 110 & bedhx$LAPS2[index1] < 122 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,24] = sum(bedhx$LAPS2[index1] >= 110 & bedhx$LAPS2[index1] < 122 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
		
		mat_ED[t,25] = sum(bedhx$LAPS2[index1] >= 122 & bedhx$LAPS2[index1] < 125 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,26] = sum(bedhx$LAPS2[index1] >= 122 & bedhx$LAPS2[index1] < 125 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
		
		mat_ED[t,27] = sum(bedhx$LAPS2[index1] >= 125 & bedhx$LAPS2[index1] < 128 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,28] = sum(bedhx$LAPS2[index1] >= 125 & bedhx$LAPS2[index1] < 128 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
		
		mat_ED[t,29] = sum(bedhx$LAPS2[index1] >= 128 & bedhx$LAPS2[index1] < 131 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,30] = sum(bedhx$LAPS2[index1] >= 128 & bedhx$LAPS2[index1] < 131 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
		
		mat_ED[t,31] = sum(bedhx$LAPS2[index1] >= 131 & bedhx$LAPS2[index1] < 135 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,32] = sum(bedhx$LAPS2[index1] >= 131 & bedhx$LAPS2[index1] < 135 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
		
		mat_ED[t,33] = sum(bedhx$LAPS2[index1] >= 135 & bedhx$LAPS2[index1] < 140 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,34] = sum(bedhx$LAPS2[index1] >= 135 & bedhx$LAPS2[index1] < 140 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
		
		mat_ED[t,35] = sum(bedhx$LAPS2[index1] >= 140 & bedhx$LAPS2[index1] < 145 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,36] = sum(bedhx$LAPS2[index1] >= 140 & bedhx$LAPS2[index1] < 145 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
		
		mat_ED[t,37] = sum(bedhx$LAPS2[index1] >= 145 & bedhx$LAPS2[index1] < 152 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,38] = sum(bedhx$LAPS2[index1] >= 145 & bedhx$LAPS2[index1] < 152 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
		
		mat_ED[t,39] = sum(bedhx$LAPS2[index1] >= 152 & bedhx$LAPS2[index1] < 161 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,40] = sum(bedhx$LAPS2[index1] >= 152 & bedhx$LAPS2[index1] < 161 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
		
		mat_ED[t,41] = sum(bedhx$LAPS2[index1] >= 161 & bedhx$LAPS2[index1] < 177 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,42] = sum(bedhx$LAPS2[index1] >= 161 & bedhx$LAPS2[index1] < 177 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
		
		mat_ED[t,43] = sum(bedhx$LAPS2[index1] >= 177 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] == "ICU")
		mat_ED[t,44] = sum(bedhx$LAPS2[index1] >= 177 & bedhx$exit_DT[index1] <= list_time[t+1] & bedhx$next_unit[index1] %in% c("FLR", "TCU"))
	
	}else if(length(index2) > 0){
		mat_ED[t,1] = sum(bedhx_direct$LAPS2[index2] >= 110 & bedhx_direct$LAPS2[index2] < 122)
		mat_ED[t,2] = sum(bedhx_direct$LAPS2[index2] >= 122 & bedhx_direct$LAPS2[index2] < 125)
		mat_ED[t,3] = sum(bedhx_direct$LAPS2[index2] >= 125 & bedhx_direct$LAPS2[index2] < 128)
		mat_ED[t,4] = sum(bedhx_direct$LAPS2[index2] >= 128 & bedhx_direct$LAPS2[index2] < 131)
		mat_ED[t,5] = sum(bedhx_direct$LAPS2[index2] >= 131 & bedhx_direct$LAPS2[index2] < 135)
		mat_ED[t,6] = sum(bedhx_direct$LAPS2[index2] >= 135 & bedhx_direct$LAPS2[index2] < 140)
		mat_ED[t,7] = sum(bedhx_direct$LAPS2[index2] >= 140 & bedhx_direct$LAPS2[index2] < 145)
		mat_ED[t,8] = sum(bedhx_direct$LAPS2[index2] >= 145 & bedhx_direct$LAPS2[index2] < 152)
		mat_ED[t,9] = sum(bedhx_direct$LAPS2[index2] >= 152 & bedhx_direct$LAPS2[index2] < 161)
		mat_ED[t,10] = sum(bedhx_direct$LAPS2[index2] >= 161 & bedhx_direct$LAPS2[index2] < 177)
		mat_ED[t,11] = sum(bedhx_direct$LAPS2[index2] >= 177)
				
		mat_ED[t,23] = sum(bedhx_direct$LAPS2[index2] >= 110 & bedhx_direct$LAPS2[index2] < 122 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,24] = sum(bedhx_direct$LAPS2[index2] >= 110 & bedhx_direct$LAPS2[index2] < 122 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,25] = sum(bedhx_direct$LAPS2[index2] >= 122 & bedhx_direct$LAPS2[index2] < 125 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,26] = sum(bedhx_direct$LAPS2[index2] >= 122 & bedhx_direct$LAPS2[index2] < 125 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,27] = sum(bedhx_direct$LAPS2[index2] >= 125 & bedhx_direct$LAPS2[index2] < 128 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,28] = sum(bedhx_direct$LAPS2[index2] >= 125 & bedhx_direct$LAPS2[index2] < 128 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,29] = sum(bedhx_direct$LAPS2[index2] >= 128 & bedhx_direct$LAPS2[index2] < 131 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,30] = sum(bedhx_direct$LAPS2[index2] >= 128 & bedhx_direct$LAPS2[index2] < 131 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,31] = sum(bedhx_direct$LAPS2[index2] >= 131 & bedhx_direct$LAPS2[index2] < 135 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,32] = sum(bedhx_direct$LAPS2[index2] >= 131 & bedhx_direct$LAPS2[index2] < 135 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,33] = sum(bedhx_direct$LAPS2[index2] >= 135 & bedhx_direct$LAPS2[index2] < 140 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,34] = sum(bedhx_direct$LAPS2[index2] >= 135 & bedhx_direct$LAPS2[index2] < 140 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,35] = sum(bedhx_direct$LAPS2[index2] >= 140 & bedhx_direct$LAPS2[index2] < 145 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,36] = sum(bedhx_direct$LAPS2[index2] >= 140 & bedhx_direct$LAPS2[index2] < 145 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,37] = sum(bedhx_direct$LAPS2[index2] >= 145 & bedhx_direct$LAPS2[index2] < 152 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,38] = sum(bedhx_direct$LAPS2[index2] >= 145 & bedhx_direct$LAPS2[index2] < 152 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,39] = sum(bedhx_direct$LAPS2[index2] >= 152 & bedhx_direct$LAPS2[index2] < 161 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,40] = sum(bedhx_direct$LAPS2[index2] >= 152 & bedhx_direct$LAPS2[index2] < 161 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,41] = sum(bedhx_direct$LAPS2[index2] >= 161 & bedhx_direct$LAPS2[index2] < 177 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,42] = sum(bedhx_direct$LAPS2[index2] >= 161 & bedhx_direct$LAPS2[index2] < 177 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))
		
		mat_ED[t,43] = sum(bedhx_direct$LAPS2[index2] >= 177 & bedhx_direct$UNIT_CD[index2] == "ICU")
		mat_ED[t,44] = sum(bedhx_direct$LAPS2[index2] >= 177 & bedhx_direct$UNIT_CD[index2] %in% c("FLR", "TCU"))

	}
	
	if(length(index3) > 0 & length(index4) > 0){
		mat_ED[t,12] = sum(bedhx$LAPS2[index3] >= 110 & bedhx$LAPS2[index3] < 122) + sum(bedhx_direct$LAPS2[index4] >= 110 & bedhx_direct$LAPS2[index4] < 122)
		mat_ED[t,13] = sum(bedhx$LAPS2[index3] >= 122 & bedhx$LAPS2[index3] < 125) + sum(bedhx_direct$LAPS2[index4] >= 122 & bedhx_direct$LAPS2[index4] < 125)
		mat_ED[t,14] = sum(bedhx$LAPS2[index3] >= 125 & bedhx$LAPS2[index3] < 128) + sum(bedhx_direct$LAPS2[index4] >= 125 & bedhx_direct$LAPS2[index4] < 128)
		mat_ED[t,15] = sum(bedhx$LAPS2[index3] >= 128 & bedhx$LAPS2[index3] < 131) + sum(bedhx_direct$LAPS2[index4] >= 128 & bedhx_direct$LAPS2[index4] < 131)
		mat_ED[t,16] = sum(bedhx$LAPS2[index3] >= 131 & bedhx$LAPS2[index3] < 135) + sum(bedhx_direct$LAPS2[index4] >= 131 & bedhx_direct$LAPS2[index4] < 135)
		mat_ED[t,17] = sum(bedhx$LAPS2[index3] >= 135 & bedhx$LAPS2[index3] < 140) + sum(bedhx_direct$LAPS2[index4] >= 135 & bedhx_direct$LAPS2[index4] < 140)
		mat_ED[t,18] = sum(bedhx$LAPS2[index3] >= 140 & bedhx$LAPS2[index3] < 145) + sum(bedhx_direct$LAPS2[index4] >= 140 & bedhx_direct$LAPS2[index4] < 145)
		mat_ED[t,19] = sum(bedhx$LAPS2[index3] >= 145 & bedhx$LAPS2[index3] < 152) + sum(bedhx_direct$LAPS2[index4] >= 145 & bedhx_direct$LAPS2[index4] < 152)
		mat_ED[t,20] = sum(bedhx$LAPS2[index3] >= 152 & bedhx$LAPS2[index3] < 161) + sum(bedhx_direct$LAPS2[index4] >= 152 & bedhx_direct$LAPS2[index4] < 161)
		mat_ED[t,21] = sum(bedhx$LAPS2[index3] >= 161 & bedhx$LAPS2[index3] < 177) + sum(bedhx_direct$LAPS2[index4] >= 161 & bedhx_direct$LAPS2[index4] < 177)
		mat_ED[t,22] = sum(bedhx$LAPS2[index3] >= 177) + sum(bedhx_direct$LAPS2[index4] >= 177)
		
	}else if(length(index3) > 0){
		mat_ED[t,12] = sum(bedhx$LAPS2[index3] >= 110 & bedhx$LAPS2[index3] < 122)
		mat_ED[t,13] = sum(bedhx$LAPS2[index3] >= 122 & bedhx$LAPS2[index3] < 125)
		mat_ED[t,14] = sum(bedhx$LAPS2[index3] >= 125 & bedhx$LAPS2[index3] < 128)
		mat_ED[t,15] = sum(bedhx$LAPS2[index3] >= 128 & bedhx$LAPS2[index3] < 131)
		mat_ED[t,16] = sum(bedhx$LAPS2[index3] >= 131 & bedhx$LAPS2[index3] < 135)
		mat_ED[t,17] = sum(bedhx$LAPS2[index3] >= 135 & bedhx$LAPS2[index3] < 140)
		mat_ED[t,18] = sum(bedhx$LAPS2[index3] >= 140 & bedhx$LAPS2[index3] < 145)
		mat_ED[t,19] = sum(bedhx$LAPS2[index3] >= 145 & bedhx$LAPS2[index3] < 152)
		mat_ED[t,20] = sum(bedhx$LAPS2[index3] >= 152 & bedhx$LAPS2[index3] < 161)
		mat_ED[t,21] = sum(bedhx$LAPS2[index3] >= 161 & bedhx$LAPS2[index3] < 177)
		mat_ED[t,22] = sum(bedhx$LAPS2[index3] >= 177)
		
	}else if(length(index4) > 0){
		mat_ED[t,12] = sum(bedhx_direct$LAPS2[index4] >= 110 & bedhx_direct$LAPS2[index4] < 122)
		mat_ED[t,13] = sum(bedhx_direct$LAPS2[index4] >= 122 & bedhx_direct$LAPS2[index4] < 125)
		mat_ED[t,14] = sum(bedhx_direct$LAPS2[index4] >= 125 & bedhx_direct$LAPS2[index4] < 128)
		mat_ED[t,15] = sum(bedhx_direct$LAPS2[index4] >= 128 & bedhx_direct$LAPS2[index4] < 131)
		mat_ED[t,16] = sum(bedhx_direct$LAPS2[index4] >= 131 & bedhx_direct$LAPS2[index4] < 135)
		mat_ED[t,17] = sum(bedhx_direct$LAPS2[index4] >= 135 & bedhx_direct$LAPS2[index4] < 140)
		mat_ED[t,18] = sum(bedhx_direct$LAPS2[index4] >= 140 & bedhx_direct$LAPS2[index4] < 145)
		mat_ED[t,19] = sum(bedhx_direct$LAPS2[index4] >= 145 & bedhx_direct$LAPS2[index4] < 152)
		mat_ED[t,20] = sum(bedhx_direct$LAPS2[index4] >= 152 & bedhx_direct$LAPS2[index4] < 161)
		mat_ED[t,21] = sum(bedhx_direct$LAPS2[index4] >= 161 & bedhx_direct$LAPS2[index4] < 177)
		mat_ED[t,22] = sum(bedhx_direct$LAPS2[index4] >= 177)	
	}
}

list_time = format(list_time, "%m/%d/%Y %H:%M")
#write.csv(cbind(list_time, mat_ED), paste("2hr snapshots/ED_LAPS2_top_10percentile/HOSP_",hosp_list[h],"_ED_2hr_snapshots.csv", sep=""), row.names=F)
