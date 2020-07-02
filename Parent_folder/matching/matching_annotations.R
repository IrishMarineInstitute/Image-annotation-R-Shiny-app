
list.of.packages <- c("epiR", "ggplot2", "gridExtra", "gtools", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(epiR)
library(ggplot2)
library(gridExtra)
library(gtools)
library(reshape2)



setwd(matching.folder)
all_counts <- list.files(path = counts.folder,
                         pattern = paste0(as.character(pairs3[row.match,2]),"|",as.character(pairs3[row.match,3])),
                         full.names = T)
stations <- gsub(".*_","", as.character(pairs3[row.match,1]))


# # setwd("C:/GitHub/Image-annotation-R-Shiny-app/Parent_folder/matching")
# all_counts <- list.files(path = "C:/GitHub/Image-annotation-R-Shiny-app/Parent_folder/app_outcome/counts",
#                          pattern = paste0("CV19017_stn_286_Patrick_counts.csv|CV19017_stn_286_Sandra_counts.csv"),
#                          full.names = T)
# stations <- 286
# all_counts <- list.files(path = "C:/GitHub/Image-annotation-R-Shiny-app/Parent_folder/app_outcome/counts",
#                          pattern = paste0("CV19017_stn_286_Gary_counts.csv|CV19017_stn_286_Bob_counts.csv"),
#                          full.names = T)
# stations <- 286
# # 
# all_counts <- list.files(path = "C:/GitHub/Image-annotation-R-Shiny-app/Parent_folder/app_outcome/counts",
#                          "CV19017_stn_122_Sandra_counts|CV19017_stn_122_Gary_counts",
#                          full.names = T)
# stations <- 122

# all_counts <- list.files(path = "C:/GitHub/Image-annotation-R-Shiny-app/Parent_folder/app_outcome/counts",
#                          "CV19017_stn_138_Sandra_counts|CV19017_stn_138_Patrick_counts",
#                          full.names = T)
# stations <- 138

# Run Lins
# confirmed_minutes <- 1:7
# confirmed_minutes <- c(2:8, 10, 11)

temp2 <- data.frame(minute = c(as.numeric(as.character(confirmed_minutes))))
for (i in 1:length(all_counts)){
  cur.counts <- read.csv(all_counts[i])
  cur.stn <- as.character(cur.counts$station[1])
  cur.counter <- as.character(cur.counts$counter_ID[1])
  cur.counts <- as.data.frame(table(cur.counts$minute))
  names(cur.counts) <- c("minute", cur.counter)
  temp2 <- merge(temp2, cur.counts, all = T)
}
temp2[is.na(temp2)] <- 0
temp2 <- temp2[temp2$minute %in% confirmed_minutes,]

# hatn (burrows per minute). If it's too low we don't run the Lins
hatn <- (sum(temp2[,2:3])) / (nrow(temp2) * 2)
burrows_cnt1 <- sum(temp2[,2])
burrows_cnt2 <- sum(temp2[,3])
# if zero / if too low / else
if (burrows_cnt1 == 0 | burrows_cnt2 == 0) {
  lins.value <- "zero_for_one_counter"
  } else if (hatn == 0) {lins.value <- "zero"} else if (hatn < 1.5) {lins.value <- "too_low"} else {
    tmp.ccc <- epi.ccc(temp2[,2], temp2[,3], ci = "z-transform",conf.level = 0.95)
    lins.value <- round(tmp.ccc$rho.c[1],2)
    }

lins <- data.frame(stn = cur.stn, VideoLine = stations, Initials1 = names(temp2)[2], Initials2 = names(temp2)[3], 
               LinsCCC = lins.value
               # , min_1 = 1, min_2 = 2, min_3 = 3,
               # min_4 = 4, min_5 = 5, min_6 = 6, min_7 = 7
               )
names(lins)[names(lins) == "est"] <- "LinsCCC"


# Run matching

#stations <- c(170:265, 1850, 1870)
# stations <- unique(lins$VideoLine)
# stations <- stations[stations!=205]

still_accu <- 36
# still_accu <- c(12, 24, 36, 48)
x_accu <- 625
# x_accu <- c(50, 150, 625, 1250)

box_y <- 50
box_x <- 5

# k<- 194
# stations<- 194:197
#l<-4

# This has to be changed also inside the loop:
# anon1 <- "Anna" # if you want anonymised
# anon2 <- "John" # if you want anonymised
anon1 <- lins$Initials1 # if you want real names
anon2 <- lins$Initials2 # if you want real names




# old.time <- Sys.time()
for (l in 1:length(x_accu)) {
  
  print(" ")
  print(paste0("x accuracy : ", x_accu[l]))
  print(" ")
  dir.create(paste0("./match_x_", x_accu[l]))
  
for (m in 1:length(still_accu)) {
  
  print(" ")
  print(paste0("still accuracy : ", still_accu[m]))
  print(" ")
  dir.create(paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m]))
  dir.create(paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "/match_annotations_plots"))
  dir.create(paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "/match_annotations_tables"))
  
  perc_total <- data.frame(NULL)

for(k in stations) {
  
  print(" ")
  print(paste0("Station: ", k))
  if (any(lins$LinsCCC[lins$VideoLine==k] == "zero")) {
    lins.tit <- "zero station"
    print("zero")
    
    ggplot() + 
      annotate("text", x = 4, y = 25, size=8, label = paste0(k, " is a ZERO station")) + 
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
    
    plot_name <- paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "/match_annotations_plots/matches_plot_stn_", k, "_ZERO_station.png")
    ggsave(plot_name, width=16, height=9)

    write.csv(paste0(k, "_zero"), paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "/match_annotations_tables/matches_table_stn_", k, "_ZERO_station.csv"), row.names=F)
    
    n.counters <- 0
    
    perc_total <- rbind(perc_total,
                        cbind(stn = k, "zero", "zero", "zero", "zero", "zero", "zero", "zero", "zero"))
    names(perc_total) <- c("stn",
                           paste0(anon1, "_no_n"), paste0(anon1, "_yes_n"),
                           paste0(anon1, "_no_%"), paste0(anon1, "_yes_%"),
                           paste0(anon2, "_no_n"), paste0(anon2, "_yes_n"),
                           paste0(anon2, "_no_%"), paste0(anon2, "_yes_%"))
    
  } else if (any(lins$LinsCCC[lins$VideoLine==k] == "zero_for_one_counter")) {
    
    lins.tit <- "quasi zero station"
    print("quasizero")
    
    ggplot() + 
      annotate("text", x = 4, y = 25, size=8, label = paste0(k, " is a quasiZERO station")) + 
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
    
    plot_name <- paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "/match_annotations_plots/matches_plot_stn_", k, "_quasiZERO_station.png")
    ggsave(plot_name, width=16, height=9)
    
    write.csv(paste0(k, "_quasizero"), paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "/match_annotations_tables/matches_table_stn_", k, "_quasiZERO_station.csv"), row.names=F)
    
    n.counters <- 0
    
    perc_total <- rbind(perc_total,
                        cbind(stn = k, "quasizero", "quasizero", "quasizero", "quasizero", "quasizero", "quasizero", "quasizero", "quasizero"))
    names(perc_total) <- c("stn",
                           paste0(anon1, "_no_n"), paste0(anon1, "_yes_n"),
                           paste0(anon1, "_no_%"), paste0(anon1, "_yes_%"),
                           paste0(anon2, "_no_n"), paste0(anon2, "_yes_n"),
                           paste0(anon2, "_no_%"), paste0(anon2, "_yes_%"))
    
    
  } else if(any(lins$LinsCCC[lins$VideoLine==k] == "too_low")) {
    lins.tit <- "too low for Lin's CCC"
    print("too low")
    
    n.counters.pre <- length(grep(paste0("_", k, "_"), all_counts))
    n.counters <- length(unique(c(as.character(lins[lins$VideoLine==k, c("Initials1")]), as.character(lins[lins$VideoLine==k, c("Initials2")]))))
    
    print(paste0("Pre-Lins of counters: ", n.counters.pre))
    
    
  } else {
    n.counters.pre <- length(grep(paste0("_", k, "_"), all_counts))
    n.counters <- length(unique(c(as.character(lins[lins$VideoLine==k, c("Initials1")]), as.character(lins[lins$VideoLine==k, c("Initials2")]))))
    
    print(paste0("Pre-Lins counters: ", n.counters.pre))
  }

  print(paste0("Post-Lins counters: ", n.counters))

  if(n.counters > 1) {
    
    stn.all <- grep(all_counts, pattern=paste0("_", as.character(k), "_"), value=T)
    
    comb <- combinations(n = n.counters, r = 2, v = stn.all, repeats.allowed = F)
    
    for(j in 1:nrow(comb)) {
    
      stn <- comb[j,]
      
      cnt1 <- read.csv(stn[1])
      cnt1 <- cbind(b_id=paste0(rep("b_1_", nrow(cnt1)), 1:nrow(cnt1)), cnt1) # unique identifier for each burrow
      ID1 <- as.character(cnt1$counter_ID[1])
      cnt1 <- subset(cnt1, cnt1$minute %in% confirmed_minutes)
      
      # lins[lins$VideoLine==k, names(lins) %in% paste0("min_", 1:7)]
      
      cnt2 <- read.csv(stn[2])
      cnt2 <- cbind(b_id=paste0(rep("b_2_", nrow(cnt2)), 1:nrow(cnt2)), cnt2) # unique identifier for each burrow
      ID2 <- as.character(cnt2$counter_ID[1])
      cnt2 <- subset(cnt2, cnt2$minute %in% confirmed_minutes)
      
      both <- rbind(cnt1, cnt2) # for later plot
      names(cnt1) <- paste0(names(cnt1), "_1")
      names(cnt2) <- paste0(names(cnt2), "_2")
      
      cnt1[,c("b_id_1", "survey_1", "station_1", "counter_ID_1", "time_1", 
              "feature_1", "y_1", "annotation_time_1", "VideoOperatorID_1",
              "minute_1")] <- lapply(cnt1[,c("b_id_1", "survey_1", "station_1", "counter_ID_1", "time_1", 
                                             "feature_1", "y_1", "annotation_time_1", "VideoOperatorID_1",
                                             "minute_1")], as.character)
      cnt2[,c("b_id_2", "survey_2", "station_2", "counter_ID_2", "time_2", 
              "feature_2", "y_2", "annotation_time_2", "VideoOperatorID_2",
              "minute_2")] <- lapply(cnt2[,c("b_id_2", "survey_2", "station_2", "counter_ID_2", "time_2", 
                                             "feature_2", "y_2", "annotation_time_2", "VideoOperatorID_2",
                                             "minute_2")], as.character)
      
      
      
      
      
      
      
      b_id_grid <- expand.grid(b_id_1=cnt1$b_id_1, b_id_2=cnt2$b_id_2)
      
      counter_ID_grid <- expand.grid(counter_ID_1=cnt1$counter_ID_1, counter_ID_2=cnt2$counter_ID_2)
      
      
      still_grid <- expand.grid(still_n_1=cnt1$still_n_1, still_n_2=cnt2$still_n_2)
      still_grid$still_dif <- abs(still_grid$still_n_1 - still_grid$still_n_2)
      
      x_grid <- expand.grid(x_1=cnt1$x_1, x_2=cnt2$x_2)
      x_grid$x_dif <- abs(x_grid$x_1 - x_grid$x_2)
      
      fin_grid <- cbind(b_id_grid, counter_ID_grid, still_grid, x_grid)
      
      if (nrow(fin_grid[ ! fin_grid$still_dif > still_accu[m],]) == 0){
        
        print ("no matches because too far in stills")
        
        cnt1_non <- cnt1
        cnt2_non <- cnt2
        cnt1_non <- cbind(cnt1_non[, c("b_id_1", "counter_ID_1", "still_n_1", "x_1")], NA, NA, NA, NA, "no")
        names(cnt1_non) <- c("b_id", "counter_ID", "still_n", "x", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")
        cnt2_non <- cbind(cnt2_non[, c("b_id_2", "counter_ID_2", "still_n_2", "x_2")], NA, NA, NA, NA, "no")
        names(cnt2_non) <- c("b_id", "counter_ID", "still_n", "x", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")
        
        final <- rbind(cnt1_non, cnt2_non)
        
        write.csv(fin_grid, paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "/match_annotations_tables/matches_table_stn_", k, "_", ID1, "_", ID2, "_no_matches_too_far_in_stills.csv"), row.names=F)
        
        
      } else {
        
        fin_grid <- fin_grid[ ! fin_grid$still_dif > still_accu[m],]
        
        
        if (nrow(fin_grid[ ! fin_grid$x_dif > x_accu[l],]) == 0) {
          
          print ("no matches because too far in x")
          
          cnt1_non <- cnt1
          cnt2_non <- cnt2
          cnt1_non <- cbind(cnt1_non[, c("b_id_1", "counter_ID_1", "still_n_1", "x_1")], NA, NA, NA, NA, "no")
          names(cnt1_non) <- c("b_id", "counter_ID", "still_n", "x", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")
          cnt2_non <- cbind(cnt2_non[, c("b_id_2", "counter_ID_2", "still_n_2", "x_2")], NA, NA, NA, NA, "no")
          names(cnt2_non) <- c("b_id", "counter_ID", "still_n", "x", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")

          final <- rbind(cnt1_non, cnt2_non)
          
          write.csv(fin_grid, paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "/match_annotations_tables/matches_table_stn_", k, "_", ID1, "_", ID2, "_NO_MATCHES_too_far_in_x.csv"), row.names=F)
          
        } else {
          
          fin_grid <- fin_grid[ ! fin_grid$x_dif > x_accu[l],]
          
          fin_grid <- fin_grid[order(fin_grid$still_dif, fin_grid$x_dif),] # preference to still_n
          # fin_grid <- fin_grid[order(fin_grid$x_dif, fin_grid$still_dif),] # preference to x
          
          match_grid <- fin_grid[match(unique(fin_grid$b_id_1), fin_grid$b_id_1),]
          match_grid <- match_grid[match(unique(match_grid$b_id_2), match_grid$b_id_2),]
          
          
          for (i in 1:nrow(match_grid)) {
            match_grid$box_ymin[i] <- min(match_grid$still_n_1[i], match_grid$still_n_2[i]) - box_y
            match_grid$box_ymax[i] <- max(match_grid$still_n_1[i], match_grid$still_n_2[i]) + box_y
            match_grid$box_xmin[i] <- min(match_grid$x_1[i], match_grid$x_2[i]) - box_x
            match_grid$box_xmax[i] <- max(match_grid$x_1[i], match_grid$x_2[i]) + box_x
          }
          
          match_grid$match <- "yes"
          
          write.csv(match_grid, paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "/match_annotations_tables/matches_table_stn_", k, "_", ID1, "_", ID2, ".csv"), row.names=F)
          
          
          cnt1_match <- match_grid[, c("b_id_1", "counter_ID_1", "still_n_1", "x_1", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")]
          cnt2_match <- match_grid[, c("b_id_2", "counter_ID_2", "still_n_2", "x_2", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")]
          names(cnt1_match) <- c("b_id", "counter_ID", "still_n", "x", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")
          names(cnt2_match) <- c("b_id", "counter_ID", "still_n", "x", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")
          
          cnt1_non <- cnt1[ ! cnt1$b_id_1 %in% match_grid$b_id_1, ]
          cnt2_non <- cnt2[ ! cnt2$b_id_2 %in% match_grid$b_id_2, ]
          
          
          
          if (nrow(cnt1_non) == 0 & nrow(cnt2_non) == 0) {
            
            print("perfect match for both counters")
            
            final <- rbind(cnt1_match, cnt2_match)
          
          } else if (nrow(cnt1_non) == 0) {
            
            print("perfect match for 1st counter")
            
            cnt2_non <- cbind(cnt2_non[, c("b_id_2", "counter_ID_2", "still_n_2", "x_2")], NA, NA, NA, NA, "no")
            names(cnt2_non) <- c("b_id", "counter_ID", "still_n", "x", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")
            
            final <- rbind(cnt1_match, cnt2_match, cnt2_non)
            
          } else if (nrow(cnt2_non) == 0) {
            
            print("perfect match for 2nd counter")
            
            cnt1_non <- cbind(cnt1_non[, c("b_id_1", "counter_ID_1", "still_n_1", "x_1")], NA, NA, NA, NA, "no")
            names(cnt1_non) <- c("b_id", "counter_ID", "still_n", "x", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")
            
            final <- rbind(cnt1_non, cnt1_match, cnt2_match)
            
          } else {
            
            cnt1_non <- cbind(cnt1_non[, c("b_id_1", "counter_ID_1", "still_n_1", "x_1")], NA, NA, NA, NA, "no")
            names(cnt1_non) <- c("b_id", "counter_ID", "still_n", "x", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")
            
            
            cnt2_non <- cbind(cnt2_non[, c("b_id_2", "counter_ID_2", "still_n_2", "x_2")], NA, NA, NA, NA, "no")
            names(cnt2_non) <- c("b_id", "counter_ID", "still_n", "x", "box_ymin", "box_ymax", "box_xmin", "box_xmax", "match")
            
            
            final <- rbind(cnt1_non, cnt1_match, cnt2_match, cnt2_non)
            
            }}}
            
            # Anonymise plots
            final$counter_ID <- as.factor(final$counter_ID)
            orig.names <- levels(final$counter_ID)
            # levels(final$counter_ID) <- c("Anna", "John") # if you want anonymised
            # anon1 <- "Anna" # if you want anonymised
            # anon2 <- "John" # if you want anonymised
            # anon1 <- unique(final$counter_ID[order(final$still_n)])[1] # if you want real names
            # anon2 <- unique(final$counter_ID[order(final$still_n)])[2] # if you want real names
            anon1 <- unique(final$counter_ID)[1] # if you want real names
            anon2 <- unique(final$counter_ID)[2] # if you want real names
  
            final$kol <- factor(paste0(final$counter_ID, "_", final$match),
                                levels=c(paste0(anon1, "_no"), paste0(anon1, "_yes"),
                                         paste0(anon2, "_yes"), paste0(anon2, "_no")))
            
            
            perc <- as.data.frame(cbind(table(final$kol),
                                        round(table(final$kol)/c(rep(nrow(cnt1), 2), rep(nrow(cnt2), 2)) * 100)))
            names(perc) <- c("n", "%")
            
            perc_total <- rbind(perc_total,
                                cbind(k,
                                      perc[paste0(anon1, "_no"),"n"], perc[paste0(anon1, "_yes"),"n"],
                                      perc[paste0(anon1, "_no"),"%"], perc[paste0(anon1, "_yes"),"%"],
                                      perc[paste0(anon2, "_no"),"n"], perc[paste0(anon2, "_yes"),"n"],
                                      perc[paste0(anon2, "_no"),"%"], perc[paste0(anon2, "_yes"),"%"]))
            
            
            lins.tit <- lins$LinsCCC[lins$VideoLine == k & lins$Initials1 %in% c(ID1, ID2) & lins$Initials2 %in% c(ID1, ID2)]
            
            ggplot(final, aes(x=x, y=still_n, colour=kol)) +
              geom_point(size = 5) +
              geom_label(aes(label=still_n), size = 7) +
              #scale_shape_manual(values=c(15, 17, 0)) +
              #scale_size_manual(values=c(3,3,10)) +
              #scale_colour_manual(values=c("mediumvioletred", "orangered", "deeppink", "orange")) +
              scale_colour_manual("ID and match?", values=c("green3", "orangered", "orange", "steelblue")) +
              {if(sum(is.na(final$box_xmin) == F) != 0){geom_rect(mapping=aes(xmin=box_xmin, xmax=box_xmax, ymin=box_ymin, ymax=box_ymax), fill=NA, col="red")}} +
              annotation_custom(tableGrob(perc, theme = ttheme_default(base_size = 20, core=list(bg_params = list(fill=c("green3", "orangered", "orange", "steelblue"))))),
                                xmin=1100, xmax=1250, ymin=max(final$still_n)-3000, ymax=max(final$still_n)) +
              labs(title = paste0("Station ", k, ";  Lin's CCC = ", as.character(lins.tit), ";  Minutes: ", paste(confirmed_minutes, collapse = "_")),
                   y = "Still number", x = "Screen x axis") +
              theme(legend.position = "none", plot.title = element_text(hjust=0.5, face="bold", size = 20)) +
              # theme(legend.justification="bottom", plot.title = element_text(hjust=0.5, face="bold"), legend.text=element_text(size=20)) +
              xlim(c(0,1229)) # max pixel number possible
            
            plot_name <- paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "/match_annotations_plots/matches_plot_stn_", k, "_", ID1, "_", ID2, ".png")
            ggsave(plot_name, width=16, height=9)
            
        }
      }
}
  names(perc_total) <- c("stn",
                         paste0(anon1, "_no_n"), paste0(anon1, "_yes_n"),
                         paste0(anon1, "_no_%"), paste0(anon1, "_yes_%"),
                         paste0(anon2, "_no_n"), paste0(anon2, "_yes_n"),
                         paste0(anon2, "_no_%"), paste0(anon2, "_yes_%"))
  write.csv(perc_total, paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "_percentages.csv"), row.names=F)
}
}
# Sys.time()-old.time





# To see overall results of matches for lots of stations

# per_list <-  list()
# cont <- 0
# 
# for (l in 1:length(x_accu)) {
# 
#   print(x_accu[l])
#   
#   for (m in 1:length(still_accu)) {
#     
#     cont <- cont + 1
#     
#     print(still_accu[m])
#     
#     cur <- read.csv(paste0("./match_x_", x_accu[l], "/match_still_", still_accu[m], "_percentages.csv"))
#     cur <- cur[,c("stn", "Anna_yes_.", "Anna_no_.", "John_yes_.", "John_no_.")]
#     print(cur[,2]+cur[,3])
#     print(cur[,4]+cur[,5])
#     names(cur) <- c("stn",
#                     paste0("A_yes_", x_accu[l], ".", still_accu[m]), paste0("A_no_", x_accu[l], ".", still_accu[m]),
#                     paste0("J_yes_", x_accu[l], ".", still_accu[m]), paste0("J_no_", x_accu[l], ".", still_accu[m]))
#     per_list[[cont]] <- cbind(pair=1:nrow(cur), melt(cur, id.vars = c("stn")))
#     per_list[[cont]]$x_accu <- gsub(".*[_]([^.]+)[.].*", "\\1", per_list[[cont]]$variable)
#     per_list[[cont]]$still_accu <- gsub(".*[.]([^.]+).*", "\\1", per_list[[cont]]$variable)
#     per_list[[cont]]$match <- gsub(".*[_]([^.]+)[_].*", "\\1", per_list[[cont]]$variable)
# 
#   }
# }
# 
# 
# per <- do.call(rbind, per_list)
# per$stn <- factor(per$stn)
# per$x_accu <- factor(per$x_accu, levels=x_accu)
# per$still_accu <- factor(per$still_accu, levels=still_accu)
# 
# 
# ggplot(per, aes(still_accu, value, fill=match)) +
#   geom_boxplot() +
#   theme_classic() +
#   labs(x="x axis accuracy-range", y="percentage")
# 
# ggplot(per, aes(x_accu, value, fill=match)) +
#   geom_boxplot() +
#   theme_classic() +
#   labs(x="x axis accuracy-range", y="percentage")
# 
# 
# 
# ggplot(per, aes(match, value, fill=match)) +
#   geom_boxplot() +
#   theme_classic() +
#   labs(x="x axis accuracy-range", y="percentage") +
#   facet_wrap(~pair)

