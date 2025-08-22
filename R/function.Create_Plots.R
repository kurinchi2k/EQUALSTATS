function.Create_Plots <- function(Predefined_lists, rv){
  # Lists
  plan <- {cbind.data.frame(
    analysis_number = paste0("AN", formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
    first_menu_choice = rv$first_menu_choice,
    second_menu_choice = rv$second_menu_choice,
    entry_1 = paste0(rv$entry[[1]], collapse = "%_%"),
    entry_2 = paste0(rv$entry[[2]], collapse = "%_%"),
    entry_3 = paste0(rv$entry[[3]], collapse = "%_%"),
    entry_4 = paste0(rv$entry[[4]], collapse = "%_%"),
    entry_5 = paste0(rv$entry[[5]], collapse = "%_%"),
    entry_6 = paste0(rv$entry[[6]], collapse = "%_%"),
    entry_7 = paste0(rv$entry[[7]], collapse = "%_%"),
    entry_8 = paste0(rv$entry[[8]], collapse = "%_%"),
    entry_9 = paste0(rv$entry[[9]], collapse = "%_%"),
    entry_10 = paste0(rv$entry[[10]], collapse = "%_%"),
    entry_11 = paste0(rv$entry[[11]], collapse = "%_%"),
    entry_12 = paste0(rv$entry[[12]], collapse = "%_%"),
    entry_13 = paste0(rv$entry[[13]], collapse = "%_%"),
    entry_14 = paste0(rv$entry[[14]], collapse = "%_%"),
    entry_15 = paste0(rv$entry[[15]], collapse = "%_%"),
    same_row_different_row = ""
  )}
  selections <- {paste0(
    '<b>entry_1: </b>', paste0(rv$entry[[1]], collapse = "; "), '<br>',
    '<b>entry_2: </b>', paste0(rv$entry[[2]], collapse = "; "), '<br>',
    '<b>entry_3: </b>', paste0(rv$entry[[3]], collapse = "; "), '<br>',
    '<b>entry_4: </b>', paste0(rv$entry[[4]], collapse = "; "), '<br>'
  )}
  code <- {paste0(
    '# AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '\n',
    'rv$first_menu_choice <- "', rv$first_menu_choice, '"\n',
    'rv$second_menu_choice <- ', ifelse(is.na(rv$second_menu_choice),NA,paste0('"',rv$second_menu_choice, '"')), '\n',
    'rv$entry[[1]] <- ', ifelse(length(rv$entry[[1]]) > 1,
                                paste0('c("', paste0(rv$entry[[1]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[1]],'"')), '\n',
    'rv$entry[[2]] <- ', ifelse(length(rv$entry[[2]]) > 1,
                                paste0('c("', paste0(rv$entry[[2]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[2]],'"')), '\n',
    'rv$entry[[3]] <- ', ifelse(length(rv$entry[[3]]) > 1,
                                paste0('c("', paste0(rv$entry[[3]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[3]],'"')), '\n',
    'rv$entry[[4]] <- ', ifelse(length(rv$entry[[4]]) > 1,
                                paste0('c("', paste0(rv$entry[[4]], collapse = '", "'), '")'),
                                paste0('"',rv$entry[[4]],'"')), '\n',
    'AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_results <- function.',rv$first_menu_choice,'(Predefined_lists, rv)', '\n',
    if(length(rv$plan) == 0){
      'if (TRUE %in% (AN0001_results$plots_list != "")) {invisible(file.rename(AN0001_results$plots_list, paste0(AN0001_results$plots_list,"_copy")))}
'
    } else {
      paste0(
        'AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$results[2,1] <- "AN',formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'"', '\n',
        'if (TRUE %in% (AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$plots_list != "")) {invisible(file.rename(AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$plots_list, str_replace_all(AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$plots_list, "/AN0001_", "/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_")))}', '\n')
    },
    'write.table(x = AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0),'_results$results, append = TRUE, file = paste0(rv$StorageFolder, "/results.csv"), sep = ",", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE, qmethod = "double")', '\n'
  )}
  # Prepare the data
  plot_title <- if (rv$entry[[3]] !="") {
    rv$entry[[3]]
  } else {
    if (rv$second_menu_choice == "EQUAL-STATS choice") {
      paste0(rv$entry[[1]], ": ", "Boxplot")
    } else {
      paste0(rv$entry[[1]], ": ", rv$second_menu_choice)
    }
  }
  # Create the data file depending on whether the plots must be produced for separate categories
  if (rv$entry[[4]] != "") {
    data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[4]])]
  } else {
    data <- data.frame(rv$import_data$data[,rv$entry[[1]]])
    colnames(data) <- rv$entry[[1]]
  }
  data <- na.omit(data)
  # Depending on the second menu choice, the remaining vary
  if (rv$second_menu_choice == "EQUAL-STATS choice") {
    if ((rv$entry[[1]] %in% rv$import_data$quantitative) & (rv$entry[[4]] == "")) {
      plot <-ggplot(data, aes(y= data[,1])) + geom_boxplot(color="blue", fill="lightblue") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ylab(rv$entry[[1]]) + ggtitle(plot_title)
    } else if ((rv$entry[[1]] %in% rv$import_data$quantitative) & (rv$entry[[4]] != "")) {
      plot <-ggplot(data, aes(x= data[,2], y=data[,1], fill= data[,2])) + geom_bar(position = "dodge", stat="identity") + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ylab(rv$entry[[1]]) + xlab(rv$entry[[4]]) + ggtitle(plot_title) + labs(fill=rv$entry[[4]])
    } else {
      # For easy writing, create a dummy variable when there is no group
      if (rv$entry[[4]] == "") {
        data[,2] <- as.factor(rep("OnlyGroup", nrow(data)))
      }
      # Create a new column where the aggregate data is calculated - use 1 to start with
      data[,3] <- rep(1, nrow(data))
      data_2 <- aggregate(data[,3]~data[,1]+ data[,2],data=data,FUN=sum)
      # Now the plots
      if (rv$entry[[4]] != "") {
        plot_title <- paste(plot_title, rv$entry[[4]], sep="\n")
        plot <- ggplot(data_2, aes(x= data_2[,2], y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge())+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge(width = 0.7), vjust = -0.5) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
      } else {
        plot <- ggplot(data_2, aes(x= "", y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge())+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge(width = 0.7), vjust = -0.5) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
      }
    }
  } else if (rv$second_menu_choice == "Histogram") {
    if (rv$entry[[4]] != "") {
      plot <- ggplot(data, aes(x= data[,1], colour= data[,2])) + geom_histogram(alpha = 0.3, position = "identity", binwidth=1, fill="white", aes(y=after_stat(density))) + geom_density() + xlab(rv$entry[[1]]) + ggtitle(plot_title) + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust=0.5)) + labs(colour = data[,2])
    } else {
      plot <- ggplot(data, aes(x= data[,1])) + geom_histogram(binwidth=1, color="blue", fill="lightblue", aes(y=after_stat(density))) + geom_density(color="navyblue") + xlab(rv$entry[[1]]) + ggtitle(plot_title) + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust=0.5))
    }
  } else if (rv$second_menu_choice == "Boxplot") {
    if (rv$entry[[4]] != "") {
      plot <-ggplot(data, aes(x= data[,2], y= data[,1] , colour= data[,2])) + geom_boxplot() + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + xlab(rv$entry[[4]]) + ylab(rv$entry[[1]]) + ggtitle(plot_title) + labs(colour=rv$entry[[4]])
    } else {
      plot <-ggplot(data, aes(y= data[,1])) + geom_boxplot(color="blue", fill="lightblue") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ylab(rv$entry[[1]]) + ggtitle(plot_title)
    }
  } else if (rv$second_menu_choice == "Vertical bar chart") {
    if (rv$entry[[1]] %in% rv$import_data$categorical) {
      # Create a clustered vertical bar chart
      # For easy writing, create a dummy variable when there is no group
      if (rv$entry[[4]] == "") {
        data[,2] <- as.factor(rep("OnlyGroup", nrow(data)))
      }
      # Create a new column where the aggregate data is calculated - use 1 to start with
      data[,3] <- rep(1, nrow(data))
      data_2 <- aggregate(data[,3]~data[,1]+ data[,2],data=data,FUN=sum)
      # Now the plots
      if (rv$entry[[4]] != "") {
        plot_title <- paste(plot_title, rv$entry[[4]], sep="\n")
        plot <- ggplot(data_2, aes(x= data_2[,2], y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge())+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge(width = 0.7), vjust = -0.5) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
      } else {
        plot <- ggplot(data_2, aes(x= "", y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge())+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge(width = 0.7), vjust = -0.5) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
      }
    } else {
      if (rv$entry[[4]] != "") {
        plot <-ggplot(data, aes(x= data[,2], y=data[,1], fill= data[,2])) + geom_bar(position = "dodge", stat="identity") + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ylab(rv$entry[[1]]) + xlab(rv$entry[[4]]) + ggtitle(plot_title) + labs(fill=rv$entry[[4]])
      } else {
        plot <-ggplot(data, aes(x= factor(1), y=data[,1])) + geom_bar(position = "dodge", stat="identity", show.legend = FALSE, fill = "lightblue") + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ylab(rv$entry[[1]]) + xlab(rv$entry[[4]]) + ggtitle(plot_title)
      }
    }
  } else if (rv$second_menu_choice == "Horizontal bar chart") {
    if (rv$entry[[1]] %in% rv$import_data$categorical) {
      # Create a clustered horizontal bar chart
      if (rv$entry[[4]] == "") {
        data[,2] <- as.factor(rep("OnlyGroup", nrow(data)))
      }
      # Create a new column where the aggregate data is calculated - use 1 to start with
      data[,3] <- rep(1, nrow(data))
      data_2 <- aggregate(data[,3]~data[,1]+ data[,2],data=data,FUN=sum)
      # Now the plots
      if (rv$entry[[4]] != "") {
        plot_title <- paste(plot_title, rv$entry[[4]], sep="\n")
        plot <- ggplot(data_2, aes(x= data_2[,2], y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge2(reverse=TRUE))+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge2(width = 0.7, reverse=TRUE), hjust = 1) + labs(fill=rv$entry[[1]]) + scale_fill_discrete() +coord_flip()
      } else {
        plot <- ggplot(data_2, aes(x= "", y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge2(reverse=TRUE))+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge2(width = 0.7, reverse=TRUE), hjust = 1) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()+coord_flip()
      }
    } else {
      if (rv$entry[[4]] != "") {
        plot <- ggplot(data, aes(x= data[,2], y= data[,1], fill= data[,2], group= data[,2]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge())+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + scale_fill_discrete() + coord_flip()
      } else {
        plot <-ggplot(data, aes(x= factor(1), y=data[,1])) + geom_bar(position = "dodge", stat="identity", show.legend = FALSE, fill = "lightblue") + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ylab(rv$entry[[1]]) + ggtitle(plot_title) + coord_flip()
      }
    }
  } else if (rv$second_menu_choice == "Vertical bar chart with error bars") {
    # if there is a group, means and SD must be calculated for each group
    if (rv$entry[[4]] != "") {
      mean_each_level <- sapply(1:nlevels(data[,2]), function(x) {
        mean(data[data[,2]==levels(data[,2])[x],1])
      })
      sd_each_level <- sapply(1:nlevels(data[,2]), function(x) {
        sd(data[data[,2]==levels(data[,2])[x],1])
      })
      data_2 <- data.frame(Level=levels(data[,2]), Mean = mean_each_level, SD = sd_each_level)
      plot <-ggplot(data_2, aes(x= data_2[,1], y=data_2[,2], fill= data_2[,1])) + geom_bar(position=position_dodge(.9), stat="identity") + geom_errorbar(aes(ymin= data_2[,2]- data_2[,3], ymax= data_2[,2]+ data_2[,3]), width=.2, position=position_dodge(.9))+theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ylab(rv$entry[[1]]) + xlab(rv$entry[[4]]) + ggtitle(plot_title) + labs(fill=rv$entry[[4]])
    } else {
      data_2 <- data.frame(Level=rv$entry[[1]], Mean = mean(data[,1]), SD = sd(data[,1]))
      plot <-ggplot(data_2, aes(x= data_2[,1], y=data_2[,2], fill= data_2[,1])) + geom_bar(stat="identity", show.legend = FALSE) + geom_errorbar(aes(ymin= data_2[,2]- data_2[,3], ymax= data_2[,2]+ data_2[,3]), width=.2, position=position_dodge(.9))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ylab(rv$entry[[1]]) + ggtitle(plot_title)
    }
  } else if (rv$second_menu_choice == "Pie chart") {
    if (rv$entry[[4]] == "") {
      data[,2] <- as.factor(rep("OnlyGroup", nrow(data)))
    }
    # Create a new column where the aggregate data is calculated - use 1 to start with
    data[,3] <- rep(1, nrow(data))
    data_2 <- aggregate(data[,3]~data[,1]+ data[,2],data=data,FUN=sum)
    # To suppress values < 3% per level of 2nd variable to avoid cluttering, a new column is created that has same value of numbers
    data_2[,4] <- ifelse(data_2[,3]/(sum(data_2[,3] / nlevels(data_2[,2])))<0.03,NA, data_2[,3])
    # Now the plots
    if (rv$entry[[4]] != "") {
      plot_title <- paste(plot_title, rv$entry[[4]], sep="\n")
      plot <- ggplot(data_2, aes(x="", y= data_2[,3], fill= data_2[,1]))+ geom_bar(width = 1, stat = "identity", position = position_stack(reverse = TRUE)) + coord_polar("y", start=0) + geom_col(position = position_stack(reverse = TRUE)) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(y= data_2[,4],label = paste0(round(data_2[,4]/nrow(data),2)*100,"%")), size=3, position = position_stack(reverse = TRUE, vjust =0.5)) + facet_grid(.~ data_2[,2]) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
    } else {
      plot <- ggplot(data_2, aes(x="", y= data_2[,3], fill= data_2[,1]))+ geom_bar(width = 1, stat = "identity", position = position_stack(reverse = TRUE)) + coord_polar("y", start=0) + geom_col(position = position_stack(reverse = TRUE)) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(y= data_2[,4],label = paste0(round(data_2[,4]/nrow(data),2)*100,"%")), size=3, position = position_stack(reverse = TRUE, vjust =0.5)) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
    }
  } else if (rv$second_menu_choice == "Stacked bar chart") {
    # For easy writing, create a dummy variable when there is no group
    if (rv$entry[[4]] == "") {
      data[,2] <- as.factor(rep("OnlyGroup", nrow(data)))
    }
    # Create a new column where the aggregate data is calculated - use 1 to start with
    data[,3] <- rep(1, nrow(data))
    data_2 <- aggregate(data[,3]~data[,1]+ data[,2],data=data,FUN=sum)
    # To suppress values < 3% per level of 2nd variable to avoid cluttering, a new column is created that has same value of numbers
    data_2[,4] <- ifelse(data_2[,3]/(sum(data_2[,3] / nlevels(data_2[,2])))<0.03,NA, data_2[,3])
    # Now the plots
    if (rv$entry[[4]] != "") {
      plot_title <- paste(plot_title, rv$entry[[4]], sep="\n")
      plot <- ggplot(data_2, aes(x="", y= data_2[,3], fill= data_2[,1]))+ geom_bar(width = 1, stat = "identity", position = position_stack(reverse = TRUE)) + geom_col(position = position_stack(reverse = TRUE)) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(y= data_2[,4],label = paste0(round(data_2[,4]/nrow(data),2)*100,"%")), size=3, position = position_stack(reverse = TRUE, vjust =0.5)) + facet_grid(.~ data_2[,2]) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
    } else {
      plot <- ggplot(data_2, aes(x="", y= data_2[,3], fill= data_2[,1]))+ geom_bar(width = 1, stat = "identity", position = position_stack(reverse = TRUE)) + geom_col(position = position_stack(reverse = TRUE)) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(y= data_2[,4],label = paste0(round(data_2[,4]/nrow(data),2)*100,"%")), size=3, position = position_stack(reverse = TRUE, vjust =0.5))+ labs(fill=rv$entry[[1]]) + scale_fill_discrete()
    }
  } else if (rv$second_menu_choice == "Clustered vertical bar chart") {
    # For easy writing, create a dummy variable when there is no group
    if (rv$entry[[4]] == "") {
      data[,2] <- as.factor(rep("OnlyGroup", nrow(data)))
    }
    # Create a new column where the aggregate data is calculated - use 1 to start with
    data[,3] <- rep(1, nrow(data))
    data_2 <- aggregate(data[,3]~data[,1]+ data[,2],data=data,FUN=sum)
    # Now the plots
    if (rv$entry[[4]] != "") {
      plot_title <- paste(plot_title, rv$entry[[4]], sep="\n")
      plot <- ggplot(data_2, aes(x= data_2[,2], y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge())+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge(width = 0.7), vjust = -0.5) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
    } else {
      plot <- ggplot(data_2, aes(x= "", y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge())+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge(width = 0.7), vjust = -0.5) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()
    }
  } else if (rv$second_menu_choice == "Clustered horizontal bar chart") {
    # For easy writing, create a dummy variable when there is no group
    if (rv$entry[[4]] == "") {
      data[,2] <- as.factor(rep("OnlyGroup", nrow(data)))
    }
    # Create a new column where the aggregate data is calculated - use 1 to start with
    data[,3] <- rep(1, nrow(data))
    data_2 <- aggregate(data[,3]~data[,1]+ data[,2],data=data,FUN=sum)
    # Now the plots
    if (rv$entry[[4]] != "") {
      plot_title <- paste(plot_title, rv$entry[[4]], sep="\n")
      plot <- ggplot(data_2, aes(x= data_2[,2], y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge2(reverse=TRUE))+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge2(width = 0.7, reverse=TRUE), hjust = 1) + labs(fill=rv$entry[[1]]) + scale_fill_discrete() +coord_flip()
    } else {
      plot <- ggplot(data_2, aes(x= "", y= data_2[,3], fill= data_2[,1], group= data_2[,1]))+ geom_bar(width = 0.7, stat = "identity", position = position_dodge2(reverse=TRUE))+ theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks.x=element_blank(), plot.title = element_text(color="navyblue", size=14, face="bold", hjust = 0.5)) + ggtitle(plot_title) + labs(fill=rv$entry[[1]]) + geom_text(aes(label = paste0(round(data_2[,3]/nrow(data),2)*100,"%")), size=3, position = position_dodge2(width = 0.7, reverse=TRUE), hjust = 1) + labs(fill=rv$entry[[1]]) + scale_fill_discrete()+coord_flip()
    }
  } else if (rv$second_menu_choice == "Scatterplot") {
    # This has the second variable - this must be included as the second variable - Simplest way, create data again
    if (rv$entry[[4]] != "") {
      data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]], rv$entry[[4]])]
    } else {
      data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]])]
    }
    data <- na.omit(data)
    if (rv$entry[[4]] != "") {
      plot <- suppressMessages(suppressWarnings(try(ggplot(data, aes(x= data[,1], y = data[,2], color= data[,3], shape = data[,3])) + geom_point()+ geom_smooth(aes(fill= data[,3], linetype= data[,3]))+ xlab(rv$entry[[1]]) + ylab(rv$entry[[2]]) + ggtitle(plot_title) + theme_classic() + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust=0.5)) + labs(fill=rv$entry[[4]], color=rv$entry[[4]], shape= rv$entry[[4]], linetype=rv$entry[[4]]), silent = TRUE)))
    } else {
      plot <- suppressMessages(suppressWarnings(try(ggplot(data, aes(x= data[,1], y = data[,2])) + geom_point()+ geom_smooth()+ xlab(rv$entry[[1]]) + ylab(rv$entry[[2]]) + ggtitle(plot_title) + theme_classic() + theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust=0.5)), silent = TRUE)))
    }
  }  else if (rv$second_menu_choice == "Before-After") {
    # This has the second variable - this must be included as the second variable - Simplest way, create data again
    if (rv$entry[[4]] != "") {
      data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]], rv$entry[[4]])]
    } else {
      data <- rv$import_data$data[,c(rv$entry[[1]], rv$entry[[2]])]
    }
    data <- na.omit(data)
    if (rv$entry[[4]] != "") {
      for (i in 1:nlevels(data[,3])){
        assign(paste0("df",i), subset(data, data[,3] %in% levels(data[,3])[i]))
        assign(paste0("plot",i), as_grob(suppressWarnings(ggplot()
                                                          + geom_segment(aes(x = 1, xend = 2, y = eval(parse(text=paste0("df",i)))[,1], yend = eval(parse(text=paste0("df",i)))[,2]))
                                                          + scale_x_discrete(breaks = c("1", "2"), labels = c(rv$entry[[1]], rv$entry[[2]]), limits = c(1, 2))
                                                          + labs(y = "") + labs(x = "")
                                                          + ggtitle(paste0(rv$entry[[4]], " (",levels(data[,3])[i], ")" ))
                                                          + theme(plot.title = element_text(color="navyblue", size=10, face="bold", hjust=0.5)))))
      }
      plot_title_combined <- ggdraw() + draw_label(plot_title, color="navyblue", size=14, fontface="bold", hjust = 0.5) + theme(plot.margin = margin(0, 0, 0, 7))
      plot <- plot_grid(plot_title_combined, plot_grid(plotlist=mget(paste0("plot", 1: nlevels(data[,3]))), nrow = 2) + theme(plot.background = element_rect(fill = "white", colour = NA)), ncol=1, rel_heights = c(0.1,1)) + theme(plot.background = element_rect(fill = "white", colour = NA))
    } else {
      plot<- suppressWarnings(ggplot() + geom_segment(aes(x = 1, xend = 2, y = data[,1], yend = data[,2])) + scale_x_discrete(breaks = c("1", "2"), labels = c(rv$entry[[1]], rv$entry[[2]]), limits = c(1, 2)) + labs(y = "") + labs(x = "")+ ggtitle(plot_title)+ theme(plot.title = element_text(color="navyblue", size=14, face="bold", hjust=0.5)))
    }
  }
  # Now save the plot
  # Only one plot for this function
  suppressWarnings(suppressMessages(ggsave(filename = paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_', substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 100) ,'.png'), plot = plot)))
  plots_list <- paste0(rv$StorageFolder,'/AN', formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0), '_', substr(str_replace_all(plot_title, "[^[:alnum:]]","_"), 1, 100) ,'.png')
  plots_list_display <- plots_list
  results_display <- data.frame(`Analysis number` = paste0("AN",formatC((length(rv$plan) + 1), width = 4, format = "d", flag = 0)),
                                `Analysis type` = paste0(Predefined_lists$main_menu[Predefined_lists$menu_short ==  rv$first_menu_choice], ifelse(! is.na(rv$second_menu_choice), paste0(": ", rv$second_menu_choice), "")),
                                `Analysis outcome` = "Successful", `Variable name` = rv$entry[[1]], check.names = FALSE)
  results <- rbind.data.frame(colnames(results_display), results_display)
  analysis_outcome <- "Successful"
  display_table <- TRUE
  display_plot <- TRUE
  function_output <- list(analysis_outcome = analysis_outcome, plan = plan, code = code, results = results, results_display = results_display, plots_list = plots_list,   plots_list_display =   plots_list_display, selections = selections, display_table = display_table, display_plot = display_plot)
  return(function_output)
}
