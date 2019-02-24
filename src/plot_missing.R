plot_missing <- function(data, title = NULL) {
  temp_df <- as.data.frame(ifelse(is.na(data), 0, 1))
  temp_df <- temp_df[, order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  
  ggplot(data_temp) + 
    geom_tile(aes(x = x, y = y, fill = factor(m))) + 
    scale_fill_manual(values = c("white", "black"), name="Missing\n(0=Yes, 1=No)") + 
    theme_light() + 
    ylab("") + xlab("") + ggtitle(title)
}
