library('scales')
library('tibble')
library('dplyr')
library('ggplot2')

ChurnStackedBar = setRefClass("ChurnStackedBar",
  fields = list(dataset = 'tbl_df'),
  methods = list(
    initialize = function(dataset) {
      .self$dataset = dataset
    },
    
    plot = function(title, xText, column) {
      dataset.churn <- filter(dataset, Exited == 1);
      dataset.notchurn <- filter(dataset, Exited == 0);
      
      table.churn.column <- table(dataset.churn[[column]]);
      table.notchurn.column <- table(dataset.notchurn[[column]]);
      
      groups = unique(c(names(table.notchurn.column), names(table.churn.column)))
      stack_bar_values = list(
        churn_count = 1:length(groups),
        not_count_count = 1:length(groups),
        churn_percentage = 1:length(groups),
        not_churn_percentage = 1:length(groups)
      )
      
      for(i in 1:length(groups)) {
        churn_count = if(groups[i] %in% names(table.churn.column)) table.churn.column[groups[i]] else 0;
        not_churn_count = if(groups[i] %in% names(table.notchurn.column)) table.notchurn.column[groups[i]] else 0;
        total = churn_count + not_churn_count
        stack_bar_values[["churn_count"]][i] = churn_count
        stack_bar_values[["not_count_count"]][i] = not_churn_count
        stack_bar_values[["churn_percentage"]][i] = percent(churn_count/total)
        stack_bar_values[["not_churn_percentage"]][i] = percent(not_churn_count/total)
      }
      
      df = data.frame(
        churn_category = rep(c("Churn", "Not Churn"), each = length(groups)),
        group = if(!is.na(as.numeric(groups[1]))) as.numeric(c(groups, groups)) else c(groups, groups),
        count = c(stack_bar_values$churn_count, stack_bar_values$not_count_count),
        percentual = c(stack_bar_values$not_churn_percentage, stack_bar_values$churn_percentage)
      )
      
      p <- ggplot(df, aes(x = group, y = count)) +
        labs(x = xText, y = "Quantidade", title = title) +
        geom_col(aes(fill = churn_category), width = 0.7) +
        geom_text(aes(y = count, label = percentual, group = churn_category), color = "white") +
        geom_hline(yintercept = mean(table.notchurn.column), linetype = "dashed") +
        geom_text(aes(0, mean(table.notchurn.column), label = "Média de não-churn"))
      p
      ggsave(paste(column, ".png", sep = ""), width = 10, height = 10)
    }
  )
)