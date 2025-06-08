library(shiny)
library(DT)
library(bslib)
library(dplyr)
library(rlang)
library(htmlTable)
library(shinyjs)
library(DBI)
library(RSQLite)
library(data.table)
library(sendmailR)
library(shinybusy)
library(lsr)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(shinyWidgets)
library(rhandsontable)
library(readxl)
library(gghalves)
library(plotly)
library(shinydashboard)


get_label = function(acolumn.data, default.label = ''){
  if(!is.na(Hmisc::label(acolumn.data)) & (Hmisc::label(acolumn.data) != "")){
    return(Hmisc::label(acolumn.data))
  }else{
    return(default.label)
  }
}

get_df_labels = function(df, vars = colnames(df)){
  labels.to.return = c()
  for(avar in vars){
    a.label = get_label(df[,avar])
    if(a.label != ""){
      labels.to.return = append(labels.to.return, a.label)
    }else{
      labels.to.return = append(labels.to.return, avar)
    }
  }
  return(as.vector(labels.to.return))
}
my_cor_table = function(data, vars = colnames(data), type = "pearson", tri = 'all', digits = 3, dontshowbelow = 0, show.only.significant = FALSE, show.only.first.var = FALSE){
  data = data[sapply(data, is.numeric)]
  vars = intersect(vars, colnames(data))
  
  library(scipub)
  atable = correltable(data[vars], method = type, tri = tri, round_n = digits)
  colnames(atable$table) = get_df_labels(data, vars)
  rownames(atable$table) = get_df_labels(data, vars)
  
  label.type = "Pearson"
  if(type == 'spearman'){
    label.type = "Spearman"
  }
  caption.str = paste0('<b>', label.type, ' Correlation Coefficients</b> <small>(&ast;:p<.05, &ast;&ast;:p<.01, &ast;&ast;&ast;:p<.001)</small>')
  if(show.only.first.var == FALSE){
    return(htmlTable::htmlTable(atable$table, caption = caption.str))
  }
  else{
    a.df = as.data.frame(atable$table[, 1])
    colnames(a.df) = 'Correlation'
    return(htmlTable::htmlTable(a.df, caption = caption.str))
  }
}

p.str = function(p){
  if(p < 0.001){
    return("p < 0.001")
  }else{
    return(paste0('p = ', round(p, 3)))
  }
}

my_check_normality_of_vector = function(avector, caption = '<b>Normality statistics</b>', type = 'short'){
  
  skew.message = '<small>Normal distribution has skew = 0. For a unimodal distribution, negative skew commonly indicates that the tail is on the left side of the distribution, and positive skew indicates that the tail is on the right.</small>'
  kurtosis.message = '<small>Normal distribution has kurtosis = 3. Values over 3 indicates a platykurtic distribution and values less than 3 indicates a leptokurtic distribution.</small>'
  agostino.message = '<small>Agostino test: Η<sub>0</sub>: This sample is from a distribution with Skew = 0 vs Η<sub>1</sub>: Not the Η<sub>0</sub>.</small>'
  anscombe.glynn.message = '<small>Anscombe-Glynn test: Η<sub>0</sub>: This sample is from a distribution with Kurtosis = 3 vs Η<sub>1</sub>: Not the Η<sub>0</sub>.</small>'
  shapiro.wilk.message = '<small>This test is more appropriate method for small sample sizes (<50 samples) although it can also be handling on larger sample size.</small>'
  kolmogorov.smirnov.message = '<small>This test is used for n ≥ 50. However it is not appropriate when values are repeated.</small>'
  lilliefors.message = '<small>The Lilliefors test uses the same calculations as the Kolmogorov-Smirnov test, but it is more conservative in the sense that the Lilliefors Test is less likely to show that data is normally distributed.</small>'
  
  skew.report = paste0(round(moments::skewness(avector, na.rm = T), 3))
  kurtosis.report = paste0(round(moments::kurtosis(avector, na.rm = T), 3))
  agostino.report = 'An Error Occurred'
  try({   agostino.test = moments::agostino.test(avector, alternative = "two.sided")
  agostino.report = paste0('z = ', round(agostino.test$statistic[2], 3), ', ', p.str(agostino.test$p.value))}, silent=TRUE)
  anscombe.glynn.report = 'An Error Occurred'
  try({   anscombe.glynn.test = moments::anscombe.test(avector, alternative = "two.sided")
  anscombe.glynn.report = paste0('z = ', round(anscombe.glynn.test$statistic[2], 3), ', ', p.str(anscombe.glynn.test$p.value))}, silent=TRUE)
  
  shapiro.wilk.test = shapiro.test(avector)
  shapiro.wilk.report = paste0('W = ', round(shapiro.wilk.test$statistic, 3), ', ', p.str(shapiro.wilk.test$p.value))
  
  kolmogorov.smirnov.report <- 'K-S test not run'
  try({
    ks.test.res <- ks.test(avector, "pnorm", 
                           mean = mean(avector, na.rm = TRUE), 
                           sd = sd(avector, na.rm = TRUE))
    ks.text <- paste0('D = ', round(ks.test.res$statistic, 3), ', ', p.str(ks.test.res$p.value))
    if (any(duplicated(avector))) {
      ks.text <- paste0(ks.text, ' (⚠️ Warning: Ties present)')
    }
    kolmogorov.smirnov.report <- ks.text
  }, silent = TRUE)
  
  
  lilliefors.test = nortest::lillie.test(avector)
  lilliefors.report = paste0('D = ', round(lilliefors.test$statistic, 3), ', ', p.str(lilliefors.test$p.value))
  
  if(type == 'long'){
    a.table = matrix(c(skew.report, skew.message,
                       kurtosis.report, kurtosis.message,
                       agostino.report, agostino.message,
                       anscombe.glynn.report, anscombe.glynn.message,
                       shapiro.wilk.report, shapiro.wilk.message,
                       kolmogorov.smirnov.report, kolmogorov.smirnov.message,
                       lilliefors.report, lilliefors.message), ncol = 2, byrow=TRUE,
                     dimnames = list(c("Skewness", 'Kurtosis', 'Agostino', 'Anscombe-Glynn', 'Shapiro–Wilk', 'Kolmogorov–Smirnov', 'Lilliefors'), c("Statistic", "Description")))
    
    return(htmlTable::htmlTable(caption = caption, a.table, align = 'l',  rgroup = c('Skewness & Kurtosis', 'Normality Tests. Η<sub>0</sub>: This sample is from a normal distribution vs H<sub>1</sub>: Not the Η<sub>0</sub>.'), n.rgroup = c(4, 3)))
  }
  if(type == 'short'){
    a.table = matrix(c(skew.report, skew.message,
                       kurtosis.report, kurtosis.message,
                       shapiro.wilk.report, shapiro.wilk.message,
                       lilliefors.report, lilliefors.message), ncol = 2, byrow=TRUE,
                     dimnames = list(c("Skewness", 'Kurtosis', 'Shapiro–Wilk', 'Lilliefors'), c("Statistic", "Description")))
    
    return(htmlTable::htmlTable(caption = caption, a.table, align = 'l',  rgroup = c('Skewness & Kurtosis', 'Normality Tests. Η<sub>0</sub>: This sample is from a normal distribution vs Η<sub>1</sub>: Not the Η<sub>0</sub>.'), n.rgroup = c(2, 2)))
  }
  
}

my_check_homogeneity_of_column = function(data, column.to.describe, afactor){
  library(dplyr)
  library(htmlTable)
  if(length(afactor) > 1){
    interaction.factor = data[,afactor[1]]
    for(onefactor in afactor[-1]){
      interaction.factor = interaction(interaction.factor, data[,onefactor])
    }
    afactor.labels = get_df_labels(data, afactor)
    data['interaction.factor'] = interaction.factor
    Hmisc::label(data[,'interaction.factor']) = paste0('Interaction of ', paste0(afactor.labels, collapse = ', '))
    afactor = 'interaction.factor'
  }
  
  
  column.label = get_label(data[, column.to.describe], column.to.describe)
  factor.label = get_label(data[, afactor], afactor)
  
  # Decriptive SD table
  dataframe = data.frame(column.to.describe = data[,column.to.describe], afactor = data[,afactor])
  dataframe = na.omit(dataframe)
  groups.data = dataframe %>% group_by(afactor) %>%
    dplyr::summarize(freq1 = length(column.to.describe),
                     sd = signif(sd(column.to.describe), 3))
  colnames(groups.data) = c('Level', 'Group Size', 'SD')
  descriptive.table = htmlTable(t(groups.data), caption = paste0('<b>Standard Deviation of ', column.label, ' among levels of ', factor.label, '</b>'))
  
  ahomogeneity.test = formula(paste(column.to.describe, " ~ ", paste0(afactor, collapse = '*')))
  # Levene Test
  ltest = car::leveneTest(ahomogeneity.test, data)
  if(ltest$`Pr(>F)`[1] > 0.05){
    result.message = 'Homogeneity hypothesis is confirmed.'
  }else{
    result.message = 'Homogeneity hypothesis is not supported!'
  }
  ltest.report = paste0('F(', ltest$Df[1], ', ', ltest$Df[2],') = ', round(ltest$`F value`[1], 3), ', p = ', round(ltest$`Pr(>F)`[1], 3), '. ', result.message)
  ltable = htmlTable::htmlTable(ltest.report, align = 'l', caption = paste0('<b>Levene test of variance equality (homogeneity) of ', column.label, ' over ', factor.label, '</b>'))
  
  # Bartlett test. The Levene test is less sensitive than the Bartlett test to departures from normality. If you have strong evidence that your data do in fact come from a normal, or nearly normal, distribution, then Bartlett's test has better performance.
  btable = htmlTable::htmlTable('Bartlett test can not be executed!', align = 'l', caption = paste0("<b>Bartlett's test of variance equality (homogeneity) of ", column.label, ' over ', factor.label, "</b><br><small>If you have strong evidence that your data do in fact come from a normal, or nearly normal, distribution, then Bartlett's test has better performance.</small>"))
  try({
    btest = bartlett.test(ahomogeneity.test, data)
    if(btest$p.value > 0.05){
      result.message = 'Homogeneity hypothesis is confirmed.'
    }else{
      result.message = 'Homogeneity hypothesis is not supported!'
    }
    btest.report = paste0('c<sup>2</sup>(', btest$parameter,') = ', round(btest$statistic, 3), ', p = ', round(btest$p.value, 3), '. ', result.message)
    btable = htmlTable::htmlTable(btest.report, align = 'l', caption = paste0("<b>Bartlett's test of variance equality (homogeneity) of ", column.label, ' over ', factor.label, "</b><br><small>If you have strong evidence that your data do in fact come from a normal, or nearly normal, distribution, then Bartlett's test has better performance.</small>"))
  })
  return(htmlTable::concatHtmlTables(list(descriptive.table, ltable, btable), headers = c('', '', '')))
}






my_ANOVA = function(data, model, posthoc = c(), normalitytest = TRUE, digits = 3,
                    check_all_factors = TRUE, max.number.of.levels.to.show = 10, show.eta.square = FALSE) {
  
  options(contrasts = c("contr.sum", "contr.poly"))
  
  
  create_factors = function(df, max.number.of.distinct.values.to.consider.as.factor = 10) {
    df.to.return = as.data.frame(df)
    for (aVar in colnames(df)) {
      if (!is.factor(df.to.return[[aVar]]) &&
          dplyr::n_distinct(df.to.return[[aVar]]) <= max.number.of.distinct.values.to.consider.as.factor) {
        df.to.return[[aVar]] = haven::as_factor(df.to.return[[aVar]])
      }
    }
    return(df.to.return)
  }
  
  data <- na.omit(data[all.vars(model)])
  data <- create_factors(data, max.number.of.levels.to.show)
  res.aov <- lm(model, data = data)
  
  table1 <- round(car::Anova(res.aov, type = "III"), digits = digits)
  table2 <- round(lsr::etaSquared(aov(res.aov), type = 3), digits = digits)
  
  if (show.eta.square) {
    table.anova <- cbind(table1, rbind(c(NA, NA), table2, c(NA, NA)))
    colnames(table.anova) <- c('SS', 'df', 'F', 'p', 'η<sup>2</sup>', 'η<sup>2</sup><sub>p</sub>')
  } else {
    table.anova <- table1
    colnames(table.anova) <- c('SS', 'df', 'F', 'p')
  }
  
  table.anova$p <- sapply(table.anova$p, function(p) ifelse(is.na(p), NA, ifelse(p < 0.001, '<0.001', p)))
  
  dependent.variable <- all.vars(model)[1]
  str1 <- get_df_labels(data, dependent.variable)
  tables.to.concat <- list()
  tables.to.concat[[1]] <- htmlTable::htmlTable(table.anova, caption = paste0("<b>ANOVA Results for ", str1, "</b>"))
  counter <- 2
  
  if (check_all_factors) {
    posthoc <- all.vars(model)[-1]
  }
  
  if (length(posthoc) > 1) {
    data$.interaction <- interaction(data[[posthoc[1]]], data[[posthoc[2]]])
    Hmisc::label(data$.interaction) <- paste("Interaction of", paste(posthoc, collapse = " and "))
    posthoc <- c(posthoc, ".interaction")
  }
  
  all.factors <- c()
  all.continuous.vars <- c()
  
  for (afactor in posthoc) {
    if (!afactor %in% names(data)) next
    
    str.factor <- get_df_labels(data, afactor)
    one_way_formula <- as.formula(paste(dependent.variable, "~", afactor))
    safe_model <- tryCatch(lm(one_way_formula, data = data), error = function(e) NULL)
    if (is.null(safe_model)) next
    
    LSD.test <- tryCatch(agricolae::LSD.test(safe_model, afactor, alpha = 0.05), error = function(e) NULL)
    if (is.null(LSD.test)) next
    
    if (nrow(LSD.test$groups) <= max.number.of.levels.to.show) {
      all.factors <- append(all.factors, afactor)
      
      LSD.test$groups <- LSD.test$groups[order(row.names(LSD.test$groups)), ]
      LSD.test$means <- LSD.test$means[order(row.names(LSD.test$means)), ]
      LSD.test$groups[, 1] <- round(LSD.test$groups[, 1], digits)
      LSD.test$groups[, 3] <- LSD.test$means$r
      names(LSD.test$groups) <- c(str1, "Groups", "N")
      tables.to.concat[[counter]] <- htmlTable::htmlTable(
        LSD.test$groups[c("N", str1, "Groups")],
        caption = paste0("<b>LSD Test for variable: ", str.factor, "</b>")
      )
      counter <- counter + 1
      
      TUKEY.model <- tryCatch(aov(one_way_formula, data = data), error = function(e) NULL)
      if (!is.null(TUKEY.model)) {
        TK_data <- tryCatch(as.data.frame(TukeyHSD(TUKEY.model)[[1]]), error = function(e) NULL)
        if (!is.null(TK_data)) {
          TK_data <- round(TK_data, digits)
          names(TK_data) <- c("Difference", "Lower", "Upper", "p")
          TK_data$p <- sapply(TK_data$p, function(p) ifelse(is.na(p), NA, ifelse(p < 0.001, "<0.001", p)))
          
          tables.to.concat[[counter]] <- htmlTable::htmlTable(
            TK_data,
            caption = paste0("<b>Tukey HSD for variable: ", str.factor, "</b>")
          )
          counter <- counter + 1
        }
      }
    } else {
      all.continuous.vars <- append(all.continuous.vars, afactor)
    }
  }
  
  if (length(all.continuous.vars) > 0) {
    corr.tables <- my_cor_table(data, c(dependent.variable, all.continuous.vars))
    tables.to.concat[[counter]] <- corr.tables
    counter <- counter + 1
  }
  
  if (normalitytest) {
    avector <- res.aov$residuals
    tables.to.concat[[counter]] <- my_check_normality_of_vector(avector, caption = '<b>Normality test for model residuals.</b><br><small>If the main goal of an ANOVA is to see whether or not certain effects are significant, then the assumption of normality of the residuals is only required for small samples, thanks to the central limit theorem. With sample sizes of a few hundred participants even extreme violations of the normality assumptions are unproblematic. So mild violations of this assumptions are usually no problem with sample sizes exceeding 30.</small>')
    counter <- counter + 1
  }
  
  list.to.return <- list()
  list.to.return$htmlTable <- concatHtmlTables(tables.to.concat, headers = rep('', counter - 1))
  list.to.return$res.aov <- res.aov
  return(list.to.return)
}





global_data <- reactiveVal(NULL)

server <- function(input, output, session){
  
  
  
  rv <- reactiveValues(aov_model = NULL)
  
  
  
  anova.report <- reactiveVal('')
  
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    ext <- tools::file_ext(inFile$datapath)
    
    data <- switch(ext,
                   csv = read.csv(inFile$datapath, header = TRUE),
                   xlsx = readxl::read_excel(inFile$datapath),
                   {
                     showNotification("Unsupported file type. Please upload .csv or .xlsx", type = "error")
                     return(NULL)
                   }
    )
    
    data <- as.data.frame(data)  # Ensure consistency
    global_data(data)
    data
  })
  
  
  output$contents <- DT::renderDataTable({
    DT::datatable(myData())       
  })
  
  output$boxplot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    # Create interaction if two factors
    if (length(ind) == 2) {
      interaction_var <- interaction(df[[ind[1]]], df[[ind[2]]], drop = TRUE)
      df$Interaction <- factor(interaction_var)
      ggplot(df, aes(x = Interaction, y = .data[[dep]])) +
        geom_boxplot(fill = "lightblue") +
        theme_minimal() +
        labs(
          title = paste("Boxplot of", dep, "by", paste(ind, collapse = " × ")),
          x = paste(ind[1], "*", ind[2]),
          y = dep
        )
    } else {
      df[[ind]] <- factor(df[[ind]])
      ggplot(df, aes_string(x = ind, y = dep)) +
        geom_boxplot(fill = "lightblue") +
        theme_minimal() +
        labs(
          title = paste("Boxplot of", dep, "by", ind),
          x = ind,
          y = dep
        )
    }
  })
  
  
  
  output$multiVariChart <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    if (length(input$indvar) != 2) {
      plot.new()
      text(0.5, 0.5, "Multi-vari chart requires two independent variables.", cex = 1.2)
      return()
    }
    
    df <- global_data()
    dep <- input$depvar
    ind1 <- input$indvar[1]
    ind2 <- input$indvar[2]
    
    df[[ind1]] <- factor(df[[ind1]])
    df[[ind2]] <- factor(df[[ind2]])
    
    ggplot(df, aes_string(x = ind1, y = dep, color = ind2, group = ind2)) +
      stat_summary(fun = mean, geom = "point", size = 3) +
      stat_summary(fun = mean, geom = "line") +
      theme_minimal() +
      labs(title = "Multi-vari Chart", x = ind1, y = dep, color = ind2)
  })
  
  
  
  output$histPlot <- renderPlot({
    req(global_data(), input$depvar)
    df <- global_data()
    dep <- input$depvar
    
    ggplot(df, aes_string(x = dep)) +
      geom_histogram(fill = "#cb2893", color = "black", bins = 30) +
      theme_minimal() +
      labs(title = "Histogram of Dependent Variable", x = dep, y = "Count")
  })
  
  output$violinPlot <- plotly::renderPlotly({
    req(global_data(), input$depvar, input$indvar)
    
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    if (!is.numeric(df[[dep]])) {
      try({ df[[dep]] <- as.numeric(as.character(df[[dep]])) }, silent = TRUE)
    }
    if (!is.numeric(df[[dep]])) return(NULL)
    
    # One or two group interaction
    group_var <- if (length(ind) == 2) {
      interaction(df[[ind[1]]], df[[ind[2]]], drop = TRUE)
    } else {
      factor(df[[ind]])
    }
    
    df$Group <- group_var
    
    # υπολογισμος στατιστικών 
    stats_df <- df %>%
      group_by(Group) %>%
      summarise(
        Mean = mean(.data[[dep]], na.rm = TRUE),
        SD = sd(.data[[dep]], na.rm = TRUE),
        N = n(),
        SEM = SD / sqrt(N),
        CI_lower = Mean - 1.96 * SEM,
        CI_upper = Mean + 1.96 * SEM,
        .groups = "drop"
      )
    
    # σε ggplot
    p <- ggplot(df, aes(x = Group, y = .data[[dep]], fill = Group)) +
      geom_violin(alpha = 0.5, show.legend = FALSE) +
      geom_jitter(width = 0.15, alpha = 0.5, color = "gray30", show.legend = FALSE) +
      geom_point(data = stats_df, aes(x = Group, y = Mean, text = paste0(
        "Group: ", Group, "<br>",
        "Mean: ", round(Mean, 2), "<br>",
        "SD: ", round(SD, 2), "<br>",
        "CI: [", round(CI_lower, 2), ", ", round(CI_upper, 2), "]"
      )), size = 2.5, color = "black", inherit.aes = FALSE) +
      geom_errorbar(data = stats_df, aes(x = Group, ymin = CI_lower, ymax = CI_upper), 
                    width = 0.15, color = "black", inherit.aes = FALSE) +
      theme_minimal() +
      labs(title = "Interactive Violin Plot with Mean ± CI",
           x = "Group", y = dep)
    
    
    ggplotly(p, tooltip = "text") #ggplot to ggplotly
  })
  
  
  
  output$raincloudPlot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    group_var <- if (length(ind) == 2) {
      interaction(df[[ind[1]]], df[[ind[2]]], drop = TRUE)
    } else {
      factor(df[[ind]])
    }
    
    df$Group <- group_var
    
    ggplot(df, aes(x = Group, y = .data[[dep]], fill = Group)) +
      gghalves::geom_half_violin(side = "l", alpha = 0.5) +
      geom_boxplot(width = 0.1, outlier.shape = NA) +
      gghalves::geom_half_point(side = "r", alpha = 0.4) +
      theme_minimal() +
      labs(title = "Raincloud Plot", x = "Group", y = dep)
  })
  
  
  
  
  
  output$qqplot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    # create model formula
    formula_text <- paste(dep, "~", paste(ind, collapse = "*"))
    model <- my_ANOVA(df, as.formula(formula_text))$res.aov
    
    # QQ Plot
    qqnorm(model$residuals, main = "QQ Plot of Residuals")
    qqline(model$residuals, col = "red", lwd = 2)
  })
  
  
  output$residualsPlot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    # two way anova
    formula_text <- paste(dep, "~", paste(ind, collapse = "*"))
    model <- my_ANOVA(df, as.formula(formula_text))$res.aov
    
    # Residuals vs Fitted plot
    plot(model$fitted.values, model$residuals,
         xlab = "Fitted Values", ylab = "Residuals",
         main = "Residuals vs Fitted")
    abline(h = 0, col = "red", lty = 2)
  })
  
  output$shapiroPlot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    formula_text <- paste(dep, "~", paste(ind, collapse = "*"))
    model <- lm(as.formula(formula_text), data = df)
    
    resids <- model$residuals
    
    hist(resids, breaks = 20, main = "Histogram of Residuals (Shapiro-Wilk)",
         xlab = "Residuals", col = "lightblue", border = "white")
    mtext(sprintf("Shapiro-Wilk p = %.3f", shapiro.test(resids)$p.value), side = 3, line = 0.5)
  })
  
  output$lillieforsPlot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    formula_text <- paste(dep, "~", paste(ind, collapse = "*"))
    model <- lm(as.formula(formula_text), data = df)
    
    resids <- model$residuals
    
    # CDF για Lilliefos
    hist(resids, breaks = 20, probability = TRUE,
         main = "Lilliefors Test: Residuals vs Normal", col = "lightpink", border = "white",
         xlab = "Residuals")
    curve(dnorm(x, mean(resids), sd(resids)), col = "darkred", lwd = 2, add = TRUE)
    mtext(sprintf("Lilliefors p = %.3f", nortest::lillie.test(resids)$p.value), side = 3, line = 0.5)
  })
  
  output$shapiroByGroupPlot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    # inderaction between group
    group_var <- if (length(ind) == 2) {
      interaction(df[[ind[1]]], df[[ind[2]]], drop = TRUE)
    } else {
      df[[ind]]
    }
    
    df$Group <- factor(group_var)
    df <- df %>% dplyr::select(Group, !!sym(dep)) %>% na.omit()
    
    result <- by(df[[dep]], df$Group, function(x) {
      if (length(x) >= 3 && length(unique(x)) > 2) {
        tryCatch(shapiro.test(x)$p.value, error = function(e) NA)
      } else {
        NA
      }
    })
    
    df_plot <- data.frame(
      Group = names(result),
      P_value = as.numeric(result)
    )
    
    ggplot(df_plot, aes(x = Group, y = P_value)) +
      geom_bar(stat = "identity", fill = "#5dade2") +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Shapiro-Wilk Normality Test by Group",
           y = "p-value", x = "Group",) +
    theme(plot.title = element_text(face="bold"))
  })
  
  
  output$lillieforsByGroupPlot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    # Create interaction group 
    group_var <- if (length(ind) == 2) {
      interaction(df[[ind[1]]], df[[ind[2]]], drop = TRUE)
    } else {
      df[[ind]]
    }
    
    df$Group <- factor(group_var)
    df <- df %>% dplyr::select(Group, !!sym(dep)) %>% na.omit()
    
    result <- by(df[[dep]], df$Group, function(x) {
      if (length(x) >= 4 && length(unique(x)) > 2) {
        tryCatch(nortest::lillie.test(x)$p.value, error = function(e) NA)
      } else {
        NA
      }
    })
    
    df_plot <- data.frame(
      Group = names(result),
      P_value = as.numeric(result)
    )
    
    ggplot(df_plot, aes(x = Group, y = P_value)) +
      geom_bar(stat = "identity", fill = "#48c9b0") +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Lilliefors Normality Test by Group",
           y = "p-value", x = "Group") +
      theme(plot.title = element_text(face="bold"))
  })
  
  
  
  
  output$meansCLPlot <- plotly::renderPlotly({
    req(global_data(), input$depvar, input$indvar)
    
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    if (!is.numeric(df[[dep]])) {
      try({ df[[dep]] <- as.numeric(as.character(df[[dep]])) }, silent = TRUE)
    }
    if (!is.numeric(df[[dep]])) return(NULL)
    
    if (length(ind) == 2) {
      df$Group <- interaction(df[[ind[1]]], df[[ind[2]]], drop = TRUE)
    } else if (length(ind) == 1) {
      df$Group <- factor(df[[ind]])
    } else return(NULL)
    
    # Summary
    summary_df <- df %>%
      group_by(Group) %>%
      summarise(
        Mean = mean(.data[[dep]], na.rm = TRUE),
        N = n(),
        SD = sd(.data[[dep]], na.rm = TRUE),
        SEM = SD / sqrt(N),
        CI_lower = Mean - 1.96 * SEM,
        CI_upper = Mean + 1.96 * SEM,
        .groups = "drop"
      )
    
    # if one way add tukeys
    if (length(ind) == 1) {
      aov_model <- aov(as.formula(paste(dep, "~ Group")), data = df)
      tukey_result <- agricolae::HSD.test(aov_model, "Group", group = TRUE)
      letters_df <- data.frame(Group = rownames(tukey_result$groups),
                               Letter = tukey_result$groups$groups)
      summary_df <- merge(summary_df, letters_df, by = "Group", all.x = TRUE)
    } else {
      summary_df$Letter <- ""
    }
    
    plotly::plot_ly(
      data = summary_df,
      x = ~Group,
      y = ~Mean,
      type = "scatter",
      mode = "markers+text",
      text = ~Letter,
      textposition = "top center",
      error_y = ~list(type = "data", array = CI_upper - Mean, arrayminus = Mean - CI_lower),
      hoverinfo = "text",
      hovertext = ~paste0(
        "Group: ", Group, "<br>",
        "Mean: ", round(Mean, 2), "<br>",
        "SD: ", round(SD, 2), "<br>",
        "N: ", N, "<br>",
        "95% CI: [", round(CI_lower, 2), ", ", round(CI_upper, 2), "]"
      )
    ) %>%
      layout(
        title = "Means ± 95% CI with Tukey Groups",
        xaxis = list(title = "Group"),
        yaxis = list(title = paste("Mean of", dep))
      )
  })
  
  
  
  output$etasquarePlot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    # multiple independent variables
    formula_text <- paste(dep, "~", paste(ind, collapse = "*"))
    model_formula <- as.formula(formula_text)
    
    model <- my_ANOVA(df, model = model_formula, show.eta.square = TRUE)
    
    eta_vals <- lsr::etaSquared(aov(model$res.aov), type = 3)
    eta_df <- as.data.frame(eta_vals)
    eta_df$Term <- rownames(eta_df)
    
    ggplot(eta_df, aes(x = reorder(Term, eta.sq), y = eta.sq)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Effect Sizes (η²)", x = "Effect", y = "η²")
  })
  
  output$tukeyPlot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    if (length(ind) != 1) {
      plot.new()
      text(0.5, 0.5, "Tukey HSD only applies to single-factor ANOVA", cex = 1.2)
      return()
    }
    
    if (!is.numeric(df[[dep]])) {
      try({ df[[dep]] <- as.numeric(as.character(df[[dep]])) }, silent = TRUE)
    }
    if (!is.numeric(df[[dep]])) return(NULL)
    
    df[[ind]] <- factor(trimws(as.character(df[[ind]])))
    if (length(unique(na.omit(df[[ind]]))) < 2) return(NULL)
    
    aov_model <- aov(as.formula(paste(dep, "~", ind)), data = df)
    tukey_result <- TukeyHSD(aov_model)
    
    plot(tukey_result, las = 1, col = "steelblue")
  })
  
  
  

  
  
  output$assumptionTests <- renderUI({
    tagList(
      htmlOutput("levbartTestTable"),
      plotOutput("sdPlot"),
      plotOutput("residualSpreadPlot")
    )
  })
  
  output$levbartTestTable <- renderUI({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    htmltools::HTML(
      my_check_homogeneity_of_column(df, input$depvar, input$indvar)
    )
  })
  
  output$sdPlot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    # interaction if two way
    group_var <- if (length(ind) == 2) {
      interaction(df[[ind[1]]], df[[ind[2]]], drop = TRUE)
    } else {
      factor(df[[ind]])
    }
    
    df$Group <- group_var
    
    sd_df <- df %>%
      group_by(Group) %>%
      summarise(SD = sd(.data[[dep]], na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(sd_df, aes(x = Group, y = SD)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      theme_minimal() +
      labs(title = "Standard Deviation per Group",
           x = "Group", y = "Standard Deviation")
  })
  
  output$residualSpreadPlot <- renderPlot({
    req(global_data(), input$depvar, input$indvar)
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    if (!is.numeric(df[[dep]])) {
      try({ df[[dep]] <- as.numeric(as.character(df[[dep]])) }, silent = TRUE)
    }
    if (!is.numeric(df[[dep]])) return(NULL)
    
    #interaction if needed 
    group_var <- if (length(ind) == 2) {
      interaction(df[[ind[1]]], df[[ind[2]]], drop = TRUE)
    } else {
      factor(df[[ind]])
    }
    
    df$Group <- group_var
    model <- lm(as.formula(paste(dep, "~ Group")), data = df)
    
    plot_data <- data.frame(
      Group = df$Group,
      AbsResidual = abs(residuals(model))
    )
    
    ggplot(plot_data, aes(x = Group, y = AbsResidual)) +
      geom_boxplot(fill = "#fbc531") +
      theme_minimal() +
      labs(
        title = "Spread of Absolute Residuals by Group (Levene Visual)",
        x = "Group", y = "Absolute Residual"
      )
  })
  
  output$aboutinfo <- renderUI({
    HTML('
    <div style="line-height: 1.6; font-size: 15px;">
      <h3><strong>ANOVA</strong></h3>
      <p>
        ANOVA stands for <strong>Analysis of Variance</strong>. It is a statistical method used to analyze the differences between the means of two or more groups. ANOVA determines whether there are any significant differences between group means based on the variation within and between groups.
      </p>
      <p>
        The core idea is to compare the variation <strong>between</strong> groups to the variation <strong>within</strong> groups. If between-group variation is significantly larger, we can conclude that some group means differ.
      </p>
      
      <h4><strong>How ANOVA Works</strong></h4>
      <ul>
        <li>Calculates an <strong>F-statistic</strong>: the ratio of between-group variance to within-group variance.</li>
        <li>If the F-value is large and the <em>p-value</em> is below a significance level (e.g. 0.05), the null hypothesis is rejected.</li>
        <li>The null hypothesis assumes all group means are equal.</li>
      </ul>
      <img src="one_way_anova_example.png" style="width:100%; max-width:600px; border:1px solid #ccc; padding:5px; margin:10px 0;" alt="One-way ANOVA example">

      <h4><strong>One-way ANOVA</strong></h4>
      <p>Used when comparing means of 3 or more groups on a single independent factor.</p>
      
      <p><strong>Hypotheses:</strong></p>
      <ul>
        <li><strong>H₀:</strong> μ₁ = μ₂ = ... = μₖ (all means are equal)</li>
        <li><strong>H₁:</strong> At least one group mean is different</li>
      </ul>

      <h4><strong>Two-way ANOVA</strong></h4>
      <p>Used when examining the effect of two factors and their interaction.</p>
      
      <ul>
        <li>Assesses <strong>main effects</strong> of each factor and their <strong>interaction effect</strong>.</li>
        <li>Example: gender × treatment on test scores.</li>
      </ul>
      
      <img src="two_way_anova.png" style="width:100%; max-width:600px; border:1px solid #ccc; padding:5px; margin:10px 0;" alt="Two-way ANOVA interaction">

      <h4><strong>Assumptions for ANOVA</strong></h4>
      <ul>
        <li><strong>Independence</strong>: Observations must be independent.</li>
        <li><strong>Normality</strong>: Residuals are approximately normally distributed.</li>
        <li><strong>Homogeneity of variance</strong>: Variances across groups are equal.</li>
      </ul>

      <h3><strong>How to Use the App</strong></h3>
      <ol>
        <li><strong>Upload a .csv or .xlsx file</strong> using the "Upload Data File" input.</li>
        <li><strong>Select your dependent variable</strong> (must be numeric).</li>
        <li><strong>Select one or two independent variables</strong> (must be factors).</li>
        <li>Click <strong>"Perform ANOVA"</strong> to run the analysis.</li>
        <li>Use the various tabs to:</li>
        <ul>
          <li>View the ANOVA results and model summary</li>
          <li>Check statistical assumptions</li>
          <li>Explore interactive plots like boxplot, violin, raincloud, histogram</li>
          <li>Visualize group means with confidence intervals</li>
        </ul>
        <li>Click <strong>"Download Report"</strong> to generate a formatted HTML report of your analysis.</li>
      </ol>
      <hr>
      <h4><strong>About this App</strong></h4>
      <p>
        This application was developed as part of my Integrated Master`s (IM) thesis project titled:<br>
        <em>“Design of an application implementing the ANOVA method and providing the results to the user in natural language”</em><br>
      </p>
      <p><strong>Developer:</strong> Jorsidi Halili<br>
         <strong>Department:</strong> Department of Electrical and Computer Engineering<br>
         <strong>University:</strong> Democritus University of Thrace<br>
         <strong>Year:</strong> 2025<br>
         <strong>Contact:</strong> <a href="mailto:chalgior@gmail.com">chalgior@gmail.com</a>
      </p>
    </div>
')})
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("anova_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      tryCatch({
        req(global_data())
        
        dep <- input$depvar
        ind <- input$indvar
        df <- global_data()
        
        if (is.null(dep) || dep == "") {
          showNotification("⚠️ Independent variable not chosen.", type = "error")
          return(NULL)
        }
        if (is.null(ind) || ind == "") {
          showNotification("⚠️ Dependent variable not chosen.", type = "error")
          return(NULL)
        }
        
        # metroph se arithmhtiko ans xreiaxzeta
        if (!is.numeric(df[[dep]])) {
          try({ df[[dep]] <- as.numeric(as.character(df[[dep]])) }, silent = TRUE)
        }
        if (!is.numeric(df[[dep]])) {
          showNotification("The independent variable must be a number!", type = "error")
          return(NULL)
        }
        
        if (length(ind) == 1 && length(unique(na.omit(df[[ind]]))) < 2) {
          showNotification("The independent variable must have at least 2 levels!", type = "error")
          return(NULL)
        }
        
        
        model_formula <- as.formula(paste(dep, "~", ind))
        anova_result <- my_ANOVA(data = df, model = model_formula, show.eta.square = TRUE)
        
        tempReport <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
        file.copy("helpers.R", file.path(tempdir(), "helpers.R"), overwrite = TRUE)
        
        
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = list(
                            model_formula = model_formula,
                            dataset = df,
                            anova_output = anova_result
                            
                          ),
                          envir = new.env(parent = globalenv()))
        
        showNotification("📄 Report created successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ Report creation error:", e$message), type = "error")
      })
    })
  
  
  
  
  
  observeEvent(myData(), {
    df <- global_data()
    
    # Identify numeric and non-numeric columns
    is_numeric <- sapply(df, is.numeric)
    
    # Only numeric variables for dependent
    dep_choices <- names(df)[is_numeric]
    
    # Only non-numeric variables for independent
    indep_choices <- names(df)[!is_numeric]
    
    # Update dependent picker
    shinyWidgets::updatePickerInput(
      session,
      "depvar",
      choices = dep_choices,
      selected = NULL,
      options = list(
        `live-search` = TRUE
      )
    )
    
    # Update independent picker
    shinyWidgets::updatePickerInput(
      session,
      "indvar",
      choices = indep_choices,
      selected = NULL,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE
      )
    )
  })
  
  
  
  
  observeEvent(
    input$file1, {
      if(is.null(input$file1)){
        showNotification("Select the file")
      } else {
        infile <- input$file1
      }
    })
  
  observeEvent(input$anovabutton, {
    req(global_data())
    
    tryCatch({
      
      cat("Running ANOVA with:\n")
      cat("Dependent:", input$depvar, "\n")
      cat("Independent(s):", paste(input$indvar, collapse = ", "), "\n")
      
      
      if (is.null(input$depvar) || input$depvar == "") {
        showNotification("⚠️ Please select a dependent variable.", type = "error")
        return(NULL)
      }
      if (is.null(input$indvar) || length(input$indvar) == 0) {
        showNotification("⚠️ Please select one or two independent variables.", type = "error")
        return(NULL)
      }
      
      df <- global_data()
      dep <- input$depvar
      ind <- input$indvar
      
      # εξαrτημενη νουμερο
      if (!is.numeric(df[[dep]])) {
        try({ df[[dep]] <- as.numeric(as.character(df[[dep]])) }, silent = TRUE)
      }
      if (!is.numeric(df[[dep]])) {
        showNotification("❌ The dependent variable must be numerical.", type = "error")
        return(NULL)
      }
      
      # Wrap variables with backticks to handle spaces
      escape <- function(x) paste0("`", x, "`")
      formula_text <- paste(escape(dep), "~", paste(escape(ind), collapse = "*"))
      cat("Formula to be used: ", formula_text, "\n")
      my.model <- as.formula(formula_text)
      
      #run ANOVA
      all.ANOVA.output <- my_ANOVA(data = df, model = my.model, normalitytest = TRUE)
      rv$aov_model <- all.ANOVA.output$res.aov
      anova.report(all.ANOVA.output$htmlTable)
      
      showNotification("✅ANOVA was succefully completed", type = "message")
      
    }, error = function(e) {
      # Catch and show the actual error
      showNotification(paste("❌ Error while running ANOVA:", e$message), type = "error")
      cat("❌ Caught error:\n")
      print(e)
    })
  })
  
  summary_card <- function(model, df, depvar, indvar) {
  # Ensure all independent variables are factors
  for (var in indvar) {
    df[[var]] <- as.factor(df[[var]])
  }

  # Create formula (with interaction for two-way ANOVA)
  formula_text <- paste(depvar, "~", paste(indvar, collapse = "*"))
  aov_model <- aov(as.formula(formula_text), data = df)
  aov_table <- summary(aov_model)[[1]]

  # Analysis summary per term
  summaries <- purrr::map_chr(seq_len(nrow(aov_table) - 1), function(i) {
    term <- rownames(aov_table)[i]
    F_val <- round(aov_table$`F value`[i], 2)
    p_val <- aov_table$`Pr(>F)`[i]
    p_text <- if (!is.na(p_val) && p_val < 0.001) "p < .001" else paste0("p = ", round(p_val, 3))
    interpretation <- if (!is.na(p_val) && p_val < 0.05) {
      "This suggests that this factor has a statistically significant effect on the dependent variable."
    } else {
      "This means that the factor likely does not have a meaningful influence on the outcome."
    }
    glue::glue("<li><strong>{term}</strong>: F = {F_val}, {p_text}. {interpretation}</li>")
  })

  # Eta-squared effect size
  eta_vals <- round(lsr::etaSquared(aov_model, type = 3)[, 1], 2)
  eta_vals <- eta_vals[!is.na(eta_vals)]
  eta_texts <- purrr::imap_chr(eta_vals, function(val, name) {
    label <- dplyr::case_when(
      val < 0.01 ~ "a very small effect (minimal influence)",
      val < 0.06 ~ "a small effect (limited influence)",
      val < 0.14 ~ "a moderate effect (noticeable influence)",
      TRUE ~ "a large effect (strong influence)"
    )
    glue::glue("<li><strong>{name}</strong>: η² = <strong>{val}</strong> ({label})</li>")
  })

  eta_html <- if (length(eta_texts) > 0) {
    glue::glue("
      <h4>Effect Size</h4>
      <p>Effect size (η²) tells us how much of the total variation in <strong>{depvar}</strong> is explained by each factor. Larger values mean a stronger influence.</p>
      <ul>{paste(eta_texts, collapse = '')}</ul>")
  } else {
    ""
  }

  # Final HTML
  glue::glue("
    <h3 style='margin-top:1em;'>Summary</h3>
    <h4>Analysis of Variance</h4>
    <p>The following statistical summary describes how different factors affect the dependent variable <strong>{depvar}</strong>. The F-statistic indicates how much the means differ between groups relative to the variation within groups.</p>
    <ul>{paste(summaries, collapse = '')}</ul>
    {eta_html}
  ")
}

  
  
  
  
  
  
  
  output$summary <- renderUI({
    req(anova.report(), rv$aov_model, input$depvar, input$indvar)
    
    df <- global_data()
    dep <- input$depvar
    ind <- input$indvar
    
    model_summary <- summary(rv$aov_model)
    
    r_squared <- round(model_summary$r.squared, 3)
    adj_r_squared <- round(model_summary$adj.r.squared, 3)
    aic_val <- round(AIC(rv$aov_model), 2)
    bic_val <- round(BIC(rv$aov_model), 2)
    residual_se <- round(sqrt(deviance(rv$aov_model) / df.residual(rv$aov_model)), 3)
    
    table_html <- htmlTable::htmlTable(
      matrix(
        c(r_squared, adj_r_squared, aic_val, bic_val, residual_se),
        ncol = 5,
        dimnames = list(NULL, c("R²", "Adjusted R²", "AIC", "BIC", "Residual Std. Error"))
      ),
      caption = "<b>Model Summary</b>",
      align = "c"
    )
    
    tagList(
      HTML(anova.report()),
      HTML(table_html),
      HTML(summary_card(rv$aov_model, df, dep, ind))
    )
  })
  

  
}


ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "ANOVA"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload & Download Report", tabName = "upload", icon = icon("upload")),
      menuItem("Main Panel", tabName = "main", icon = icon("table")),
      menuItem("Assumption Tests", tabName = "assumptions", icon = icon("vial")),
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-bar")),
      menuItem("Residuals", tabName = "residuals", icon = icon("chart-line")),
      menuItem("Effect Size (η²)", tabName = "etasq", icon = icon("chart-area")),
      menuItem("Tukey Plot", tabName = "tukey", icon = icon("project-diagram")),
      menuItem("Means ± CI + Tukey", tabName = "means", icon = icon("chart-scatter")),
      menuItem("Info", tabName = "info", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    add_busy_spinner(spin = "fading-circle"),
    
    tags$head(
      tags$style(HTML("
    .box.box-info {
      border-top-color: #2e4053 !important;
    }
    .box.box-info > .box-header {
      background-color: #2e4053 !important;
      color: white !important;
    }
  "))
    ),
    
    
    
    tabItems(
      tabItem("upload",
              fluidRow(
                box(title = "Upload File", width = 6, solidHeader = TRUE, status = "info",
                    fileInput('file1', 'Upload Data File',
                              accept = c('.csv', '.xlsx')),
                    pickerInput("depvar", "Dependent (Numeric):", choices = NULL, options = list(`live-search` = TRUE)),
                    pickerInput("indvar", "Independent (Factor):", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                    actionButton("anovabutton", "Perform ANOVA", class = "btn-primary"),
                    downloadButton("downloadReport", "Download Report", class = "btn btn-success mt-2")
                )
              )),
      
      tabItem("main", htmlOutput("summary")),
      
      tabItem("assumptions", uiOutput("assumptionTests")),
      
      tabItem("graphs",
              tabBox(
                width = 12,
                tabPanel("Histogram", plotOutput("histPlot")),
                tabPanel("Boxplot", plotOutput("boxplot")),
                tabPanel("Multi-vari Chart", plotOutput("multiVariChart")),
                tabPanel("Violin Plot", plotlyOutput("violinPlot")),
                tabPanel("Raincloud Plot", plotOutput("raincloudPlot"))
              )),
      
      tabItem("residuals",
              fluidRow(
                column(6, plotOutput("qqplot")),
                column(6, plotOutput("residualsPlot"))
              ),
              fluidRow(
                column(6, plotOutput("shapiroPlot")),
                column(6, plotOutput("lillieforsPlot"))
              ),
              fluidRow(
                column(6, plotOutput("shapiroByGroupPlot")),
                column(6, plotOutput("lillieforsByGroupPlot"))
              )),
      
      tabItem("etasq", plotOutput("etasquarePlot")),
      
      tabItem("tukey", plotOutput("tukeyPlot")),
      
      tabItem("means", plotlyOutput("meansCLPlot")),
      
      tabItem("info", htmlOutput("aboutinfo"))
    )
  )
)




shinyApp(ui,server)