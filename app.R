library(shiny)
library(readr)
library(readxl)
library(DT)
library(car) 
library(stats)
library(ggplot2)
library(nortest) 
library(report) 
library(parameters) 

suggest_tests <- function(data) {
  suggestions <- list()
  for (col in colnames(data)) {
    column_data <- data[[col]]
    if (is.numeric(column_data)) {
      if (length(na.omit(column_data)) > 3) {
        suggestions[[col]] <- paste("Columna numérica:", col, 
                                    "\n- Pruebas de normalidad: Shapiro-Wilk, Lilliefors, Anderson-Darling",
                                    "\n- Pruebas paramétricas: t-test, ANOVA, Pearson",
                                    "\n- Pruebas no paramétricas: Wilcoxon, Kruskal-Wallis, Spearman")
      } else {
        suggestions[[col]] <- paste("Columna numérica:", col, 
                                    "no tiene suficientes valores para pruebas de normalidad.")
      }
    } else if (is.factor(column_data) || is.character(column_data) || length(unique(na.omit(column_data))) < 10) {
      if (length(unique(na.omit(column_data))) == 2) {
        suggestions[[col]] <- paste("Columna categórica binaria:", col, 
                                    "\n- Pruebas: Chi-cuadrado, McNemar, Fisher exact test")
      } else {
        suggestions[[col]] <- paste("Columna categórica:", col, 
                                    "con", length(unique(na.omit(column_data))), "niveles",
                                    "\n- Pruebas: Chi-cuadrado, ANOVA, Kruskal-Wallis")
      }
    } else {
      suggestions[[col]] <- paste("Columna:", col, "no procesable para análisis estadístico estándar.")
    }
  }
  return(suggestions)
}

ui <- fluidPage(
  titlePanel("Aplicación para Datos Cualitativos y Cuantitativos"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Subir archivo (CSV o Excel):", 
                accept = c(".csv", ".xls", ".xlsx")),
      actionButton("analyze", "Analizar Datos"),
      uiOutput("variable_selectors"),
      selectInput("operation", "Seleccione la prueba estadística:",
                  choices = c(
                    "Resumen descriptivo" = "summary",
                    "Normalidad: Shapiro-Wilk" = "shapiro",
                    "Normalidad: Lilliefors" = "lillie",
                    "Normalidad: Anderson-Darling" = "ad",
                    "Homocedasticidad: Levene" = "levene",
                    "Homocedasticidad: Bartlett" = "bartlett",
                    "t-test para una muestra" = "t_test_one",
                    "t-test para dos muestras" = "t_test_two",
                    "t-test pareado" = "t_test_paired",
                    "ANOVA" = "anova",
                    "ANOVA no paramétrico: Kruskal-Wallis" = "kruskal",
                    "Wilcoxon rank-sum" = "wilcoxon",
                    "Wilcoxon signed-rank" = "wilcoxon_paired",
                    "Correlación: Pearson" = "pearson",
                    "Correlación: Spearman" = "spearman",
                    "Chi-cuadrado" = "chi_sq",
                    "McNemar" = "mcnemar"
                  )),
      conditionalPanel(
        condition = "input.operation == 'pearson' || input.operation == 'spearman'",
        uiOutput("second_var_selector")
      ),
      actionButton("run_analysis", "Ejecutar Prueba")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Datos", DTOutput("data_preview")),
        tabPanel("Sugerencias", verbatimTextOutput("suggestions")),
        tabPanel("Resultados", verbatimTextOutput("results")),
        tabPanel("Gráficos", plotOutput("plot")),
        tabPanel("Interpretación", 
                 h4("Interpretación Automática:"),
                 verbatimTextOutput("auto_interpretation"),
                 hr(),
                 h4("Guía de Interpretación:"),
                 verbatimTextOutput("manual_interpretation"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  analysis_result <- reactiveVal(NULL)
  
  observeEvent(input$analyze, {
    req(input$file)
    file <- input$file
    
    tryCatch({
      if (grepl(".csv$", file$name)) {
        df <- read_csv(file$datapath, show_col_types = FALSE)
      } else if (grepl(".xls$|.xlsx$", file$name)) {
        df <- read_excel(file$datapath)
      } else {
        showNotification("Formato de archivo no soportado. Use CSV o Excel.", type = "error")
        return(NULL)
      }
      
      char_cols <- sapply(df, is.character)
      for (col in names(df)[char_cols]) {
        if (length(unique(df[[col]])) <= 10) {
          df[[col]] <- as.factor(df[[col]])
        }
      }
      
      data(df)
    }, error = function(e) {
      showNotification(paste("Error al leer el archivo:", e$message), type = "error")
    })
  })
  
  output$data_preview <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 5))
  })
  
  output$suggestions <- renderPrint({
    req(data())
    suggestions <- suggest_tests(data())
    for (col in names(suggestions)) {
      cat(paste0(suggestions[[col]], "\n\n"))
    }
  })
  
  # Generar selectores de variables dinámicamente
  output$variable_selectors <- renderUI({
    req(data())
    df <- data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    factor_vars <- names(df)[sapply(df, is.factor)]
    
    tagList(
      selectInput("var1", "Seleccione variable principal:", 
                  choices = names(df), selected = names(df)[1]),
      conditionalPanel(
        condition = "input.operation == 't_test_two' || input.operation == 't_test_paired' || 
                   input.operation == 'anova' || input.operation == 'kruskal' || 
                   input.operation == 'levene' || input.operation == 'bartlett' ||
                   input.operation == 'chi_sq' || input.operation == 'mcnemar'",
        selectInput("var2", "Seleccione variable secundaria:", 
                    choices = names(df), selected = names(df)[2])
      )
    )
  })
  
  output$second_var_selector <- renderUI({
    req(data())
    df <- data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    selectInput("var2_corr", "Seleccione segunda variable:", 
                choices = numeric_vars, selected = numeric_vars[2])
  })
  
  observeEvent(input$run_analysis, {
    req(data(), input$operation, input$var1)
    df <- data()
    var1 <- input$var1
    var2 <- if (input$operation %in% c("pearson", "spearman")) input$var2_corr else input$var2
    
    tryCatch({
      result <- NULL
      
      if (input$operation == "summary") {
        result <- summary(df)
        
      } else if (input$operation == "shapiro") {
        if (!is.numeric(df[[var1]])) stop("La variable debe ser numérica")
        result <- shapiro.test(na.omit(df[[var1]]))
        
      } else if (input$operation == "lillie") {
        if (!is.numeric(df[[var1]])) stop("La variable debe ser numérica")
        result <- lillie.test(na.omit(df[[var1]]))
        
      } else if (input$operation == "ad") {
        if (!is.numeric(df[[var1]])) stop("La variable debe ser numérica")
        result <- ad.test(na.omit(df[[var1]]))
        
      } else if (input$operation == "levene") {
        if (!is.numeric(df[[var1]])) stop("La primera variable debe ser numérica")
        if (!is.factor(df[[var2]])) stop("La segunda variable debe ser categórica")
        result <- leveneTest(df[[var1]] ~ df[[var2]], data = df)
        
      } else if (input$operation == "bartlett") {
        if (!is.numeric(df[[var1]])) stop("La primera variable debe ser numérica")
        if (!is.factor(df[[var2]])) stop("La segunda variable debe ser categórica")
        result <- bartlett.test(df[[var1]] ~ df[[var2]], data = df)
        
      } else if (input$operation == "t_test_one") {
        if (!is.numeric(df[[var1]])) stop("La variable debe ser numérica")
        result <- t.test(df[[var1]], mu = 0)
        
      } else if (input$operation == "t_test_two") {
        if (!is.numeric(df[[var1]])) stop("La primera variable debe ser numérica")
        if (!is.factor(df[[var2]])) stop("La segunda variable debe ser categórica")
        if (length(unique(na.omit(df[[var2]]))) != 2) stop("La variable categórica debe tener 2 niveles")
        result <- t.test(df[[var1]] ~ df[[var2]], data = df)
        
      } else if (input$operation == "t_test_paired") {
        if (!is.numeric(df[[var1]]) || !is.numeric(df[[var2]])) stop("Ambas variables deben ser numéricas")
        if (length(df[[var1]]) != length(df[[var2]])) stop("Las variables deben tener la misma longitud")
        result <- t.test(df[[var1]], df[[var2]], paired = TRUE)
        
      } else if (input$operation == "anova") {
        if (!is.numeric(df[[var1]])) stop("La primera variable debe ser numérica")
        if (!is.factor(df[[var2]])) stop("La segunda variable debe ser categórica")
        model <- aov(df[[var1]] ~ df[[var2]], data = df)
        result <- list(model = model, summary = summary(model))
        
      } else if (input$operation == "kruskal") {
        if (!is.numeric(df[[var1]])) stop("La primera variable debe ser numérica")
        if (!is.factor(df[[var2]])) stop("La segunda variable debe ser categórica")
        result <- kruskal.test(df[[var1]] ~ df[[var2]], data = df)
        
      } else if (input$operation == "wilcoxon") {
        if (!is.numeric(df[[var1]])) stop("La primera variable debe ser numérica")
        if (!is.factor(df[[var2]])) stop("La segunda variable debe ser categórica")
        if (length(unique(na.omit(df[[var2]]))) != 2) stop("La variable categórica debe tener 2 niveles")
        result <- wilcox.test(df[[var1]] ~ df[[var2]], data = df)
        
      } else if (input$operation == "wilcoxon_paired") {
        if (!is.numeric(df[[var1]]) || !is.numeric(df[[var2]])) stop("Ambas variables deben ser numéricas")
        if (length(df[[var1]]) != length(df[[var2]])) stop("Las variables deben tener la misma longitud")
        result <- wilcox.test(df[[var1]], df[[var2]], paired = TRUE)
        
      } else if (input$operation == "pearson") {
        if (!is.numeric(df[[var1]]) || !is.numeric(df[[var2]])) stop("Ambas variables deben ser numéricas")
        result <- cor.test(df[[var1]], df[[var2]], method = "pearson")
        
      } else if (input$operation == "spearman") {
        if (!is.numeric(df[[var1]]) || !is.numeric(df[[var2]])) stop("Ambas variables deben ser numéricas")
        result <- cor.test(df[[var1]], df[[var2]], method = "spearman")
        
      } else if (input$operation == "chi_sq") {
        if (!is.factor(df[[var1]]) && !is.character(df[[var1]])) stop("La primera variable debe ser categórica")
        if (!is.factor(df[[var2]]) && !is.character(df[[var2]])) stop("La segunda variable debe ser categórica")
        tbl <- table(df[[var1]], df[[var2]])
        if (any(tbl <= 0)) stop("La tabla de contingencia contiene celdas con conteo cero")
        result <- chisq.test(tbl)
        
      } else if (input$operation == "mcnemar") {
        if (!is.factor(df[[var1]]) && !is.character(df[[var1]])) stop("La primera variable debe ser categórica")
        if (!is.factor(df[[var2]]) && !is.character(df[[var2]])) stop("La segunda variable debe ser categórica")
        tbl <- table(df[[var1]], df[[var2]])
        if (nrow(tbl) != 2 || ncol(tbl) != 2) stop("McNemar requiere una tabla 2x2")
        result <- mcnemar.test(tbl)
      }
      
      analysis_result(result)
      
    }, error = function(e) {
      showNotification(paste("Error en el análisis:", e$message), type = "error")
      analysis_result(NULL)
    })
  })
  
  output$results <- renderPrint({
    req(analysis_result())
    res <- analysis_result()
    
    if (input$operation == "anova") {
      print(res$summary)
    } else {
      print(res)
    }
  })
  
  output$plot <- renderPlot({
    req(data(), input$operation, input$var1)
    df <- data()
    var1 <- input$var1
    var2 <- if (input$operation %in% c("pearson", "spearman")) input$var2_corr else input$var2
    
    tryCatch({
      if (input$operation %in% c("shapiro", "lillie", "ad")) {
        # Gráfico QQ para pruebas de normalidad
        qqnorm(na.omit(df[[var1]]))
        qqline(na.omit(df[[var1]]))
        title(paste("Gráfico QQ para", var1))
        
      } else if (input$operation %in% c("t_test_one", "t_test_two", "t_test_paired", 
                                        "anova", "kruskal", "wilcoxon", "wilcoxon_paired")) {
        # Boxplot para comparaciones de grupos
        if (input$operation == "t_test_one") {
          boxplot(df[[var1]], main = paste("Distribución de", var1), ylab = var1)
        } else if (input$operation == "t_test_paired" || input$operation == "wilcoxon_paired") {
          plot_data <- data.frame(
            value = c(df[[var1]], df[[var2]]),
            group = rep(c(var1, var2), each = nrow(df))
          )
          ggplot(plot_data, aes(x = group, y = value)) + 
            geom_boxplot() +
            labs(title = "Comparación de muestras pareadas", x = "Variable", y = "Valor")
        } else {
          ggplot(df, aes(x = factor(df[[var2]]), y = df[[var1]])) + 
            geom_boxplot() +
            labs(title = paste("Distribución por grupos de", var2), 
                 x = var2, y = var1)
        }
        
      } else if (input$operation %in% c("pearson", "spearman")) {
        # Gráfico de dispersión para correlación
        ggplot(df, aes(x = df[[var1]], y = df[[var2]])) + 
          geom_point() +
          geom_smooth(method = "lm", se = FALSE) +
          labs(title = paste("Correlación entre", var1, "y", var2), 
               x = var1, y = var2)
        
      } else if (input$operation %in% c("chi_sq", "mcnemar")) {
        # Mosaico para tablas de contingencia
        tbl <- table(df[[var1]], df[[var2]])
        mosaicplot(tbl, main = paste("Tabla de contingencia:", var1, "vs", var2), 
                   color = TRUE)
      }
    }, error = function(e) {
      plot(1, type = "n", main = "No se pudo generar el gráfico", 
           xlab = "", ylab = "", axes = FALSE)
      text(1, 1, paste("Error:", e$message))
    })
  })
  
  output$auto_interpretation <- renderPrint({
    req(analysis_result())
    res <- analysis_result()
    
    tryCatch({
      if (input$operation == "anova") {
        report_text <- report(res$model)
      } else {
        report_text <- report(res)
      }
      
      cat(as.character(report_text))
      
    }, error = function(e) {
      cat("No se pudo generar la interpretación automática para esta prueba.\n")
      cat("Error:", e$message, "\n")
    })
  })
  
  output$manual_interpretation <- renderPrint({
    req(input$operation)
    cat("GUÍA DE INTERPRETACIÓN:\n\n")
    
    if (input$operation %in% c("shapiro", "lillie", "ad")) {
      cat("PRUEBAS DE NORMALIDAD:\n")
      cat("- H0: Los datos siguen una distribución normal\n")
      cat("- p-value > 0.05: No se rechaza H0 (normalidad)\n")
      cat("- p-value <= 0.05: Se rechaza H0 (no normalidad)\n")
      
    } else if (input$operation %in% c("levene", "bartlett")) {
      cat("PRUEBAS DE HOMOCEDASTICIDAD (igualdad de varianzas):\n")
      cat("- H0: Las varianzas son iguales entre grupos\n")
      cat("- p-value > 0.05: No se rechaza H0 (homocedasticidad)\n")
      cat("- p-value <= 0.05: Se rechaza H0 (heterocedasticidad)\n")
      
    } else if (input$operation %in% c("t_test_one", "t_test_two", "t_test_paired")) {
      cat("PRUEBAS T DE STUDENT:\n")
      cat("- H0: No hay diferencia entre las medias\n")
      cat("- p-value > 0.05: No se rechaza H0 (no diferencia significativa)\n")
      cat("- p-value <= 0.05: Se rechaza H0 (diferencia significativa)\n")
      if (input$operation == "t_test_one") {
        cat("- Intervalo de confianza: Rango probable para la media poblacional\n")
      } else {
        cat("- Intervalo de confianza: Rango probable para la diferencia de medias\n")
      }
      
    } else if (input$operation == "anova") {
      cat("ANÁLISIS DE VARIANZA (ANOVA):\n")
      cat("- H0: Las medias de todos los grupos son iguales\n")
      cat("- p-value > 0.05: No se rechaza H0 (no diferencias significativas)\n")
      cat("- p-value <= 0.05: Se rechaza H0 (al menos un grupo difiere)\n")
      
    } else if (input$operation == "kruskal") {
      cat("KRUSKAL-WALLIS (ANOVA no paramétrico):\n")
      cat("- H0: Las distribuciones son iguales en todos los grupos\n")
      cat("- p-value > 0.05: No se rechaza H0 (no diferencias significativas)\n")
      cat("- p-value <= 0.05: Se rechaza H0 (al menos un grupo difiere)\n")
      
    } else if (input$operation %in% c("wilcoxon", "wilcoxon_paired")) {
      cat("PRUEBA DE WILCOXON:\n")
      cat("- H0: Las distribuciones son iguales\n")
      cat("- p-value > 0.05: No se rechaza H0 (no diferencia significativa)\n")
      cat("- p-value <= 0.05: Se rechaza H0 (diferencia significativa)\n")
      
    } else if (input$operation %in% c("pearson", "spearman")) {
      cat("PRUEBAS DE CORRELACIÓN:\n")
      cat("- H0: No hay correlación (ρ = 0)\n")
      cat("- p-value > 0.05: No se rechaza H0 (no correlación significativa)\n")
      cat("- p-value <= 0.05: Se rechaza H0 (correlación significativa)\n")
      cat("- Coeficiente: Medida de fuerza y dirección de la relación (-1 a 1)\n")
      
    } else if (input$operation == "chi_sq") {
      cat("PRUEBA CHI-CUADRADO DE INDEPENDENCIA:\n")
      cat("- H0: Las variables son independientes\n")
      cat("- p-value > 0.05: No se rechaza H0 (no asociación significativa)\n")
      cat("- p-value <= 0.05: Se rechaza H0 (asociación significativa)\n")
      
    } else if (input$operation == "mcnemar") {
      cat("PRUEBA DE McNEMAR (datos pareados categóricos):\n")
      cat("- H0: Las proporciones marginales son iguales\n")
      cat("- p-value > 0.05: No se rechaza H0 (no cambio significativo)\n")
      cat("- p-value <= 0.05: Se rechaza H0 (cambio significativo)\n")
    }
  })
}

shinyApp(ui, server)