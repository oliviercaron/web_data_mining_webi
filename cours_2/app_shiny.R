# app_shiny.R
#
# Dashboard marketing : A/B test Facebook vs AdWords
# Données : ../data_full/facebook_vs_adword/A_B_testing_dataset.csv

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(scales)
library(DT)
library(gridExtra)

# -------------------------------------------------------------------
# 1. Chargement + préparation des données (dplyr)
# -------------------------------------------------------------------

ads_raw <- read_csv(
  "../data_full/facebook_vs_adword/A_B_testing_dataset.csv",
  show_col_types = FALSE
)

# Paramètre business : revenu moyen par conversion (à ajuster selon le contexte)
REVENUE_PER_CONVERSION <- 50  # en euros

# On repart propre : on ne garde que les colonnes "brutes"
ads_long <- ads_raw %>%
  transmute(
    date = date_of_campaign,
    # On renomme pour que le pattern soit simple : platform_metric
    facebook_views       = facebook_ad_views,
    facebook_clicks      = facebook_ad_clicks,
    facebook_conversions = facebook_ad_conversions,
    facebook_cost        = facebook_cost_per_ad,
    adwords_views        = adword_ad_views,
    adwords_clicks       = adword_ad_clicks,
    adwords_conversions  = adword_ad_conversions,
    adwords_cost         = adword_cost_per_ad
  ) %>%
  # wide -> long
  pivot_longer(
    cols = -date,
    names_to   = c("platform", "metric"),
    names_pattern = "^(facebook|adwords)_(.*)$",
    values_to = "value"
  ) %>%
  # CORRECTION : agréger avant pivot_wider pour gérer les doublons
  group_by(date, platform, metric) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  # long -> wide par métrique
  pivot_wider(
    names_from  = metric,
    values_from = value
  ) %>%
  mutate(
    platform = recode(
      platform,
      "facebook" = "Facebook",
      "adwords"  = "AdWords"
    ),
    # Tout est déjà numérique, mais on s'assure
    across(c(views, clicks, conversions, cost), as.numeric),
    # KPI calculés
    ctr = if_else(views > 0, clicks / views, NA_real_),
    cvr = if_else(clicks > 0, conversions / clicks, NA_real_),
    cpc = if_else(clicks > 0, cost / clicks, NA_real_),
    cpa = if_else(conversions > 0, cost / conversions, NA_real_),
    # NOUVEAU : métriques business
    revenue = conversions * REVENUE_PER_CONVERSION,
    profit = revenue - cost,
    roas = if_else(cost > 0, revenue / cost, NA_real_),
    # Jour de la semaine
    weekday = wday(date, label = TRUE, abbr = FALSE, week_start = 1)
  )

min_date <- min(ads_long$date, na.rm = TRUE)
max_date <- max(ads_long$date, na.rm = TRUE)

# -------------------------------------------------------------------
# 2. UI Shiny
# -------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Dashboard marketing – A/B test Facebook vs AdWords"),
  
  # Un peu de CSS pour les KPI boxes
  tags$head(
    tags$style(HTML("
      .kpi-box {
        background: #f8f9fa;
        border-radius: 12px;
        padding: 12px 16px;
        margin-bottom: 12px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      }
      .kpi-title {
        font-size: 0.8rem;
        text-transform: uppercase;
        letter-spacing: .06em;
        color: #6c757d;
        margin-bottom: 4px;
      }
      .kpi-value {
        font-size: 1.4rem;
        font-weight: 600;
        margin-bottom: 2px;
      }
      .kpi-sub {
        font-size: 0.75rem;
        color: #6c757d;
      }
      .positive { color: #28a745; }
      .negative { color: #dc3545; }
      .recommendation-box {
        background: #e7f3ff;
        border-left: 4px solid #0066cc;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .recommendation-box h4 {
        margin-top: 0;
        color: #0066cc;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        label = "Période",
        start = min_date,
        end   = max_date,
        min   = min_date,
        max   = max_date
      ),
      checkboxGroupInput(
        "platforms",
        label   = "Plateformes",
        choices = c("Facebook", "AdWords"),
        selected = c("Facebook", "AdWords")
      ),
      hr(),
      numericInput(
        "revenue_per_conv",
        label = "Revenu moyen par conversion (€)",
        value = REVENUE_PER_CONVERSION,
        min = 0,
        step = 5
      ),
      helpText("Ajustez selon votre panier moyen pour calculer le ROAS et profit."),
      hr(),
      radioButtons(
        "time_agg",
        "Agrégation temporelle",
        choices = c("Jour" = "day", "Semaine" = "week", "Mois" = "month"),
        selected = "week"
      ),
      selectInput(
        "metric_comparison",
        "Métrique à comparer",
        choices = c("ROAS" = "roas", "CPA" = "cpa", "CTR" = "ctr", "CVR" = "cvr"),
        selected = "roas"
      ),
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(3, uiOutput("kpi_conversions")),
        column(3, uiOutput("kpi_roas")),
        column(3, uiOutput("kpi_profit")),
        column(3, uiOutput("kpi_cpa"))
      ),
      hr(),
      tabsetPanel(
        tabPanel(
          "Vue d'ensemble",
          br(),
          uiOutput("recommendations"),
          br(),
          plotOutput("performance_overview", height = "350px")
        ),
        tabPanel(
          "Évolution temporelle",
          br(),
          plotOutput("time_plot", height = "350px")
        ),
        tabPanel(
          "Funnel de conversion",
          br(),
          plotOutput("funnel_plot", height = "350px"),
          br(),
          plotOutput("funnel_rates", height = "300px")
        ),
        tabPanel(
          "Performance hebdomadaire",
          br(),
          plotOutput("weekday_plot", height = "350px")
        ),
        tabPanel(
          "Optimisation budget",
          br(),
          fluidRow(
            column(6,
                   sliderInput(
                     "budget_split",
                     "Allocation budgétaire Facebook (%)",
                     min = 0, max = 100, value = 50, step = 5
                   )
            ),
            column(6,
                   numericInput(
                     "total_budget",
                     "Budget total mensuel (€)",
                     value = 10000,
                     min = 0,
                     step = 1000
                   )
            )
          ),
          uiOutput("budget_projection")
        ),
        tabPanel(
          "Tableau détaillé",
          br(),
          DTOutput("detail_table")
        )
      )
    )
  )
)

# -------------------------------------------------------------------
# 3. Serveur Shiny
# -------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Recalcul avec le revenu ajustable
  ads_filtered <- reactive({
    req(input$date_range, input$revenue_per_conv)
    ads_long %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        platform %in% input$platforms
      ) %>%
      mutate(
        revenue = conversions * input$revenue_per_conv,
        profit = revenue - cost,
        roas = if_else(cost > 0, revenue / cost, NA_real_)
      )
  })
  
  # Agrégation globale pour KPI
  summary_global <- reactive({
    ads_filtered() %>%
      summarise(
        total_cost        = sum(cost, na.rm = TRUE),
        total_views       = sum(views, na.rm = TRUE),
        total_clicks      = sum(clicks, na.rm = TRUE),
        total_conversions = sum(conversions, na.rm = TRUE),
        total_revenue     = sum(revenue, na.rm = TRUE),
        total_profit      = sum(profit, na.rm = TRUE)
      ) %>%
      mutate(
        ctr = if_else(total_views > 0, total_clicks / total_views, NA_real_),
        cpa = if_else(total_conversions > 0, total_cost / total_conversions, NA_real_),
        roas = if_else(total_cost > 0, total_revenue / total_cost, NA_real_)
      )
  })
  
  # Agrégation par plateforme
  summary_by_platform <- reactive({
    ads_filtered() %>%
      group_by(platform) %>%
      summarise(
        cost        = sum(cost, na.rm = TRUE),
        views       = sum(views, na.rm = TRUE),
        clicks      = sum(clicks, na.rm = TRUE),
        conversions = sum(conversions, na.rm = TRUE),
        revenue     = sum(revenue, na.rm = TRUE),
        profit      = sum(profit, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        ctr = if_else(views > 0, clicks / views, NA_real_),
        cvr = if_else(clicks > 0, conversions / clicks, NA_real_),
        cpa = if_else(conversions > 0, cost / conversions, NA_real_),
        roas = if_else(cost > 0, revenue / cost, NA_real_)
      )
  })
  
  # KPI boxes
  output$kpi_conversions <- renderUI({
    s <- summary_global()
    div(class = "kpi-box",
        div(class = "kpi-title", "Conversions"),
        div(class = "kpi-value", 
            ifelse(is.na(s$total_conversions), "–",
                   formatC(s$total_conversions, format = "d", big.mark = " "))),
        div(class = "kpi-sub", "Total sur la période")
    )
  })
  
  output$kpi_roas <- renderUI({
    s <- summary_global()
    roas_class <- if(!is.na(s$roas) && s$roas > 1) "positive" else "negative"
    div(class = "kpi-box",
        div(class = "kpi-title", "ROAS (Return on Ad Spend)"),
        div(class = paste("kpi-value", roas_class),
            ifelse(is.na(s$roas), "–",
                   paste0(formatC(s$roas, format = "f", digits = 2), "×"))),
        div(class = "kpi-sub", 
            ifelse(!is.na(s$roas) && s$roas > 1, 
                   "✓ Rentable", "✗ Non rentable"))
    )
  })
  
  output$kpi_profit <- renderUI({
    s <- summary_global()
    profit_class <- if(!is.na(s$total_profit) && s$total_profit > 0) "positive" else "negative"
    div(class = "kpi-box",
        div(class = "kpi-title", "Profit net"),
        div(class = paste("kpi-value", profit_class),
            ifelse(is.na(s$total_profit), "–",
                   paste0(formatC(s$total_profit, format = "f", digits = 0, big.mark = " "), " €"))),
        div(class = "kpi-sub", "Revenu - Coût publicitaire")
    )
  })
  
  output$kpi_cpa <- renderUI({
    s <- summary_global()
    div(class = "kpi-box",
        div(class = "kpi-title", "CPA (coût / conversion)"),
        div(class = "kpi-value",
            ifelse(is.na(s$cpa), "–",
                   paste0(formatC(s$cpa, format = "f", digits = 2, big.mark = " "), " €"))),
        div(class = "kpi-sub", "Plus c'est bas, mieux c'est")
    )
  })
  
  # Recommandations automatiques
  output$recommendations <- renderUI({
    df <- summary_by_platform()
    req(nrow(df) > 0)
    
    if(nrow(df) == 2) {
      fb <- df %>% filter(platform == "Facebook")
      aw <- df %>% filter(platform == "AdWords")
      
      best_roas <- if(fb$roas > aw$roas) "Facebook" else "AdWords"
      best_cpa <- if(fb$cpa < aw$cpa) "Facebook" else "AdWords"
      
      diff_roas <- abs(fb$roas - aw$roas)
      diff_cpa_pct <- abs(fb$cpa - aw$cpa) / min(fb$cpa, aw$cpa) * 100
      
      div(class = "recommendation-box",
          h4("💡 Recommandations"),
          tags$ul(
            tags$li(strong(best_roas), " a le meilleur ROAS (",
                    formatC(max(fb$roas, aw$roas), format = "f", digits = 2), 
                    "× vs ", 
                    formatC(min(fb$roas, aw$roas), format = "f", digits = 2), "×)"),
            tags$li(strong(best_cpa), " a le CPA le plus bas (",
                    formatC(min(fb$cpa, aw$cpa), format = "f", digits = 2), 
                    "€ vs ", 
                    formatC(max(fb$cpa, aw$cpa), format = "f", digits = 2), "€, soit ",
                    round(diff_cpa_pct, 0), "% de différence)"),
            tags$li(
              if(best_roas == best_cpa) {
                paste0("✓ ", best_roas, " domine sur les 2 métriques clés → Augmenter son budget")
              } else {
                "⚠ Résultats mixtes → Analyser le contexte (objectif : volume ou rentabilité ?)"
              }
            )
          )
      )
    }
  })
  
  # Graph vue d'ensemble : comparaison directe avec métrique sélectionnable
  output$performance_overview <- renderPlot({
    df <- summary_by_platform()
    req(nrow(df) > 0, input$metric_comparison)
    
    metric_labels <- c(
      "roas" = "ROAS",
      "cpa" = "CPA (€)",
      "ctr" = "CTR (%)",
      "cvr" = "Taux de conversion (%)"
    )
    
    metric_title <- metric_labels[input$metric_comparison]
    
    # Préparer les données selon la métrique choisie
    if(input$metric_comparison == "ctr") {
      df <- df %>% mutate(value = ctr * 100)
    } else if(input$metric_comparison == "cvr") {
      df <- df %>% mutate(value = cvr * 100)
    } else {
      df <- df %>% mutate(value = .data[[input$metric_comparison]])
    }
    
    # Créer deux graphiques : métrique choisie + part budget vs conversions
    p1 <- ggplot(df, aes(x = platform, y = value, fill = platform)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = round(value, 2)), vjust = -0.3, size = 4) +
      labs(x = "", y = metric_title, title = paste("Comparaison :", metric_title)) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11))
    
    # Graphique part de budget vs part de conversions
    total_cost <- sum(df$cost)
    total_conv <- sum(df$conversions)
    
    df_share <- df %>%
      mutate(
        budget_share = cost / total_cost * 100,
        conv_share = conversions / total_conv * 100
      ) %>%
      select(platform, budget_share, conv_share) %>%
      pivot_longer(-platform, names_to = "type", values_to = "percentage") %>%
      mutate(type = recode(type, 
                           "budget_share" = "Part du budget",
                           "conv_share" = "Part des conversions"))
    
    p2 <- ggplot(df_share, aes(x = type, y = percentage, fill = platform)) +
      geom_col(position = "dodge", alpha = 0.8) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")),
                position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5) +
      labs(x = "", y = "%", title = "Efficacité : Budget investi vs Conversions obtenues",
           fill = "Plateforme") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11))
    
    # Combiner les deux graphiques
    gridExtra::grid.arrange(p1, p2, ncol = 2)
  })
  
  # Données agrégées par jour/semaine/mois + plateforme
  daily_metrics <- reactive({
    df <- ads_filtered()
    
    # Agrégation selon le choix de l'utilisateur
    if(input$time_agg == "week") {
      df <- df %>%
        mutate(period = floor_date(date, "week", week_start = 1))
    } else if(input$time_agg == "month") {
      df <- df %>%
        mutate(period = floor_date(date, "month"))
    } else {
      df <- df %>%
        mutate(period = date)
    }
    
    df %>%
      group_by(period, platform) %>%
      summarise(
        cost        = sum(cost, na.rm = TRUE),
        views       = sum(views, na.rm = TRUE),
        clicks      = sum(clicks, na.rm = TRUE),
        conversions = sum(conversions, na.rm = TRUE),
        revenue     = sum(revenue, na.rm = TRUE),
        profit      = sum(profit, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        ctr = if_else(views > 0, clicks / views, NA_real_),
        cvr = if_else(clicks > 0, conversions / clicks, NA_real_),
        cpa = if_else(conversions > 0, cost / conversions, NA_real_),
        roas = if_else(cost > 0, revenue / cost, NA_real_)
      ) %>%
      rename(date = period)
  })
  
  # Graph évolution temporelle
  output$time_plot <- renderPlot({
    df <- daily_metrics()
    req(nrow(df) > 0, input$metric_comparison)
    
    # Label de l'axe X selon l'agrégation
    x_label <- case_when(
      input$time_agg == "day" ~ "",
      input$time_agg == "week" ~ "Semaine",
      input$time_agg == "month" ~ "Mois"
    )
    
    # Labels et paramètres selon la métrique
    metric_labels <- c(
      "roas" = "ROAS",
      "cpa" = "CPA (€)",
      "ctr" = "CTR (%)",
      "cvr" = "Taux de conversion (%)"
    )
    
    y_label <- metric_labels[input$metric_comparison]
    
    # Préparer les données selon la métrique choisie
    if(input$metric_comparison == "ctr") {
      df <- df %>% mutate(metric_value = ctr * 100)
    } else if(input$metric_comparison == "cvr") {
      df <- df %>% mutate(metric_value = cvr * 100)
    } else {
      df <- df %>% mutate(metric_value = .data[[input$metric_comparison]])
    }
    
    # Graphique de base
    p <- ggplot(df, aes(x = date)) +
      geom_line(aes(y = metric_value, color = platform), linewidth = 1.2) +
      geom_point(aes(y = metric_value, color = platform), size = 2, alpha = 0.6) +
      labs(
        x = x_label, y = y_label,
        color = "Plateforme",
        title = paste0("Évolution du ", y_label, " dans le temps (agrégation : ", 
                       c("day" = "journalière", "week" = "hebdomadaire", "month" = "mensuelle")[input$time_agg], ")")
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
    
    # Ajouter une ligne de référence uniquement pour ROAS et CPA
    if(input$metric_comparison == "roas") {
      p <- p + 
        geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
        annotate("text", x = min(df$date), y = 1, label = "Seuil de rentabilité", 
                 vjust = -0.5, hjust = 0, size = 3, color = "red")
    }
    
    p
  })
  
  # Funnel de conversion
  output$funnel_plot <- renderPlot({
    df <- summary_by_platform() %>%
      select(platform, views, clicks, conversions) %>%
      pivot_longer(-platform, names_to = "stage", values_to = "count") %>%
      mutate(stage = factor(stage, levels = c("views", "clicks", "conversions"),
                            labels = c("Vues", "Clics", "Conversions")))
    
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = platform, y = count, fill = platform)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = formatC(count, format = "d", big.mark = " ")),
                vjust = -0.3, size = 3.5) +
      facet_wrap(~stage, scales = "free_y", ncol = 3) +
      scale_y_continuous(labels = label_number(big.mark = " ")) +
      labs(
        x = "", y = "Volume",
        fill = "Plateforme",
        title = "Funnel de conversion : Vues → Clics → Conversions"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  })
  
  # Taux de conversion du funnel
  output$funnel_rates <- renderPlot({
    df <- summary_by_platform() %>%
      select(platform, CTR = ctr, CVR = cvr) %>%
      pivot_longer(-platform, names_to = "metric", values_to = "rate")
    
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = metric, y = rate, fill = platform)) +
      geom_col(position = "dodge", alpha = 0.8) +
      geom_text(aes(label = percent(rate, accuracy = 0.1)),
                position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5) +
      scale_y_continuous(labels = percent_format()) +
      labs(
        x = "", y = "Taux",
        fill = "Plateforme",
        title = "Taux de conversion à chaque étape"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  })
  
  # Performance par jour de la semaine
  output$weekday_plot <- renderPlot({
    df <- ads_filtered() %>%
      group_by(weekday, platform) %>%
      summarise(
        conversions = sum(conversions, na.rm = TRUE),
        cost = sum(cost, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(cpa = if_else(conversions > 0, cost / conversions, NA_real_))
    
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = weekday, y = conversions, fill = platform)) +
      geom_col(position = "dodge", alpha = 0.8) +
      labs(
        x = "", y = "Conversions",
        fill = "Plateforme",
        title = "Performance par jour de la semaine"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Projection budget
  output$budget_projection <- renderUI({
    req(input$total_budget, input$budget_split)
    
    df <- summary_by_platform()
    req(nrow(df) == 2)
    
    fb <- df %>% filter(platform == "Facebook")
    aw <- df %>% filter(platform == "AdWords")
    
    fb_budget <- input$total_budget * input$budget_split / 100
    aw_budget <- input$total_budget * (100 - input$budget_split) / 100
    
    fb_conv_proj <- fb_budget / fb$cpa
    aw_conv_proj <- aw_budget / aw$cpa
    total_conv <- fb_conv_proj + aw_conv_proj
    
    fb_revenue <- fb_conv_proj * input$revenue_per_conv
    aw_revenue <- aw_conv_proj * input$revenue_per_conv
    total_revenue <- fb_revenue + aw_revenue
    total_profit <- total_revenue - input$total_budget
    
    div(
      class = "recommendation-box",
      h4("📊 Projection avec cette allocation budgétaire"),
      tags$table(
        style = "width: 100%; border-collapse: collapse;",
        tags$thead(
          tags$tr(
            tags$th("Plateforme", style = "text-align: left; padding: 8px; border-bottom: 2px solid #ddd;"),
            tags$th("Budget", style = "text-align: right; padding: 8px; border-bottom: 2px solid #ddd;"),
            tags$th("Conversions projetées", style = "text-align: right; padding: 8px; border-bottom: 2px solid #ddd;"),
            tags$th("Revenu projeté", style = "text-align: right; padding: 8px; border-bottom: 2px solid #ddd;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Facebook", style = "padding: 8px;"),
            tags$td(paste0(formatC(fb_budget, format = "f", digits = 0, big.mark = " "), " €"), 
                    style = "text-align: right; padding: 8px;"),
            tags$td(formatC(round(fb_conv_proj), format = "d", big.mark = " "), 
                    style = "text-align: right; padding: 8px;"),
            tags$td(paste0(formatC(fb_revenue, format = "f", digits = 0, big.mark = " "), " €"), 
                    style = "text-align: right; padding: 8px;")
          ),
          tags$tr(
            tags$td("AdWords", style = "padding: 8px;"),
            tags$td(paste0(formatC(aw_budget, format = "f", digits = 0, big.mark = " "), " €"), 
                    style = "text-align: right; padding: 8px;"),
            tags$td(formatC(round(aw_conv_proj), format = "d", big.mark = " "), 
                    style = "text-align: right; padding: 8px;"),
            tags$td(paste0(formatC(aw_revenue, format = "f", digits = 0, big.mark = " "), " €"), 
                    style = "text-align: right; padding: 8px;")
          ),
          tags$tr(
            style = "font-weight: bold; border-top: 2px solid #ddd;",
            tags$td("TOTAL", style = "padding: 8px;"),
            tags$td(paste0(formatC(input$total_budget, format = "f", digits = 0, big.mark = " "), " €"), 
                    style = "text-align: right; padding: 8px;"),
            tags$td(formatC(round(total_conv), format = "d", big.mark = " "), 
                    style = "text-align: right; padding: 8px;"),
            tags$td(paste0(formatC(total_revenue, format = "f", digits = 0, big.mark = " "), " €"), 
                    style = "text-align: right; padding: 8px;")
          )
        )
      ),
      br(),
      tags$p(
        style = "font-size: 1.1em; margin-top: 10px;",
        strong("Profit net projeté : "),
        span(
          style = if(total_profit > 0) "color: #28a745;" else "color: #dc3545;",
          paste0(formatC(total_profit, format = "f", digits = 0, big.mark = " "), " €"),
          " (ROAS : ", formatC(total_revenue / input$total_budget, format = "f", digits = 2), "×)"
        )
      )
    )
  })
  
  # Tableau détaillé
  output$detail_table <- renderDT({
    df <- daily_metrics() %>%
      arrange(desc(date), platform) %>%
      select(
        date, platform,
        views, clicks, conversions,
        ctr, cpa, roas, profit
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    ) %>%
      formatPercentage("ctr", 1) %>%
      formatCurrency("cpa", currency = "€", digits = 2) %>%
      formatRound("roas", 2) %>%
      formatCurrency("profit", currency = "€", digits = 0)
  })
}

shinyApp(ui, server)