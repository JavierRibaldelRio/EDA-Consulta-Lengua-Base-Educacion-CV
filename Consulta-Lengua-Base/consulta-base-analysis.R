library(tidyverse)

results <- read_csv("Resultados-Consulta_Base.csv", locale = locale(decimal_mark = ","))


results$comarca <- str_trim(str_remove_all(results$comarca, pattern = ".*?/"))
# Results for each province


media = mean(results$pval) / 100

results |>
    group_by(provincia) |>
    summarise(val = mean(pval) / 100, cas = mean(pcas) / 100) |>
    
    pivot_longer(!provincia, names_to = "lengua", values_to = "porcentaje")  |>
    ggplot(aes(
        x = provincia,
        y = porcentaje,
        fill = lengua,
        label = str_c(ceiling(porcentaje * 10000) / 100, "%")
    )) +
    geom_col() +
    geom_text(size = 5, position = position_stack(vjust = 0.5)) +    # Etiquetas centradas
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(
        # Leyenda
        name = "Lengua",
        labels = c("cas" = "Castellano", "val" = "Valenciano"),
        values = c("cas" = "#C41E3A", "val" = "orange")
    ) +
    geom_hline(aes(yintercept = media), size = 2) +
    annotate(
        geom = "label",
        x = "Castelló",
        y = 0.58,
        label = str_c("Porcentaje Valenciano: ", ceiling(media * 10000) /
                          100, "%")
    ) +
    labs(
        title = "Resultado Consulta Lingüística Base por Provincia",
        subtitle = "Comunidad Valenciana | Marzo 2025",
        x = "Provincia",
        y = "Porcentaje",
        caption = "Fuente: gva.es"
    ) +
    theme_minimal()

results |>
    group_by(provincia, comarca) |>
    summarise(
        val = mean(pval) / 100, 
        cas = mean(pcas) / 100, ) |>
    pivot_longer(
        cols = -c(provincia, comarca),
        names_to = "lengua",
        values_to = "porcentaje"
    ) |>
    
    ggplot(aes(
        x = comarca,
        y = porcentaje,
        fill = lengua,
        label = str_c(ceiling(porcentaje * 100), "%")
    )) +
    geom_col() +
    geom_text(size = 2.5, position = position_stack(vjust = 0.5)) +    # Etiquetas centradas
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(
        # Leyenda
        
        name = "Lengua",
        labels = c("cas" = "Castellano", "val" = "Valenciano"),
        values = c("cas" = "#C41E3A", "val" = "orange")
    ) +
    labs(
        title = "Resultado Consulta Lingüística Base por Provincia",
        subtitle = "Comunidad Valenciana | Marzo 2025",
        x = "Comarca",
        y = "Porcentaje",
        caption = "Fuente: gva.es"
    ) +
    facet_wrap( ~ provincia, scales = "free_x", ncol = 1) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(
            angle = 30,
            vjust = 1,
            hjust = 1
        ),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(0.1, "cm")
    )
