library(readxl)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

arkusz1 <- read_excel("data.xlsx", sheet = "Arkusz1")  
arkusz2 <- read_excel("data.xlsx", sheet = "Arkusz2")  

deskryptory <- arkusz1 %>%
  select(where(is.numeric)) %>%
  na.omit()

if ("No." %in% colnames(arkusz1)) {
  deskryptory <- deskryptory %>% select(where(~ var(.) > 0))
} else {
  stop("Brak kolumny 'No.' w arkuszu1.")
}

pca <- prcomp(deskryptory, scale. = TRUE)
pca_df <- as.data.frame(pca$x)
pca_df$No. <- arkusz1$No.[complete.cases(deskryptory)]

create_pca_plot <- function(merged_df, toxicity_col, title, legend_label) {
  if (nrow(merged_df) > 0) {
    vectors <- merged_df %>%
      summarize(
        PC1_sum = mean(PC1 * get(toxicity_col), na.rm = TRUE),
        PC2_sum = mean(PC2 * get(toxicity_col), na.rm = TRUE)
      )
    
    if (!is.na(vectors$PC1_sum) & !is.na(vectors$PC2_sum)) {
      vectors_df <- data.frame(
        id = c(toxicity_col),
        PC1 = c(-vectors$PC1_sum),
        PC2 = c(-vectors$PC2_sum),
        color = c("#FFA07A")
      )
      
      default_length <- 30
      
      vectors_df <- vectors_df %>%
        mutate(
          PC1 = PC1 / sqrt(PC1^2 + PC2^2) * default_length,
          PC2 = PC2 / sqrt(PC1^2 + PC2^2) * default_length
        )
      
      ggplot(merged_df, aes(x = PC1, y = PC2, color = get(toxicity_col))) +
        geom_point(size = 3) +  # Zwiększ rozmiar punktów
        scale_color_gradient(low = "red", high = "green") +
        labs(title = title, 
             x = "PC1", 
             y = "PC2", 
             color = legend_label) +
        theme_minimal() +
        geom_segment(data = vectors_df, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                     color = "blue", 
                     arrow = arrow(length = unit(0.3, "cm")), size = 1.2, inherit.aes = FALSE) +
        theme(legend.position = "bottom") +
        theme(legend.position = "bottom", 
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 8))
    } else {
      print(paste("Brak danych do narysowania strzałek dla", toxicity_col, "."))
      return(NULL)
    }
  } else {
    print(paste("Brak danych do analizy dla", toxicity_col, "."))
    return(NULL)
  }
}

create_and_plot <- function(toxicity_col, title, legend_label) {
  if ("No." %in% colnames(arkusz2)) {
    merged_df <- merge(pca_df, arkusz2[, c("No.", toxicity_col)], by = "No.")
    create_pca_plot(merged_df, toxicity_col, title, legend_label)
  } else {
    stop("Brak kolumny 'No.' w arkuszu2.")
  }
}


pca_daphnia_plot <- create_and_plot("Daphnia Magna", "Daphnia Magna Chronic Toxicity", "Daphnia Magna Toxicity [mg/L]")
pca_algae_plot <- create_and_plot("Algae", "Algae Chronic Toxicity", "Algae Toxicity [mg/L]")
pca_fish_plot <- create_and_plot("Fish", "Fish Acute Toxicity", "Fish Acute Toxicity [mg/L]")
pca_daphnia_ec_plot <- create_and_plot("Daphnia_EC", "Daphnia Magna Acute Toxicity", "Daphnia Magna Acute Toxicity [mg/L]")
pca_alga_ec_plot <- create_and_plot("Alga_EC", "Algae Acute Toxicity", "Algae Acute Toxicity [mg/L]")


grid.arrange(pca_daphnia_ec_plot, pca_alga_ec_plot, pca_fish_plot, pca_daphnia_plot, pca_algae_plot, nrow = 2)


ggsave("pca_plots.png", 
       arrangeGrob(
         pca_daphnia_ec_plot, 
         pca_alga_ec_plot, 
         pca_fish_plot, 
         pca_daphnia_plot, 
         pca_algae_plot, 
         nrow = 2
       ), 
       width = 14, height = 10, dpi = 300, units = "in", bg = "transparent")
