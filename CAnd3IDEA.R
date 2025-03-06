# Install necessary packages if not already installed
install.packages(c("tidyverse", "MASS", "ggplot2", "knitr", "kableExtra", "ggpubr", "ordinal", "gridExtra", "grid", "car"))
install.packages("brant")



install.packages("data.table", type = "source")

# Load libraries
library(tidyverse)
library(MASS)
library(ggplot2)
library(knitr)    
library(kableExtra)
library(ggpubr) 
library(ordinal)
library(gridExtra)
library(grid)  
library(car)
library(broom)
library(brant)



df <- read.csv("/Users/samuelnemeroff/Documents/gss-12M0025-E-2017-c-31_F1.csv")

df <- df %>%
  mutate(
    # Recoding immigration status
    immigrant_status = case_when(
      BRTHCAN == 1 ~ "Non-Immigrant",
      BPR_16 == 1 ~ "Immigrant",
      TRUE ~ NA_character_
    ),
    immigrant_status = factor(immigrant_status),
    
    # Recoding parental contact (binary: Yes/No)
    parental_contact = case_when(
      LAF_50 %in% c(1,2,3,4) | LAM_50 %in% c(1,2,3,4) ~ 1, 
      LAF_50 == 5 & LAM_50 == 5 ~ 0,
      TRUE ~ NA_real_
    ),
    Parental_Contact = factor(parental_contact, levels = c(0, 1), labels = c("No", "Yes")),
    
    # Recoding children (binary: Has children vs. No children)
    children_binary = case_when(
      TOTCHDC %in% c(96, 97, 98, 99) ~ NA_real_,
      TOTCHDC == 0 ~ 0,
      TOTCHDC >= 1 ~ 1 
    ),
    Children_Status = factor(children_binary, levels = c(0, 1), labels = c("No Children", "1 or More Children")),
    
    df$SRH_115_label <- factor(df$SRH_115, levels = 1:5, 
                               labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
                               ordered = TRUE)
    
    
    # Recoding sex as a factor
    sex = factor(SEX, levels = c(1, 2), labels = c("Female", "Male")),
    
    # 🔹 Creating Immigrant_Children_Status
    Immigrant_Children_Status = case_when(
      immigrant_status == "Immigrant" & children_binary == 1 ~ "Immigrant w/ Children",
      immigrant_status == "Immigrant" & children_binary == 0 ~ "Immigrant w/o Children",
      immigrant_status == "Non-Immigrant" & children_binary == 1 ~ "Non-Immigrant w/ Children",
      immigrant_status == "Non-Immigrant" & children_binary == 0 ~ "Non-Immigrant w/o Children",
      TRUE ~ NA_character_
    ),
    Immigrant_Children_Status = factor(Immigrant_Children_Status)  # Convert to factor
  )




create_percentage_bar_chart <- function(df, group_var, title, filename) {
  # Compute valid percentages
  desc_table <- df %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(Percent = (Count / sum(Count)) * 100)  # Convert to percentage
  
  # Convert group variable to factor
  desc_table[[group_var]] <- factor(desc_table[[group_var]])
  
  # Create labels for legend with line breaks
  legend_labels <- paste0(desc_table[[group_var]], "\n(n=", desc_table$Count, ", ", round(desc_table$Percent, 1), "%)")
  
  # Create a percentage bar chart
  bar_chart <- ggplot(desc_table, aes(x = reorder(.data[[group_var]], -Percent), y = Percent, fill = .data[[group_var]])) +
    geom_bar(stat = "identity", color = "black") +
    labs(title = title, x = NULL, y = "Percentage", fill = NULL) +  # Remove legend title
    scale_fill_manual(values = c("steelblue", "firebrick", "purple", "green"), labels = legend_labels) +  # Custom colors
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "palegoldenrod", color = NA),  # Background color
      panel.background = element_rect(fill = "palegoldenrod", color = NA),
      panel.grid.major = element_blank(),  # Remove major gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text.x = element_blank(),  # Remove x-axis labels
      axis.ticks.x = element_blank(),  # Remove x-axis ticks
      axis.text.y = element_text(size = 12),
      legend.position = "right",  # Keep legend on the right
      legend.title = element_blank(),  # Remove legend title
      legend.text = element_text(size = 10, hjust = 0)  # Adjust text size & alignment for wrapping
    )
  
  # Save chart as PNG
  ggsave(filename, plot = bar_chart, width = 6, height = 4, dpi = 300)
  
  return(bar_chart)  # Display in RStudio
}

# 📌 1. Immigrants vs Non-Immigrants
p1 <- create_percentage_bar_chart(df, "immigrant_status", "Immigrants vs Non-Immigrants", "immigrants_vs_non.png")

# 📌 2. Immigrants With vs Without Parental Contact
p2 <- create_percentage_bar_chart(df %>% filter(immigrant_status == "Immigrant"), "Parental_Contact",
                                  "Parental Contact (Immigrants)", "immigrant_parental_contact.png")

# 📌 3. Non-Immigrants With vs Without Parental Contact
p3 <- create_percentage_bar_chart(df %>% filter(immigrant_status == "Non-Immigrant"), "Parental_Contact",
                                  "Parental Contact (Non-Immigrants)", "non_immigrant_parental_contact.png")

# 📌 4. Immigrants vs Non-Immigrants With and Without Children (With Wrapped Legend)
p4 <- create_percentage_bar_chart(df, "Immigrant_Children_Status",
                                  "Immigrants & Non-Immigrants With/Without Children", "immigrant_children_status.png")

# 📌 5. Combine into a Single Image
combined_plot <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Save final combined image with fixed alignment
ggsave("combined_descriptives_final.png", plot = combined_plot, width = 12, height = 8, dpi = 300)

# Display final combined plot
combined_plot



ggplot(df %>% filter(!is.na(immigrant_status)), aes(x = immigrant_status, fill = SRH_115_label)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Mental Health Ratings by Immigration Status",
       x = "Immigration Status", y = "Proportion",
       fill = "Self Rated Mental Health") +  # Updated legend title
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "palegoldenrod", color = NA),  # Background color
    panel.background = element_rect(fill = "palegoldenrod", color = NA),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12)
  )




ggplot(df %>% filter(!is.na(Children_Status)), aes(x = Children_Status, fill = SRH_115_label)) + 
  geom_bar(position = "fill") +
  labs(title = "Self-Rated Mental Health by Number of Children",
       x = "Children (No vs. 1+)", y = "Proportion",
       fill = "Self Rated Mental Health") +  # Updated legend title
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "palegoldenrod", color = NA),  # Background color
    panel.background = element_rect(fill = "palegoldenrod", color = NA),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12)
  )




save_table_as_image <- function(df, group_var, title, filename) {
  # Compute central tendency measures
  desc_table <- df %>%
    filter(!is.na(SRH_115), !is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      `Mean Mental Health` = round(mean(SRH_115, na.rm = TRUE), 2),
      `Median Mental Health` = median(SRH_115, na.rm = TRUE),
      `SD Mental Health` = round(sd(SRH_115, na.rm = TRUE), 2),
      `Number of Respondents` = n(),
      .groups = "drop"
    )
  
  # Convert to a ggplot2 table
  table_plot <- ggtexttable(desc_table, rows = NULL, theme = ttheme("light")) %>%
    tab_add_title(title)
  
  # Save as PNG
  ggsave(filename, plot = table_plot, width = 6, height = 4, dpi = 300)
  
  return(table_plot)  # Display in RStudio
}



# 📌 1. Immigrants vs Non-Immigrants
save_table_as_image(df, "immigrant_status", 
                    "Central Tendency of Mental Health by Immigration Status", 
                    "mental_health_immigrants_vs_non.png")

# 📌 2. Immigrants With vs Without Parental Contact
save_table_as_image(df %>% filter(immigrant_status == "Immigrant"), "Parental_Contact",
                    "Mental Health of Immigrants With and Without Parental Contact", 
                    "mental_health_immigrant_parental.png")

# 📌 3. Non-Immigrants With vs Without Parental Contact
save_table_as_image(df %>% filter(immigrant_status == "Non-Immigrant"), "Parental_Contact",
                    "Mental Health of Non-Immigrants With and Without Parental Contact", 
                    "mental_health_non_immigrant_parental.png")

# 📌 4. Immigrants With vs Without Children
save_table_as_image(df %>% filter(immigrant_status == "Immigrant"), "Children_Status",
                    "Mental Health of Immigrants With and Without Children", 
                    "mental_health_immigrant_children.png")

# 📌 5. Non-Immigrants With vs Without Children
save_table_as_image(df %>% filter(immigrant_status == "Non-Immigrant"), "Children_Status",
                    "Mental Health of Non-Immigrants With and Without Children", 
                    "mental_health_non_immigrant_children.png")



model_logit <- clm(SRH_115_label ~ immigrant_status + Parental_Contact + Children_Status + sex + AGEC + BRTHMACR,
                   data = df, link = "logit")
model_probit <- clm(SRH_115_label ~ immigrant_status + Parental_Contact + Children_Status + sex + AGEC + BRTHMACR,
                    data = df, link = "probit")

# Compare AIC values
AIC(model_logit, model_probit)

#technically a probit model has a better model fit
model_probit1 <- clm(SRH_115_label ~ immigrant_status + Parental_Contact + Children_Status + sex + AGEC + BRTHMACR,
             data = df, link = "probit")

summary(model_probit1)

#logit model is better for odds ratios so I've included it here for team discussion. Changing to logit model changes some of the significance, but we can express in ORs
model_logit1 <- clm(SRH_115_label ~ immigrant_status + Parental_Contact + Children_Status + sex + AGEC + BRTHMACR,
                    data = df, link = "logit")

summary(model_logit1)

brant(model_logit1)
#Aw beans, that's not good. I think this means we have to switch to a different model.

install.packages("VGAM")
library(VGAM)  


df$immigrant_status <- relevel(df$immigrant_status, ref = "Non-Immigrant")

model_ppom <- vglm(SRH_115_label ~ immigrant_status + Parental_Contact + Children_Status + sex + AGEC + BRTHMACR, 
                   family = cumulative(parallel = ~Parental_Contact + AGEC + BRTHMACR), 
                   data = df)



summary(model_ppom)



exp(coef(model_ppom))
AIC(model_ppom, model_logit1)




#riding the struggle bus but we're getting through

coefs <- coef(model_ppom)
odds_ratios <- exp(coefs)

or_df <- data.frame(
  Predictor = names(odds_ratios),
  OddsRatio = odds_ratios
)

print(or_df)

or_df <- or_df[!grepl("Intercept", or_df$Predictor), ]

or_df$Predictor <- gsub(":\\d+", "", or_df$Predictor)  # Remove cutoff numbers
or_df$Predictor <- gsub("_", " ", or_df$Predictor)  # Replace underscores

print(or_df)


ggplot(or_df, aes(x = reorder(Predictor, OddsRatio), y = OddsRatio, fill = Predictor)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Reference line at OR = 1
  labs(title = "Effect of Immigration, Children, and Gender on Self-Rated Health",
       x = "Predictor",
       y = "Odds Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")  # Hide legend

ggplot(or_df, aes(x = reorder(Predictor, OddsRatio), y = OddsRatio, fill = Predictor)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Reference line at OR = 1
  geom_text(aes(label = round(OddsRatio, 2)), vjust = -0.5, size = 5) +  # Add OR labels above bars
  labs(title = "Effect of Immigration, Children, and Gender on Self-Rated Health",
       x = "Predictor",
       y = "Odds Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#This is all stuff for model fit, normally I would have organized better but I was getting frustrated and sort of lost the plot. 
model_ppom_simple <- vglm(SRH_115_label ~ immigrant_status + Children_Status + sex, 
                          family = cumulative(parallel = FALSE), 
                          data = df, maxit = 100)

log

install.packages("rcompanion")
library(rcompanion)

nagelkerke(model_ppom_simple)

AIC(model_null)
AIC(model_ppom_simple)


model_null <- vglm(SRH_115_label ~ 1, 
                   family = cumulative(parallel = FALSE), 
                   data = df, maxit = 100)


summary(model_null)

df_clean <- df[!is.na(df$AGEC) & !is.na(residuals(model_ppom_simple, type = "response")), ]

plot(df_clean$AGEC, residuals(model_ppom_simple, type = "response"), 
     xlab = "Age (AGEC)", ylab = "Residuals", 
     main = "Checking Linearity of Age in PPOM")
install.packages("arm")


binned <- binnedplot(df$AGEC, residuals(model_ppom_simple, type = "working"),
                     xlab = "Age (AGEC)", ylab = "Binned Residuals",
                     main = "Binned Residuals Plot for Age")




#Final visualization, I think



# Custom x-axis labels
custom_labels <- c("immigrant statusNon-Immigrant" = "Non-immigrants",
                   "Children Status1 or More Children" = "Parents",
                   "sexMale" = "Male",
                   "AGEC" = "Age")

# Fix label spacing logic with `case_when()`
df_or_significant <- or_df %>%
  filter(!Predictor %in% c("BRTHMACR", "Parental ContactYes")) %>%
  mutate(
    label_position = case_when(
      Predictor == "sexMale" ~ -0.7,  # Space out Male odds ratios
      Predictor == "immigrant statusNon-Immigrant" ~ -1.0,  # Adjust immigrant status labels
      Predictor == "Children Status1 or More Children" ~ -1.5,  # Space out children odds ratios
      OddsRatio > 5 ~ -0.5,  # Position labels slightly below on tall bars
      OddsRatio < 0.8 ~ 1.2,  # Place labels above small bars
      TRUE ~ -0.2  # Default label position
    )
  )

# Create the refined odds ratio plot
ggplot(df_or_significant, aes(x = Predictor, y = OddsRatio, fill = Predictor)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1) +
  geom_text(aes(label = round(OddsRatio, 2), vjust = label_position), size = 5, fontface = "bold") +
  labs(title = "Effect of Immigration, Parenthood, and Gender on Self-Rated Health",
       x = "Predictor",
       y = "Odds Ratio") +
  scale_fill_manual(values = c("purple", "forestgreen", "darkorange", "steelblue")) + 
  scale_x_discrete(labels = custom_labels) +  # Apply custom x-axis labels
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),  # Horizontal labels
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "palegoldenrod", color = NA),  # Background color
    panel.background = element_rect(fill = "palegoldenrod", color = NA)
  )

# Save as high-resolution PNG
ggsave("odds_ratios_final_refined.png", width = 8, height = 6, dpi = 300)
