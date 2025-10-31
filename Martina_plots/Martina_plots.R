library(readxl)
library(dplyr)
library(lmtest)
library(stargazer)
library(tidyr)
library(stargazer)
library(ggplot2)
library(GGally)


#Open dataset
dataset <- read_excel("/Users/User/Downloads/fuel_prices_tidy.xlsx")

# I set the numeric variables and create descriptive statistics table
summary_table <- dataset %>%
  select(Gasoline,
         Diesel,
         Population,
         income) %>%
  summarise(across(
    everything(),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      min = ~min(.x, na.rm = TRUE),
      Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      Q3 = ~quantile(.x, 0.75, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    )
  ))

summary_table_long <- summary_table %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Statistic"),
               names_sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = value)

stargazer(summary_table_long,
          type = "text",
          summary = FALSE,
          title = "Descriptive statistics",
          digits = 2)

#a. Plotting the distributions of the numerical variables from the previous part
ggplot(dataset, aes(x = `Gasoline`)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Fuel Prices", x = "Price", y = "Frequency")

ggplot(dataset, aes(x = `Diesel`)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  labs(title = "Distribution of Disesel Prices", x = "Price", y = "Frequency")

ggplot(dataset, aes(x = `Population`)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "white") +
  labs(title = "Distribution of Population", x = "Population", y = "Frequency")

ggplot(dataset, aes(x = `income`)) +
  geom_histogram(bins = 30, fill = "grey", color = "white") +
  labs(title = "Distribution of Income", x = "Income", y = "Frequency")


#Dummy shares about the settlement types and corporations
#Checking the ratio among settlement types
legal_share <-dataset %>%
  count(LegalStatus) %>%
  mutate(share = 100 * n / sum(n))
#Plotting the ratio in settlement types
ggplot(legal_share, aes(x = reorder(LegalStatus, -share), y = share, fill = LegalStatus)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(share, 1), "%")), 
            vjust = -0.5, size = 3, color = "black") +
  labs(
    title = "Settlement type shares in the sample",
    x = "Settlement type",
    y = "Share (%)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

#Checking the ratio among companies
company_share <- dataset %>%
  count(Company) %>%
  mutate(share = 100 * n / sum(n)) %>%
  arrange(desc(share)) %>%
  slice(1:10)

#Plotting the diagram about the ratios
ggplot(company_share, aes(x = reorder(Company, share), y = share, fill = Company)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = paste0(round(share, 1), "%")), 
            hjust = 1.2, color = "black", fontface = "bold", size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  
  labs(
    title = "Largest corporations in the sample",
    x = "Company",
    y = "Share (%)"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 13)

#Correlation between Gasoline and Diesel prices
ggplot(data = dataset,
       mapping = aes(y = Gasoline,
                     x = Diesel)) +
  geom_point()

cor(dataset$Diesel, dataset$Gasoline, use = "complete.obs")

#Plotting Income and Fuel Price relationships
ggplot(dataset, aes(x = income, y = Gasoline)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Income and Gasoline price",
       x = "Income", y = "Gasoline price")

ggplot(dataset, aes(x = income, y = Diesel)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Income and Diesel price",
       x = "Income", y = "Diesel price")


#Diagram about Population and Fuel price relationships
ggplot(dataset, aes(x = Population, y = Gasoline)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +
  labs(title = "Relationship between Population and Fuel Price",
       x = "Population", y = "Gasoline price")

ggplot(dataset, aes(x = Population, y = Diesel)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Relationship between Population and Fuel Price",
       x = "Population", y = "Diesel price")



ggplot(dataset, aes(x = nearest_diff_brand_km, y = Gasoline)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "purple", se = FALSE) +
  theme_minimal() +
  labs(title = "Relationship between the closest competing brand and Gasoline price",
       x = "Nearest brand",
       y = "Gasoline price")

ggplot(dataset, aes(x = nearest_diff_brand_km, y = Diesel)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "pink", se = FALSE) +
  theme_minimal() +
  labs(title = "Relationship between the closest competing brand and Diesel price",
       x = "Nearest brand",
       y = "Diesel price")

ggplot(dataset, aes(x = County, y = Gasoline)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Gasoline price on County level", x = "County", y = "Price")

ggplot(dataset, aes(x = County, y = Diesel)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Diesel price on County level", x = "County", y = "Price")

ggplot(dataset, aes(x = LegalStatus, y = Gasoline)) +
  geom_boxplot() +
  labs(title = "Gasoline price and Settlement type",
       x = "Settlement type", y = "Gasoline price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dataset, aes(x = LegalStatus, y = Diesel)) +
  geom_boxplot() +
  labs(title = "Diesel price and Settlement type",
       x = "Settlement type", y = "Diesel price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


