# 1. Import danych ----
kraje_1 = read.table("kraje_makro_1.csv", header=TRUE, sep=",", dec=".")
kraje_2 = read.table("kraje_makro_2.csv", header=TRUE, sep=",", dec=".")

# 2. Instalowanie pakietów ----
# install.packages("readxl")
library(readxl)
 
# 3. Przygotowanie danych ----
head(kraje_1)	
head(kraje_2)      

head(kraje_1, 10)	
head(kraje_2, 10)

tail(kraje_1, 5)	
tail(kraje_2, 5)

summary(kraje_1)
summary(kraje_2)

mean(kraje_1$Przyrost_populacji)	
median(kraje_1$Przyrost_populacji)
min(kraje_1$Przyrost_populacji)	
max(kraje_1$Przyrost_populacji)	
  
# 4. Porz¹dkowanie nazw kolumn ----
kraje_1$X = NULL
kraje_2$X = NULL

colnames(kraje_2) = c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")


# 5. Porz¹dkowanie typów danych ----
is.numeric(kraje_2$Region)
is.character(kraje_2$Region)

kraje_2$Region = as.factor(kraje_2$Region)

summary(kraje_2)
levels(kraje_2$Region)
  
  
# 6. Porz¹dkowanie braków danych ----
colSums(is.na(kraje_1))	
colSums(is.na(kraje_2))	

sum(is.na(kraje_2$Internet_proc.)) 
kraje_2[is.na(kraje_2$Internet_proc.), ]
  
levels(kraje_2$Region)

kraje_2$Region <- gsub("&", "and", kraje_2$Region)
kraje_2$Region = as.factor(kraje_2$Region)
kraje = merge(kraje_1, kraje_2, by.x="Kod", by.y="Kod_kraju")
  
kraje$Nazwa = NULL
  
  
summary(kraje)
str(kraje)
  
# 7. Analiza danych ----
library(dplyr)

kraje = kraje %>%
  mutate(Populacja_mln = Populacja / 1e6)

kraje$Populacja_mln = kraje$Populacja / 1e6

kraje = kraje %>%
  mutate(PKB_per_capita = PKB / Populacja)

kraje$PKB_per_capita = kraje$PKB / kraje$Populacja

kraje %>%
  filter(Urbanizacja_proc. > 50)

kraje[kraje$Urbanizacja_proc. > 50, ]

kraje %>%
  select(Panstwo, Region, PKB, Populacja_mln)

kraje[, c("Panstwo", "Region", "PKB", "Populacja_mln")]

kraje %>%
  arrange(Przyrost_populacji)

kraje %>%
  arrange(desc(Przyrost_populacji))

kraje[order(kraje$Przyrost_populacji), ]  # rosn¹co
kraje[order(kraje$Przyrost_populacji, decreasing = TRUE), ]  # malej¹co

kraje %>%
  filter(PKB > 1e12) %>%
  arrange(PKB) %>%
  select(Panstwo, PKB, PKB_per_capita)

kraje_filtr = kraje[kraje$PKB > 1e12, ]

kraje_sort = kraje_filtr[order(kraje_filtr$PKB), ]

kraje_sort[, c("Panstwo", "PKB", "PKB_per_capita")]

kraje %>%
  filter(Region == "Sub-Saharan Africa") %>%
  select(Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja_proc.) %>%
  arrange(desc(PKB_per_capita))

kraje_reg = kraje[kraje$Region == "Sub-Saharan Africa", c("Panstwo", "PKB_per_capita", "Populacja_mln", "Urbanizacja_proc.")]

kraje_reg[order(kraje_reg$PKB_per_capita, decreasing = TRUE), ]

bogate = kraje %>%
  group_by(Region) %>%
  filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE))

bogate = kraje[kraje$PKB_per_capita > ave(kraje$PKB_per_capita, kraje$Region, 
                                          FUN = mean, na.rm = TRUE), ]
kraje %>%
  summarise(max_PKB_per_capita = max(PKB_per_capita, na.rm = TRUE))

max(kraje$PKB_per_capita, na.rm = TRUE)

kraje %>%
  summarise(
    min_populacja = min(Populacja_mln, na.rm = TRUE),
    max_populacja = max(Populacja_mln, na.rm = TRUE))

min(kraje$Populacja_mln, na.rm = TRUE)
max(kraje$Populacja_mln, na.rm = TRUE)

kraje %>%
  summarise(srednia_populacja = mean(Populacja_mln, na.rm = TRUE))

mean(kraje$Populacja_mln, na.rm = TRUE)

kraje %>%
  summarise(liczba_krajow = n())

nrow(kraje)

kraje %>%
  group_by(Region) %>%
  summarise(liczba_krajow = n())

table(kraje$Region)

kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))


{
  wynik = aggregate(cbind(Internet_proc., Urbanizacja_proc.) ~ Region,
                    kraje, mean, na.rm = TRUE)
  wynik$liczba_krajow = as.vector(table(kraje$Region)[wynik$Region])
  colnames(wynik) = c("Region", "sredni_internet", "srednia_urbanizacja", "liczba_krajow")
  wynik[order(-wynik$sredni_internet), ]
}

# 8. Tworzenie wykresów ----
# install.packages("ggplot2")
library(ggplot2)

# a) Prosty wykres punktowy: urbanizacja a PKB per capita
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita)) +
  geom_point() +
  labs(
    title = "Urbanizacja a PKB per capita",
    x = "Urbanizacja (%)",
    y = "PKB per capita")

# b) Zaawansowany wykres punktowy: urbanizacja a PKB per capita
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Urbanizacja a PKB per capita",
    subtitle = "Czy bardziej zurbanizowane kraje s¹ bogatsze?",
    x = "Urbanizacja (% ludnoœci miejskiej)",
    y = "PKB per capita (USD, skala log)",
    color = "Region œwiata"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom")

# c) Zaawansowany wykres punktowy: rozmiar gospodarki a populacja
ggplot(kraje, aes(x = Populacja_mln, y = PKB, size = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Skala gospodarki i demografia",
    x = "Populacja (mln, log10)",
    y = "PKB (USD, log10)",
    size = "PKB per capita"
  ) +
  theme_minimal()

# d) Prosty wykres s³upkowy: liczba krajów w regionach
ggplot(kraje, aes(x = Region)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(
    title = "Liczba krajów w regionach œwiata",
    x = "Region",
    y = "Liczba krajów"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5))

# e) Zaawansowany wykres s³upkowy poziomy: TOP 15 najbogatszych krajów
kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita), y = PKB_per_capita, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TOP 15 najbogatszych krajów œwiata (2016)",
    subtitle = "PKB per capita w USD",
    x = NULL,
    y = "PKB per capita (USD)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10))

# f) Wykres pude³kowy (boxplot): dostêp do internetu wed³ug regionów
ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median), 
                  y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Dostêp do internetu wed³ug regionów œwiata",
    subtitle = "(punkty to poszczególne kraje)",
    x = NULL,
    y = "Dostêp do internetu (% populacji)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none")

# g) Wykres pude³kowy (boxplot): przyrost populacji wed³ug regionów (mediana, rozrzut i obserwacje odstaj¹ce)
ggplot(kraje, aes(x = Region, y = Przyrost_populacji)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Tempo przyrostu populacji w regionach œwiata",
    subtitle = "(punkty to poszczególne kraje, linia przerywana = 0%)",
    x = "Region",
    y = "Przyrost populacji (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14))

# 9. Eksport danych ----
# install.packages("writexl")
library(writexl)

write.csv(kraje, "kraje_analiza.csv") 
write_xlsx(kraje, "kraje_wynik.xlsx")