# TBP PDM

### --- LIBRARY --- ###
library(tidyverse)
library(dplyr)
library(readxl)
library(gridExtra)
library(corrplot)
library(caTools)
library(caret)
library(MLmetrics)
library(class)
library(rpart)
library(rsample)
library(smotefamily)
library(rpart.plot)

### --- DATA --- ### 
# import data
data <- read.csv("data/dataset-tbp.csv")
## -- EKSPLORASI DATA -- ##
# lihat beberapa baris pertama
head(data)
# struktur data
str(data)
# melihat apakah ada nilai hilang (Na)
sapply(data, function(x) sum(is.na(x)))

### --- EKSPLORASI DATA -- ###
# total missing values
total_missing <- sum(is.na(data))
cat("Total missing values:", total_missing)

# distribusi variabel target
table(data$Bankrupt.)
prop.table(table(data$Bankrupt.)) * 100
# Mengubah variabel Bankrupt? menjadi faktor untuk visualisasi diskret yang tepat
data$Bankrupt._Factor <- factor(data$Bankrupt., 
                                levels = c(0, 1),
                                labels = c("Tidak Bangkrut (0)", "Bangkrut (1)"))

# Membuat Bar Chart
ggplot(data, aes(x = Bankrupt._Factor, fill = Bankrupt._Factor)) +
  geom_bar() +
  # Menambahkan label count di atas setiap bar
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("Tidak Bangkrut (0)" = "steelblue", "Bangkrut (1)" = "firebrick")) +
  labs(
    title = "Distribusi Variabel Target: Bangkrut vs. Tidak Bangkrut",
    x = "Status Perusahaan",
    y = "Jumlah Perusahaan (Count)",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 


### --- PREPROCESSING --- ###

### --- SPLITTING DATA --- ###
# set target menjadi factor
data$Bankrupt. <- as.factor(data$Bankrupt.)
# pisahkan dulu data training dan testing, digunakan untuk menghindari data leakage
set.seed(42)
data_split <- initial_split(data, prop = 0.70, strata = Bankrupt.)

train_data <- training(data_split)
testing_data <- testing(data_split)

train_data <- train_data %>%
  select(-Bankrupt._Factor)
testing_data <- testing_data %>%
  select(-Bankrupt._Factor)

## -- BALANCING DATA -- ##
print("Distribusi Kelas di Data Pelatihan (Sebelum SMOTE):")
table(train_data$Bankrupt.)

# 2. Terapkan SMOTE (menggunakan fungsi 'SMOTE' dari smotefamily)
print("Menerapkan SMOTE dari smotefamily...")

smote_output <- SMOTE(
  # X = Fitur (Semua kolom kecuali Bankrupt.)
  X = train_data %>% select(-Bankrupt.), 
  # target = Variabel Target
  target = train_data$Bankrupt.,       
  # K = Jumlah tetangga terdekat yang digunakan (nilai standar 5)
  K = 5,                                 
  # dup_size = Rasio duplikasi per sampel minoritas (28x)
  dup_size = 28                          
)

# 3. Ekstrak dan Bersihkan Data Frame Baru
# Hasil SMOTE disimpan dalam elemen '$data' dan variabel target dinamai 'class'
train_data_smoted_sf <- smote_output$data %>%
  # Ganti nama 'class' kembali ke 'Bankrupt.'
  rename(Bankrupt. = class) %>%
  # Pastikan Bankrupt. tetap sebagai faktor
  mutate(Bankrupt. = factor(Bankrupt., levels = c("0", "1")))


# 4. Verifikasi Distribusi Target
print("---------------------------------------")
print("Distribusi Kelas Setelah SMOTE (smotefamily):")
table(train_data_smoted_sf$Bankrupt.)

print("Distribusi Persentase Setelah SMOTE (smotefamily):")
prop.table(table(train_data_smoted_sf$Bankrupt.)) * 100

print("Distribusi Persentase Target di Test")
table(testing_data$Bankrupt.)
prop.table(table(testing_data$Bankrupt.)) * 100

### --- PEMODELAN --- ###
## -- DECISION TREE -- ##
model_dt <- rpart(
  `Bankrupt.` ~ .,
  data = train_data_smoted_sf,
  method = "class",
  control = rpart.control(minsplit = 30, cp = 0.01)
)
## EVALUASI DAN PREDIKSI
prediksi_kelas_dt <- predict(
  model_dt,
  newdata = testing_data,
  type = "class"
)

# CM
cm_dt <- confusionMatrix(
  data = prediksi_kelas_dt,
  reference = testing_data$Bankrupt.,
  positive = "1"
)
print(cm_dt)

## Matriks Evaluasi
precision_dt <- Precision(y_pred = prediksi_kelas_dt,
                         y_true = testing_data$Bankrupt.,
                         positive = "1")
recall_dt <- Recall(y_pred = prediksi_kelas_dt,
                    y_true = testing_data$Bankrupt.,
                    positive = "1")
f1_dt <- F1_Score(y_pred = prediksi_kelas_dt,
                  y_true = testing_data$Bankrupt.,
                  positive = "1")
dt_metrics <- data.frame(
  metrik = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Nilai = c(cm_dt$overall['Accuracy'], precision_dt, recall_dt, f1_dt)
)
dt_metrics
