# Tugas 1 PDM
# Memakai semua fitur
# Tidak Pakai CV
# Masih Memakai 3 Model

### --- LIBRARY-- ###
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
library(rpart.plot)

### --- DATA --- ###
data <- read_excel("data/dataset-uci.xlsx")

# struktur data
str(data)

### --- EKSPLORASI DATA -- ###
# ringkasan statistik
summary(data)

# melihat apakah ada nilai hilang (Na)
sapply(data, function(x) sum(is.na(x)))

# meilihat distribusi variabel target
table(data$`Gallstone Status`)
prop.table(table(data$`Gallstone Status`)) * 100

# visualisasi distribusi variabel numerik
## ambil semua kolom fitur (kecuali target)
fitur <- data %>%
  select(-'Gallstone Status')
## ambil nama 
nama_fitur <- names(fitur)
## histogram (semua variabel fitur sekaligus)
histogram_list <- nama_fitur %>%
  map(~ ggplot(data, aes(x = !!sym(.))) + 
      geom_histogram(bins = 30, fill = "darkblue", color = "white") + 
        labs(title = paste("Distribusi:", .)) +
        theme_minimal())
grid.arrange(grobs = histogram_list, ncol = 2)  
### nb: jika mau melihat per satu variabel fitur
# print(histogram_list[[1]])

## boxplot semua variabel fitur


# korelasi
## rubah target ke factor
data$`Gallstone Status` <- as.numeric(data$`Gallstone Status`)
## ambil hanya fitur tanpa target
fitur <- data[, -1]
## hitung matrik korelasi
korelasi_matriks <- cor(fitur)
# visualisasi heatmap
corrplot(korelasi_matriks, 
         method = "color",           # Tampilkan dengan warna
         type = "lower",             # HANYA tampilkan segitiga bawah (menghemat ruang)
         order = "hclust",           # Susun ulang berdasarkan pengelompokan (clustering)
         tl.col = "black",           # Warna label hitam
         tl.srt = 45,                # Putar label 45 derajat (membantu membaca)
         tl.cex = 0.5,               # PERKECIL ukuran font label (misal: 0.5 atau 0.4)
         cl.cex = 0.7,               # Perkecil ukuran font color-legend
         addCoef.col = "grey20",     # Tambahkan nilai korelasi numerik
         number.cex = 0.5            # Perkecil ukuran font angka korelasi
)

# korelasi fitur dengan target
korelasi_target <- cor(data[, -1], data$`Gallstone Status`)

## rubah ke dataframe agar rapi
korelasi_target_df <- data.frame(
  Variabel = rownames(korelasi_target),
  Korelasi = korelasi_target[, 1]
)

## jadikaan nilai mutlak lalu urutkan
korelasi_target_df <- korelasi_target_df %>%
  arrange(desc(abs(Korelasi)))

## melihat 10 fitur korelasi tertinggi
head(korelasi_target_df, 10)

### --- SPLITTING DATA --- ###
# konversi target ke factor
data$`Gallstone Status` <- as.factor(data$`Gallstone Status`)
# atur seed untuk reproduksibilitas
set.seed(42)
# buat indeks split (80:20)
split_index <- sample.split(data$`Gallstone Status`, SplitRatio = 0.80)
# buat subset data training dan testing
training_set <- subset(data, split_index == TRUE)
testing_set <- subset(data, split_index == FALSE)
# lihat training dan testing set
cat("Jumlah baris Training Set:", nrow(training_set), "\n")
cat("Jumlah baris Testing Set:", nrow(testing_set), "\n")

# Cek distribusi target di training set
cat("\nDistribusi Target di Training Set:\n")
print(prop.table(table(training_set$`Gallstone Status`)))

# Cek distribusi target di testing set
cat("\nDistribusi Target di Testing Set:\n")
print(prop.table(table(testing_set$`Gallstone Status`)))

### --- PEMODELAN --- ###
## -- REGRESI LOGISTIK -- ##
# bangun model
model_logreg <- glm(
  `Gallstone Status` ~ .,
  data = training_set,
  family = binomial
)
# ringkasan model
summary(model_logreg)
# prediksi pada data testing
probabilitas_prediksi <- predict(
  model_logreg,
  newdata = testing_set,
  type = "response"
)
# kembalikan probabilitas ke biner
prediksi_kelas_logreg <- ifelse(probabilitas_prediksi > 0.5, 1, 0)
# jadikan/pasikan hasil kelas prediksi adalah factor
prediksi_kelas_logreg <- as.factor(prediksi_kelas_logreg)
ground_truth <- testing_set$`Gallstone Status`
# confussion matrix
cm_logistik <- confusionMatrix(
  data = prediksi_kelas_logreg,
  reference = ground_truth,
  positive = "1"
)

cat("Confussion Matriks")
print(cm_logistik)

# precision = TP / (TP + FP)
precision_logreg <- Precision(y_pred = prediksi_kelas_logreg, y_true = ground_truth, positive = "1")
cat("\nPrecision", round(precision_logreg, 4), "\n")

# recall
recall_logreg <- Recall(y_pred = prediksi_kelas_logreg, y_true = ground_truth, positive = "1")
cat("\nRecall", round(recall_logreg, 4), "\n")

# F1 Score
f1_logreg <- F1_Score(y_pred = prediksi_kelas_logreg, y_true = ground_truth, positive = "1")
cat(cat("\nF1 Score", round(f1_logreg, 4), "\n"))

## -- KNN -- ##
# perlu dilakukan scaling pada variabel numeriknya agar tidak memberatkan komputasi
fitur_numerik <- names(training_set)[-1]
# standarisasi pada data training
# dapatkan parameter untuk scaling
scaler <- preProcess(training_set[, fitur_numerik], method = c("center", "scale"))
# terapkan scaler pada training
training_scaled <- predict(scaler, training_set)
# terapkan scaler pada testing set
testing_scaled <- predict(scaler, testing_set)
# verifikasi
str(training_scaled$`Gallstone Status`)
summary(training_scaled$Age)
# buat matriks untuk pelatihan
fitur_training <- training_scaled[, fitur_numerik]
fitur_testing <- testing_scaled[, fitur_numerik]
target_training <- training_scaled$`Gallstone Status`
target_testing <- testing_scaled$`Gallstone Status`
# modelling
k <- 10
# prediksi langsung
prediksi_knn <- knn(
  train = fitur_training,
  test = fitur_testing,
  cl = target_training,
  k = k
)
# confussion matrix
cm_knn <- confusionMatrix(
  data = prediksi_knn,
  reference = target_testing,
  positive = "1"
)

print(cm_knn)

# precision
precision_knn <- Precision(y_pred = prediksi_knn, y_true = target_testing, positive = "1")
cat(cat("\nPrecision", round(precision_knn, 4), "\n"))
# recall
recall_knn <- Recall(y_pred = prediksi_knn, y_true = target_testing, positive = "1")
cat("\nRecall ", round(recall_knn, 4), "\n")
# F1
f1_knn <- F1_Score(y_pred = prediksi_knn, y_true = target_testing, positive = "1")
cat("\nF1 Score", round(f1_knn, 4), "\n")

## -- DECISION TREE -- ##
# model
model_dt <- rpart(
  `Gallstone Status` ~ .,
  data = training_set,
  method = "class",
  control = rpart.control(minsplit = 20, cp = 0.01)
)
# visualisasi pohon keputusan
rpart.plot(model_dt, extra = 104, fallen.leaves = TRUE, main = "Decision Tree Plot")
# prediksi pada test
prediksi_kelas_dt <- predict(
  model_dt,
  newdata = testing_set,
  type = "class"
)
# prediksi probabilitas
probabilitas_prediksi_dt <- predict(
  model_dt,
  newdata = testing_set,
  type = "prob"
)

# confusion matriks
cm_dt <- confusionMatrix(
  data = prediksi_kelas_dt,
  reference = target_testing,
  positive = "1"
)

print(cm_dt)
# precision
precision_dt <- Precision(y_pred = prediksi_kelas_dt, y_true = target_testing, positive = "1")
cat("\nPrecision", round(precision_dt, 4), "\n")
# recall
recall_dt <- Recall(y_pred = prediksi_kelas_dt, y_true = target_testing, positive = "1")
cat("\nRecall", round(recall_dt, 4), "\n")
# F1
F1_dt <- F1_Score(y_pred = prediksi_kelas_dt, y_true = target_testing, positive = "1")
cat("\nF1 Score", round(F1_dt, 4), "\n")





