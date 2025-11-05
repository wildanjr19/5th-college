# Script R
# Team Based Project - Kelompok F

### --- LIBRARY --- ###
library(tidyverse)
library(corrplot)
library(caret)
library(MLmetrics)
library(rpart)
library(rsample)
library(smotefamily)
library(rpart.plot)
library(randomForest)
library(e1071)       
library(pROC)
library(dplyr)
library(ggplot2)
library(tidymodels)
library(themis)

### --- DATA --- ###
data <- read.csv(
  "data/dataset-tbp.csv"
)

### --- EKSPLORASI DATA --- ###
head(data)
str(data)
sapply(data, function(x) sum(is.na(x)))

# Cek missing value dan duplikasi
cat("Total missing values:", sum(is.na(data)), "\n")
cat("Total duplikasi baris:", sum(duplicated(data)), "\n")

# Pastikan target adalah faktor "0"/"1"
data$Bankrupt. <- factor(data$Bankrupt., levels = c(0,1), labels = c("0","1"))

# Visualisasi distribusi target
data$Bankrupt._Factor <- factor(
  data$Bankrupt., levels = c("0","1"),
  labels = c("Tidak Bangkrut (0)", "Bangkrut (1)")
)

ggplot(data, aes(x = Bankrupt._Factor, fill = Bankrupt._Factor)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(title = "Distribusi Variabel Target: Bangkrut vs Tidak Bangkrut",
       x = "Status Perusahaan", y = "Jumlah Perusahaan") +
  theme_minimal() +
  theme(legend.position = "none")

# Statistik deskriptif
cat("\nRingkasan statistik deskriptif variabel numerik:\n")
print(summary(dplyr::select_if(data, is.numeric)))

### --- FEATURE SELECTION --- ###

## -- Hapus Multikolinearitas -- ##
cat("\n=== (1) Hapus Multikolinearitas ===\n")
num_data <- dplyr::select_if(data, is.numeric)

# Buang variansi yang mendekati nol (near-zero variance)
nzv_info <- caret::nearZeroVar(num_data, saveMetrics = TRUE)
drop_nzv <- rownames(nzv_info)[nzv_info$zeroVar | nzv_info$nzv]
if (length(drop_nzv) > 0) {
  cat("Hapus NZV:", paste(drop_nzv, collapse = ", "), "\n")
  num_data <- dplyr::select(num_data, -all_of(drop_nzv))
} else cat("Tidak ada fitur NZV.\n")

# Buang standar deviasi yang mendekati nol
sd_zero <- names(which(sapply(num_data, function(x) sd(x, na.rm = TRUE)) == 0))
if (length(sd_zero) > 0) {
  cat("Hapus sd=0:", paste(sd_zero, collapse = ", "), "\n")
  num_data <- dplyr::select(num_data, -all_of(sd_zero))
}

# Hapus fitur yang memiliki korelasi < - 0.8 dan > 0.8 
if (ncol(num_data) >= 2) {
  cor_matrix <- suppressWarnings(cor(num_data, use = "pairwise.complete.obs"))
  cor_matrix[is.na(cor_matrix)] <- 0
  high_corr <- caret::findCorrelation(cor_matrix, cutoff = 0.80, names = TRUE)
  cat("Hapus korelasi tinggi:", paste(high_corr, collapse = ", "), "\n")
  data_nocorr <- data %>% dplyr::select(-all_of(high_corr))
} else {
  warning("Fitur numerik < 2 setelah NZV; lewati tahap korelasi.")
  data_nocorr <- data
}

# Cek kembali
str(data_nocorr)
count(data_nocorr)

# ambil numerik dan variabel target
num_feats <- dplyr::select_if(data_nocorr, is.numeric)
target <- data_nocorr$Bankrupt.

## -- Uji Chi-Square -- ##
cat("\n=== Seleksi Fitur Dengan Chi-Square dan Validasi ===\n")
# Fungsi untuk menghitung chi-square
chi_square_p <- function(x, y, bins = 5) {
  x_cut <- tryCatch(
    cut(x, breaks = quantile(x, probs = seq(0,1,length.out=bins+1), na.rm = TRUE),
        include.lowest = TRUE),
    error = function(e) return(NULL)
  )
  if (is.null(x_cut)) return(NA_real_)
  tbl <- table(x_cut, y)
  if (min(tbl) == 0) return(NA_real_)
  suppressWarnings(chisq.test(tbl)$p.value)
}

# Apply fungsi dan bentuk dataframe dari p-valuenya
pvals <- sapply(num_feats, chi_square_p, y = target)
pvals_df <- tibble(Fitur = names(pvals), p_value = pvals) %>%
  filter(!is.na(p_value)) %>%
  arrange(p_value)

## -- Seleksi Dengan Beberapa Kombinasi Fitur -- ##
# Buat ketentuan looping
k_list <- seq(5, min(60, nrow(pvals_df)), by = 5) 

# Buat dataframe untuk store scorenya
scores <- data.frame(k = integer(), F1 = numeric(), AUC = numeric())

# Looping
for (k in k_list) {
  feats <- head(pvals_df$Fitur, k)
  dat_sel <- data_nocorr %>% dplyr::select(all_of(feats), Bankrupt.)
  
  # Split sementara untuk validasi
  set.seed(42)
  split_tmp <- initial_split(dat_sel, prop = 0.8, strata = Bankrupt.)
  train_tmp <- training(split_tmp)
  test_tmp  <- testing(split_tmp)
  
  # Balancing dengan SMOTE-Tomek
  rec <- recipe(Bankrupt. ~ ., data = train_tmp) %>%
    step_smote(Bankrupt.) %>% step_tomek(Bankrupt.)
  prep_rec <- prep(rec, training = train_tmp)
  train_bal <- bake(prep_rec, new_data = NULL)
  test_bal  <- bake(prep_rec, new_data = test_tmp)
  
  # Coba dengan logistic regression
  glm_fit <- glm(Bankrupt. ~ ., data = train_bal, family = binomial)
  prob <- predict(glm_fit, newdata = test_bal, type = "response")
  cls  <- factor(ifelse(prob >= 0.5, "1","0"), levels = c("0","1"))
  truth <- test_bal$Bankrupt.
  
  # Hitung F1 & AUC
  TP <- sum(cls=="1" & truth=="1"); FP <- sum(cls=="1" & truth=="0")
  FN <- sum(cls=="0" & truth=="1")
  prec <- TP/(TP+FP); rec <- TP/(TP+FN)
  f1 <- 2*prec*rec/(prec+rec)
  aucv <- as.numeric(roc(truth, prob, levels=c("0","1"))$auc)
  
  scores <- rbind(scores, data.frame(k=k, F1=f1, AUC=aucv))
}

# Ambil fitur terbaik dari hasil scores tertingi
best_k <- scores$k[which.max(scores$F1)]
cat(sprintf("\nJumlah fitur terbaik berdasarkan F1 validation: %d\n", best_k))

# Visualisasi hasil evaluasi
ggplot(scores, aes(x=k, y=F1)) +
  geom_line(color="steelblue", linewidth=1) + 
  geom_point(color="firebrick", size=2) +
  geom_vline(xintercept = best_k, linetype="dashed", color="darkred") +
  labs(title="Evaluasi Jumlah Fitur",
       x="Jumlah fitur", y="F1 Score (Validation)") +
  theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

# Gunakan fitur terpilih
top_features <- head(pvals_df$Fitur, best_k)
cat("\nFitur terpilih otomatis berdasarkan uji Chi-square + F1 tertinggi:\n")
print(top_features)

# Dataset akhir dengan fitur terpilih
data_selected <- data_nocorr %>% dplyr::select(all_of(top_features), Bankrupt.)
view(data_selected)

### --- DATA SPLITTING --- ###
# Set seed untuk reproduksibilitas
set.seed(42)
# Proprosi 70:30
data_split <- initial_split(data_selected, prop = 0.7, strata = Bankrupt.)
# Train
train_data <- training(data_split)
train_data
# Testing
test_data  <- testing(data_split)
test_data

## -- Balancing dengan SMOTE-Tomek pada Data Train -- ##
# Konfigurasi SMOTE-TOMEK
rec <- recipe(Bankrupt. ~ ., data = train_data) %>%
  step_smote(Bankrupt.) %>%  
  step_tomek(Bankrupt.)       
prep_rec <- prep(rec, training = train_data)

# Apply SMOTE-TOMEK
train_bal <- bake(prep_rec, new_data = NULL)
test_bal  <- bake(prep_rec, new_data = test_data)

## -- Validasi -- ##
# Cek kembali
cat("Distribusi setelah SMOTE–Tomek:\n")
print(table(train_bal$Bankrupt.))

# Hitung distribusi target sebelum dan sesudah balancing (untuk visualisasi)
# Sebelum
before_tbl <- train_data %>%
  count(Bankrupt.) %>%
  mutate(Status = "Sebelum")

# Sesudah
after_tbl <- train_bal %>%
  count(Bankrupt.) %>%
  mutate(Status = "Sesudah")

# Gabungkan
dist_df <- bind_rows(before_tbl, after_tbl) %>%
  mutate(Bankrupt. = recode(Bankrupt., "0" = "Tidak Bangkrut", "1" = "Bangkrut"))

# Plot
ggplot(dist_df, aes(x = Bankrupt., y = n, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(width = 0.7), vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("#4B9CD3", "#E74C3C")) +
  labs(title = "Distribusi Target Sebelum dan Sesudah Penerapan SMOTE–Tomek",
       x = "Kelas Perusahaan", y = "Jumlah Data",
       fill = "Kondisi") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"))

### --- PEMODELAN DAN EVALUASI --- ###
# Buat fungsi untuk matrik evaluasi
metrics_simple <- function(truth, pred_class, prob1 = NULL) {
  truth <- factor(truth, levels = c("0","1"))
  pred_class <- factor(pred_class, levels = c("0","1"))
  cm <- table(Pred = pred_class, Truth = truth)
  TP <- cm["1","1"]; TN <- cm["0","0"]; FP <- cm["1","0"]; FN <- cm["0","1"]
  acc <- (TP + TN) / sum(cm)
  prec <- ifelse((TP+FP)==0, NA, TP/(TP+FP))
  rec  <- ifelse((TP+FN)==0, NA, TP/(TP+FN))
  f1   <- ifelse(is.na(prec) || is.na(rec) || (prec+rec)==0, NA, 2*prec*rec/(prec+rec))
  auc  <- if (is.null(prob1)) NA else as.numeric(roc(truth, prob1, levels = c("0","1"))$auc)
  list(Accuracy=acc, Precision=prec, Recall=rec, F1=f1, AUC=auc)
}

## -- Decision Tree -- ##
dt_fit <- rpart(Bankrupt. ~ ., data = train_bal, method = "class")
dt_prob <- predict(dt_fit, newdata = test_bal, type = "prob")[, "1"]
dt_cls  <- factor(ifelse(dt_prob >= 0.5, "1","0"), levels = c("0","1"))
dt_met  <- metrics_simple(test_bal$Bankrupt., dt_cls, dt_prob)

## -- Logistic Regression -- ##
lg_fit <- glm(Bankrupt. ~ ., data = train_bal, family = binomial)
lg_prob <- predict(lg_fit, newdata = test_bal, type = "response")
lg_cls  <- factor(ifelse(lg_prob >= 0.5, "1","0"), levels = c("0","1"))
lg_met  <- metrics_simple(test_bal$Bankrupt., lg_cls, lg_prob)

## -- XGBoost -- ##
# Siapkan data untuk XGBoost
X_train <- model.matrix(Bankrupt. ~ . - 1, data = train_bal)
y_train <- as.numeric(as.character(train_bal$Bankrupt.))
X_test  <- model.matrix(Bankrupt. ~ . - 1, data = test_bal)

dtrain <- xgb.DMatrix(X_train, label = y_train)
dtest  <- xgb.DMatrix(X_test)

# List parameter untuk XGBoost
param <- list(objective="binary:logistic", eval_metric="auc", max_depth=7,
              eta=0.1, subsample=0.8, colsample_bytree=0.8)

xgb_model <- xgb.train(params=param, data=dtrain, nrounds=300, verbose=0)
xgb_prob <- predict(xgb_model, dtest)
xgb_cls  <- factor(ifelse(xgb_prob >= 0.5, "1","0"), levels = c("0","1"))
xgb_met  <- metrics_simple(test_bal$Bankrupt., xgb_cls, xgb_prob)


# Dataframe hasil dari tiap model
hasil <- rbind(
  data.frame(Model="Decision Tree",  Accuracy=dt_met$Accuracy,  Precision=dt_met$Precision,  Recall=dt_met$Recall,  F1=dt_met$F1,  AUC=dt_met$AUC),
  data.frame(Model="Logistic Regression", Accuracy=lg_met$Accuracy, Precision=lg_met$Precision, Recall=lg_met$Recall, F1=lg_met$F1, AUC=lg_met$AUC),
  data.frame(Model="XGBoost",        Accuracy=xgb_met$Accuracy, Precision=xgb_met$Precision, Recall=xgb_met$Recall, F1=xgb_met$F1, AUC=xgb_met$AUC)
)
print(hasil, digits = 3)


### --- VISUALISASI PERBANDINGAN MODEL --- ###
hasil_long <- hasil %>%
  pivot_longer(cols = c(Accuracy, Precision, Recall, F1, AUC), 
               names_to = "Metric", 
               values_to = "Value")

ggplot(hasil_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Perbandingan Performa Model",
       x = "Model", y = "Nilai Metrik") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle =45,hjust=1))


### --- PLOT CONFUSSION MATRIX --- ###
# Fungsi untuk plot confussion matrix
plot_cm <- function(truth, pred, title = "", normalize = c("true","none","pred","all")) {
  normalize <- match.arg(normalize)
  truth <- factor(truth, levels = c("0","1"))
  pred  <- factor(pred,  levels = c("0","1"))
  
  # Plot CM 2X2
  cm <- table(Truth = truth, Pred = pred)
  cm <- cm[levels(truth), levels(pred), drop = FALSE] 
  
  # Normalisasi
  val <- switch(normalize,
                "true" = prop.table(cm, 1),
                "pred" = prop.table(cm, 2),
                "all"  = prop.table(cm),
                "none" = cm * 1.0
  )
  
  # Buat ke dataframe dulu
  df <- as.data.frame(as.table(cm))
  names(df) <- c("Truth","Pred","Count")
  df$Value <- as.vector(val)
  
  # Label di cell
  df$Metric <- c(
    "True Negative", # Truth=0, Pred=0
    "False Negative", # Truth=1, Pred=0
    "False Positive", # Truth=0, Pred=1
    "True Positive"  # Truth=1, Pred=1
  )
  
  # Gabungkan dataframe tadi dengan label
  df$label <- if (normalize == "none") {
    sprintf("%s\n%d", df$Metric, df$Count)
  } else {
    sprintf("%s\n%d\n%.1f%%", df$Metric, df$Count, 100*df$Value)
  }
  
  vmax <- if (normalize == "none") max(df$Value) else 1
  
  # Plot
  ggplot2::ggplot(df, ggplot2::aes(Pred, Truth, fill = Value)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 4) +
    ggplot2::scale_fill_gradient2(low = "green", mid = "yellow", high = "blue", midpoint = 0.5) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = paste("Confusion Matrix —", title),
      subtitle = switch(normalize,
                        "none"="Counts (no normalization)",
                        "pred"="Normalized by Predicted (Precision)",
                        "all"="Normalized by Total"
      ),
      x = "Predicted", y = "Actual",
      fill = ifelse(normalize == "none", "Count", "Value")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
}

# Buat fungsi untuk apply fungsi plot_cm sebelumnya
plot_all_cms <- function(truth, model_preds, normalize = "true") {
  truth <- factor(truth, levels = c("0","1"))
  lapply(names(model_preds), function(nm) {
    plot_cm(truth, model_preds[[nm]], title = nm, normalize = normalize)
  })
}

# Apply plot_all_cms ke hasil model
cm_plots <- plot_all_cms(truth_vec, model_preds, normalize = "true")

# Tampilkan salah satu
print(cm_plots[[3]])

### --- KURVA ROC AUC XGBOOST --- ###
# Ambil nilai sebenarnya (ground truth)
truth_vec <- test_bal$Bankrupt. 

# Membuat objek ROC
roc_obj <- roc(
  response = truth_vec,  # Nilai aktual 
  predictor = xgb_prob,  # Probabilitas XGBoost
  levels = c("0", "1")   # Level dari target
)

# Plot Kurva ROC
plot(
  roc_obj, 
  main = "ROC AUC XGBoost",
  col = "#002F6C", 
  lwd = 2,
  print.auc = TRUE, 
  legacy.axes = TRUE, # Sumbu TPR dan FPR
  xlab = "False Positive Rate (1 - Specificity)", 
  ylab = "True Positive Rate (Sensitivity/Recall)"
)

# Ambil nilai AUC
auc_value <- auc(roc_obj)

# Nilai AUC
cat("\nNilai AUC XGBoost:", round(auc_value, 4), "\n")
