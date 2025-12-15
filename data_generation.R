# VERİ ÜRETİMİ
set.seed(123) # Tekrarlanabilirlik için
# PARAMETRELER 
n_respondents <- 50   # Katılımcı sayısı
n_questions <- 5      # Her bir katılımcının göreceği kart sayısı
n_alts <- 3           # Her bir karttaki seçenek sayısı
toplam_satir <- n_respondents * n_questions * n_alts
# ID: Katılıcı kimliği
# Set: Kart Numarası
# Alt: Seçenek numaraları (1, 2, 3)
sim_data <- data.frame(
  ID = rep(1:n_respondents, each = n_questions * n_alts),
  Set = rep(rep(1:n_questions, each = n_alts), times = n_respondents),
  Alt = rep(1:n_alts, times = n_respondents * n_questions)
)
# NİTELİK DÜZEYLERİNİ RASTGELE ATAMA
# Düzeyler (Levels) 1, 2, 3 gibi tamsayılar (Ayrık)
sim_data$Price <- sample(1:3, toplam_satir, replace = TRUE)  # 3 Düzey
sim_data$Density <- sample(1:3, toplam_satir, replace = TRUE) # 3 Düzey
sim_data$Type <- sample(1:2, toplam_satir, replace = TRUE)    # 2 Düzey
sim_data$Integ <- sample(1:2, toplam_satir, replace = TRUE)   # 2 Düzey
sim_data$Time <- sample(1:3, toplam_satir, replace = TRUE)    # 3 Düzey

#  TERCİHLERİ (CHOICE) SİMÜLE ETME 
# Gerçekçi olması için gizli bir fayda (Utility) fonksiyonu tanımlıyoruz.
# Örneğin: Fiyat arttıkça fayda düşmeli, Elektrikli bisiklet faydayı artırmalı.
# "Gerçek" katsayılar (Modelin bulmasını beklediğimiz değerler)
beta_price <- -1.5  # Fiyat arttıkça tercih düşer
beta_dens  <- 0.8   # İstasyon yoğunluğu arttıkça tercih artar
beta_type  <- 1.2   # Elektrikli (2) tercih sebebidir
beta_integ <- 0.5   # Entegrasyon (2) tercih sebebidir
beta_time  <- -0.3  # Süre uzadıkça tercih biraz düşebilir (veya nötr)

# Fayda Hesabı (Utility) = Beta * X + Hata Terimi
# Not: Kategorik değişkenleri numeric gibi çarparak basit bir simülasyon yaoılır
# Gerçek analizde bunlar faktör olarak kodlanmalıdır.
library(evd)
sim_data$Utility <- (beta_price * sim_data$Price) + 
  (beta_dens * sim_data$Density) + 
  (beta_type * sim_data$Type) + 
  (beta_integ * sim_data$Integ) + 
  (beta_time * sim_data$Time) + 
  rgumbel(toplam_satir, loc=0, scale=1) # Gumbel hatası (Logit için)


# Her Set içinde en yüksek faydaya sahip olanı seçilmesi (1)
# Veriyi ID ve Set'e göre gruplayıp en yüksek Fayda  seçiliyor, böylelikle aeçim 0 ya da 1 olarak atanabilir
library(dplyr) 

final_data <- sim_data %>%
  group_by(ID, Set) %>%
  mutate(MaxUtil = max(Utility)) %>%
  mutate(Choice = ifelse(Utility == MaxUtil, 1, 0)) %>%
  ungroup() %>%
  # Analiz için gereksiz sütunları (Utility, MaxUtil) temizleyelim
  select(ID, Set, Alt, Price, Density, Type, Integ, Time, Choice)

# Veriyi data.frame formatına geri çevrilir
final_data <- as.data.frame(final_data)

# SONUÇ GÖSTERİMİ 
head(final_data, 12) # İlk 12 satır (İlk kullanıcının 4 seti)

