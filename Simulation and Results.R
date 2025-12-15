library(ggplot2)
library(dplyr)
library(tidyr)

# 1. VERİYİ HAZIRLA
df_betas <- read.csv("outputs/rbetas.csv")
colnames(df_betas) <- c("ID", "Beta_Price", "Beta_Dens", "Beta_Type", "Beta_Integ", "Beta_Time")

# 2. SENARYOLARI (ÜRÜNLERİ) TANIMLA
senaryolar <- data.frame(
  Urun = c("Seçenek A", "Seçenek B", "Mevcut"),
  Price = c(1, 3, 2),
  Dens  = c(1, 2, 3),
  Type  = c(1, 2, 1),
  Integ = c(1, 1, 2),
  Time  = c(2, 2, 2)
)

# 3. SİMÜLASYON FONKSİYONU
hesapla_pazar_payi <- function(betas, urunler) {
  n_kisi <- nrow(betas)
  n_urun <- nrow(urunler)
  olasiliklar <- matrix(0, nrow = n_kisi, ncol = n_urun)
  
  for (i in 1:n_kisi) {
    b <- betas[i, ]
    utilities <- numeric(n_urun)
    
    for (j in 1:n_urun) {
      u <- (b$Beta_Price * urunler$Price[j]) +
        (b$Beta_Dens  * urunler$Dens[j]) +
        (b$Beta_Type  * urunler$Type[j]) +
        (b$Beta_Integ * urunler$Integ[j]) +
        (b$Beta_Time  * urunler$Time[j])
      utilities[j] <- u
    }
    
    exp_u <- exp(utilities)
    probs <- exp_u / sum(exp_u)
    olasiliklar[i, ] <- probs
  }
  pazar_paylari <- colMeans(olasiliklar) * 100
  return(pazar_paylari)
}

# Fonksiyonu çalıştır
sonuc_paylar <- hesapla_pazar_payi(df_betas, senaryolar)

# Sonucu veri çerçevesine dök
simulasyon_sonucu <- data.frame(
  Urun = senaryolar$Urun,
  Pazar_Payi = sonuc_paylar
)

# 4. GÖRSELLEŞTİRME (ŞEKİL 5) - GÜNCELLENMİŞ KOD
ggplot(simulasyon_sonucu, aes(x = Urun, y = Pazar_Payi, fill = Urun)) +
  geom_bar(stat = "identity", width = 0.6) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Tahmini Pazar Payı (%)" # Y ekseni başlığı
  ) +
  geom_text(aes(label = sprintf("%%%.1f", Pazar_Payi)), vjust = -0.5, fontface = "bold", family = "serif", size = 5) +
  scale_fill_manual(values = c("gray70", "gray40", "#2E86C1")) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        # X ekseni metin fontu
        axis.text.x = element_text(size = 14, face = "bold", family = "serif"),
        # Y EKSENİ BAŞLIĞI İÇİN GEREKLİ EKLEME
        axis.title.y = element_text(face = "bold", size = 14, family = "serif")) +
  ylim(0, 100)