library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Dosyayı Oku (outputs klasöründe)
df_betas <- read.csv("outputs/rbetas.csv")

# 2. Sütun İsimlerini Anlaşılır Hale Getir
# Sıralama: Fiyat, Yoğunluk, Tip, Entegrasyon, Süre
colnames(df_betas) <- c("ID", "Fiyat_Beta", "Yogunluk_Beta", "Tip_Beta", "Integ_Beta", "Sure_Beta")

# Veriyi kontrol et
print(head(df_betas))

# ÖNEM DÜZEYİ HESAPLAMA

# Her niteliğin "Etki Gücü"nü hesaplayalım.
# Katsayıların mutlak değerinin ortalamasını alıyoruz (Mutlak değer çünkü yönü değil, şiddeti önemli)
onem_tablosu <- df_betas %>%
  select(-ID) %>%  # ID sütununu çıkar
  summarise(
    Fiyat = mean(abs(Fiyat_Beta)),
    Yogunluk = mean(abs(Yogunluk_Beta)),
    Tip = mean(abs(Tip_Beta)),
    Entegrasyon = mean(abs(Integ_Beta)),
    Sure = mean(abs(Sure_Beta))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Nitelik", values_to = "Skor") %>%
  mutate(Yuzde = (Skor / sum(Skor)) * 100) # Yüzdeye çevir

ggplot(onem_tablosu, aes(x = reorder(Nitelik, Yuzde), y = Yuzde, fill = Nitelik)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL,
       y = "Önem Düzeyi (%)") +
  theme(legend.position = "none",
        text = element_text(size = 12)) +
  geom_text(
    aes(label = sprintf("%%%.1f", Yuzde)),
    hjust = 1.1,         # Çubuğun içine gelecek şekilde
    fontface = "bold",
    color = "black",
    family = "sans"
  ) +
  scale_fill_brewer(palette = "Set2")


library(dplyr)
library(ggplot2)

# 1. Tahmin Edilen Değerleri Oku ve Ortalamasını Al
df_tahmin <- read.csv("outputs/rbetas.csv")

# Sütun isimlerinin düzenlenmesi
colnames(df_tahmin) <- c("ID", "Fiyat", "Yogunluk", "Tip", "Entegrasyon", "Sure")

# Ortalamaların hesaplanması
tahmin_ortalamalar <- colMeans(df_tahmin[, -1]) # ID hariç

# 2. Gerçek (Hedef) Değerleri Tanımla
gercek_degerler <- c(
  Fiyat = -1.5,
  Yogunluk = 0.8,
  Tip = 1.2,
  Entegrasyon = 0.5,
  Sure = -0.3
)

# 3. Yan Yana Karşılaştırma Tablosu
karsilastirma <- data.frame(
  Nitelik = names(gercek_degerler),
  Gercek_Beta = gercek_degerler,
  Tahmin_Beta = as.numeric(tahmin_ortalamalar)
)

# Ölçek farkını görmek için Oran (Ratio) hesaplayalım
# Model genelde her şeyi sabit bir katsayı ile çarpar (Scale Factor)
karsilastirma$Oran = karsilastirma$Tahmin_Beta / karsilastirma$Gercek_Beta

print("KATSAYI KARŞILAŞTIRMASI")
print(karsilastirma)

# 4. Görselleştirme (Scatter Plot)
# Eğer noktalar bir doğru üzerindeyse model mükemmel çalışmış demektir.
ggplot(karsilastirma, aes(x = Gercek_Beta, y = Tahmin_Beta, label = Nitelik)) +
  geom_point(color = "red", size = 4) +
  geom_text(vjust = -0.5, fontface = "bold") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(x = "Gerçek Değerler",
       y = "Modelin Tahminleri")

