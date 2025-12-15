library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Dosyayı Oku (outputs klasöründe olduğunu varsayıyorum)
# Dosya yolunu kendi bilgisayarınızdaki tam yola göre düzenleyebilirsiniz.
df_betas <- read.csv("outputs/rbetas.csv")

# 2. Sütun İsimlerini Anlaşılır Hale Getir
# Sıralama: Fiyat, Yoğunluk, Tip, Entegrasyon, Süre
colnames(df_betas) <- c("ID", "Fiyat_Beta", "Yogunluk_Beta", "Tip_Beta", "Integ_Beta", "Sure_Beta")

# Veriyi kontrol et
print(head(df_betas))

# --- ÖNEM DÜZEYİ HESAPLAMA ---

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

# --- GRAFİK ÇİZİMİ ---
ggplot(onem_tablosu, aes(x = reorder(Nitelik, Yuzde), y = Yuzde, fill = Nitelik)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Yatay çubuk
  theme_minimal() +
  labs( x = NULL,
       y = "Önem Düzeyi (%)") +
  theme(legend.position = "none",
        text = element_text(size = 16,face="bold",family = "serif")) +
  geom_text(aes(label = sprintf("%%%.1f", Yuzde)), hjust = 0.01, size = 7,fontface = "bold",family = "serif") +
  scale_fill_brewer(palette = "Set1")