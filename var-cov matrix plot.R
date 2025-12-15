library(ggplot2)

# --- VERİ HAZIRLIĞI (Önceki kodun aynısı) ---
x_ekseni <- seq(-6, 6, length.out = 1000)
var_fiyat <- 4.5
var_tip   <- 0.8
sd_fiyat <- sqrt(var_fiyat)
sd_tip   <- sqrt(var_tip)
y_fiyat <- dnorm(x_ekseni, mean = 0, sd = sd_fiyat)
y_tip   <- dnorm(x_ekseni, mean = 0, sd = sd_tip)

df_varyans <- data.frame(
  Tercih_Skoru = rep(x_ekseni, 2),
  Yogunluk = c(y_fiyat, y_tip),
  Nitelik = factor(rep(c("Fiyat (Yüksek Varyans: 4.5)", 
                         "Bisiklet Tipi (Düşük Varyans: 0.8)   "), 
                       each = 1000))
)

# --- GRAFİK ÇİZİMİ (GÜNCELLENMİŞ TASARIM) ---
ggplot(df_varyans, aes(x = Tercih_Skoru, y = Yogunluk, fill = Nitelik, color = Nitelik)) +
  geom_area(alpha = 0.4, position = "identity") +
  geom_line(size = 1) +
  
  scale_fill_manual(values = c("#E74C3C", "#2980B9")) +
  scale_color_manual(values = c("#C0392B", "#2471A3")) +
  
  # Akademik görünüm için 'theme_bw' daha uygundur (Siyah çerçeve)
  theme_bw() + 
  
  labs(x = "Tercih Katsayısı (Beta Değeri)",
       y = "Olasılık Yoğunluğu") +
  
  # Görsel Üzerine Notlar Ekleyelim (Annotation)
  annotate("text", x = 3.5, y = 0.1, label = "FİYAT:\nFarklı Davranışlar\n(Heterojen)", 
           color = "black", fontface = "bold",family = "serif") +
  annotate("text", x = 2.5, y = 0.25, label = "BİSİKLET TİPİ:\n Benzer Davranışlar\n(Homojen)", 
           color = "black", fontface = "bold", size = 3.5,family = "serif") +
  
  # --- FONT VE BOYUT AYARLARI BURADA ---
  theme(
    # 1. Tüm yazıları Times New Roman yap (R'da karşılığı "serif"tir)
    text = element_text(family = "serif"),
    
    # 2. Başlık Ayarları
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    
    # 3. X ve Y Eksen BAŞLIKLARI (Büyük ve Kalın)
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    
    # 4. X ve Y Eksen SAYILARI (Okunabilir Boyut)
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    
    # 5. Lejand (Açıklama Kutusu)
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )