# choiceModelR'ı çalıştırma örneği

# xcoding vektörü: Hangi değişkenin kategorik olduğunu belirtir.
# Price(Cat), Density(Cat), Type(Cat), Integ(Cat), Time(Cat) Hepsi katogorik.
xcoding <- c(1, 1, 1, 1, 1) 

# MCMC (Markov Chain Monte Carlo) ayarları
mcmc_ayarlari <- list(R = 2000, use = 1000) # Hızlı sonuç için düşük tuttum

# Modeli Çalıştır
# Not: Bu işlem bilgisayar hızına göre +/- birkaç saniye sürebilir.
model_output <- choicemodelr(data = final_data, xcoding = xcoding, 
                    mcmc = mcmc_ayarlari, 
                    directory ="./outputs") # Sonuçları çalışma dizinine kaydeder

# Sonuçları inceleme (Özet)
# out$betadraw, her birey için tahmin edilen katsayıları (part-worths) içerir.
print("Model tamamlandı. Katsayı ortalamaları:")

