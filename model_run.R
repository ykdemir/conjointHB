library(ChoiceModelR)
# choiceModelR'ı çalıştırma örneği

# xcoding vektörü: Hangi değişkenin kategorik olduğunu belirtir.
# Price(Cat), Density(Cat), Type(Cat), Integ(Cat), Time(Cat) Hepsi katogorik.
xcoding <- c(1, 1, 1, 1, 1) 

# MCMC (Markov Chain Monte Carlo) ayarları
mcmc_ayarlari <- list(R = 20000, use = 2000) # Hızlı sonuç için düşük tutulur, ancak bu yapılırsa her çalıştırmada sonuçlar biraz sapma gösteriri

# Modeli Çalıştır
# Not: Bu işlem bilgisayar hızına göre +/- birkaç saniye sürebilir.
model_output <- choicemodelr(data = final_data, xcoding = xcoding, 
                    mcmc = mcmc_ayarlari, 
                    directory ="./outputs") # Sonuçları çalışma dizinine kaydeder


