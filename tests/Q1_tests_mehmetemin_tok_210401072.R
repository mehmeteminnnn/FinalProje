# Yüklü kütüphaneleri kontrol et
library(testthat)
filename <- "Summer-Olympic-medals-1976-to-2008.csv"
library(readr)
olympic <- read_csv(filename)
# Test senaryolarını içeren bir dosya oluşturun (örneğin, test_veri_keşfi.R)
context("Q3 Adımı için Test Senaryoları: Veri seti keşifsel analizi")

# Test: Veri setinin satır ve sütun sayıları doğru
test_that("Veri setinin satır ve sütun sayıları doğru", {
  # Beklenen değerler
  beklenen_satir_sayisi <- 15433
  beklenen_sutun_sayisi <- 11
  
  # Test
  expect_equal(nrow(olympic), beklenen_satir_sayisi)
  expect_equal(ncol(olympic), beklenen_sutun_sayisi)
})

# Test: Veri setindeki sütun isimleri doğru tanımlandı
test_that("Veri setindeki sütun isimleri doğru tanımlandı", {
  # Beklenen sütun isimleri
  beklenen_sutun_isimleri <- c("City", "Year", "Sport", "Discipline", "Event", "Athlete", "Gender", "Country_Code", "Country", "Event_gender", "Medal")
  
  # Test
  expect_true(all(colnames(olympic) == beklenen_sutun_isimleri))
})
##Q4 Kısmı


test_that("Veri seti ön işleme doğru yapıldı", {
  # Atlet sütunu factor türüne dönüştürüldü mü?
  expect_true(is.factor(olympic$Athlete), info = "Atlet sütunu factor türüne dönüştürüldü mü?")
  
  # Özet rapor doğru mu?
  expected_summary_df <- data.frame(
    unique_values_Athlete = length(unique(olympic$Athlete)),
    unique_values_Event = length(unique(olympic$Event))
  )
  
  expect_equal(summary_df, expected_summary_df, info = "Özet rapor doğru mu?")
})

##Q5 KISMI
library(testthat)

# Test: 'Athlete' sütunu factor türüne dönüştürüldü mü?
test_that("Athlete sütunu factor türüne dönüştürüldü mü?", {
  expect_true(is.factor(olympic$Athlete), info = "Athlete sütunu factor türüne dönüştürüldü mü?")
})

# Test: En başarılı ülkelerin data frame'i oluşturuldu mu?
test_that("En başarılı ülkelerin data frame'i oluşturuldu mu?", {
  en_basarili_ulke <- olympic %>%
    filter(Sport == "Handball" & Gender == "Women") %>%
    group_by(Country) %>%
    summarise(Medal = n()) %>%
    top_n(5, Medal)
  
  expect_equal(nrow(en_basarili_ulke), 5, info = "Doğru sayıda satır oluşturuldu mu?")
  expect_equal(colnames(en_basarili_ulke), c("Country", "Medal"), info = "Doğru sütun isimleri oluşturuldu mu?")
})

# Test: Yüzmede en iyi 10 ülkenin data frame'i oluşturuldu mu?
test_that("Yüzmede en iyi 10 ülkenin data frame'i oluşturuldu mu?", {
  swimming_men <- olympic %>%
    filter(Gender == "Men" & Discipline == "Swimming")
  
  medal_counts <- swimming_men %>%
    group_by(Country) %>%
    summarise(Total_Medals = n())
  
  top_10_countries <- medal_counts %>%
    arrange(desc(Total_Medals)) %>%
    top_n(10, Total_Medals)
  
  expect_equal(nrow(top_10_countries), 10, info = "Doğru sayıda satır oluşturuldu mu?")
  expect_equal(colnames(top_10_countries), c("Country", "Total_Medals"), info = "Doğru sütun isimleri oluşturuldu mu?")
})


