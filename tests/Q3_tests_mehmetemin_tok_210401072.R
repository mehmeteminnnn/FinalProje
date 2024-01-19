# YC<klC< kC<tC<phaneleri kontrol et
library(testthat)
filename <- "Summer-Olympic-medals-1976-to-2008.csv"
library(readr)
olympic <- read_csv(filename)
# Test senaryolarD1nD1 iC'eren bir dosya oluEturun (C6rneDin, test_veri_keEfi.R)
context("Q3 AdD1mD1 iC'in Test SenaryolarD1: Veri seti keEifsel analizi")

# Test: Veri setinin satD1r ve sC<tun sayD1larD1 doDru
test_that("Veri setinin satD1r ve sC<tun sayD1larD1 doDru", {
  # Beklenen deDerler
  beklenen_satir_sayisi <- 15433
  beklenen_sutun_sayisi <- 11
  
  # Test
  expect_equal(nrow(olympic), beklenen_satir_sayisi)
  expect_equal(ncol(olympic), beklenen_sutun_sayisi)
})

# Test: Veri setindeki sC<tun isimleri doDru tanD1mlandD1
test_that("Veri setindeki sC<tun isimleri doDru tanD1mlandD1", {
  # Beklenen sC<tun isimleri
  beklenen_sutun_isimleri <- c("City", "Year", "Sport", "Discipline", "Event", "Athlete", "Gender", "Country_Code", "Country", "Event_gender", "Medal")
  
  # Test
  expect_true(all(colnames(olympic) == beklenen_sutun_isimleri))
})
