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
##Q4 KD1smD1


test_that("Veri seti C6n iEleme doDru yapD1ldD1", {
  # Atlet sC<tunu factor tC<rC<ne dC6nC<EtC<rC<ldC< mC<?
  expect_true(is.factor(olympic$Athlete), info = "Atlet sC<tunu factor tC<rC<ne dC6nC<EtC<rC<ldC< mC<?")
  
  # Czet rapor doDru mu?
  expected_summary_df <- data.frame(
    unique_values_Athlete = length(unique(olympic$Athlete)),
    unique_values_Event = length(unique(olympic$Event))
  )
  
  expect_equal(summary_df, expected_summary_df, info = "Czet rapor doDru mu?")
})

