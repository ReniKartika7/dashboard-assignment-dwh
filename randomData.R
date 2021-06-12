setwd("D:/Data Reni/Kuliah/Semester 8/Data Warehouse/Kelas Biasa/Assignment/Dashboard")
library(tidyverse)

x = 1:999

PurchaseID = str_c("PU", sprintf('%05d', seq(1, 10000, 1))) 
ShopID = str_c("SH", sprintf('%05d', sample(x, size = 10000, replace = TRUE)))
UserID = str_c("SU", sprintf('%05d', sample(x, size = 10000, replace = TRUE)))
PurchaseDate = sample(seq(as.Date('2019/01/01'), as.Date('2021/06/01'), by="day"), 10000, replace = TRUE)

TrPurchaseHeader = tibble(PurchaseID = PurchaseID,
                          ShopID = ShopID,
                          UserID = UserID,
                          PurchaseDate = format(PurchaseDate, "%d/%m/%Y"))

TrPurchaseHeader <- TrPurchaseHeader %>%
  mutate(PurchaseDate = as.Date(PurchaseDate, format = "%d/%m/%Y"))

write_csv(TrPurchaseHeader, 'TrPurchaseHeader.csv')


PurchaseID = c(str_c("PU", sprintf('%05d', seq(1, 10000, 1))), str_c("PU", sprintf('%05d', sample(x, size = 15500, replace = TRUE))))
ProductID = str_c("PR", sprintf('%05d', sample(x, size = 25500, replace = TRUE)))
Quantity = sample(1:50, size = 25500, replace = TRUE)

TrPurchaseDetail = tibble(PurchaseID = PurchaseID,
                          ProductID = ProductID,
                          Quantity = Quantity)

TrPurchaseDetail <- TrPurchaseDetail %>%
  arrange(PurchaseID, ProductID)%>%
  group_by(PurchaseID, ProductID)%>%
  slice(1)

write_csv(TrPurchaseDetail, 'TrPurchaseDetail.csv')

product <- read_csv("Product Dimension.csv")

#Perabotan Rumah Tangga|Makanan dan Minuman|Elektronik|Pakaian Wanita|Pakaian Pria|Aksesoris|Alat Tulis)
Mainan <- c("Ular Tangga", "Kelereng", "Yoyo", "Congklak", "Gasing", 
            "Boneka", "Mobil mobilan", "Lego", "Halma", "Gembot", "Tamagotchi")
Perabotan <- c("Lemari", "Meja", "Kursi", "Tempat tidur", "Sofa",
               "Meja rias", "Kursi malas")
Makanan <- c("Tteokbokki", "Indomie", "Kopi", "Susu", "Ramyeon", "Rumput Lau",
             "Pasta", "Latte", "Kwetiau Goreng", "Aqua", "Cookie", "Kue Kering")
Elektronik <-c("Samsung", "iPhone", "Xiaomi", "Laptop", "Komputer", "TV",
               "Kulkas", "AC", "Oppo", "Macbook", "iPad", "Smart Watch", "Earphone")
pW <- c("Dress", "Kardigan", "T-shirt", "Kaos", "Jaket", "Hoodie", "Crop Top",
        "Piyama")
pP <- c("Kemeja", "Jas", "T-shirt", "Kaos", "Jaket", "Hoodie", "Piyama")
Aksesoris <- c("Kalung", "Anting", "Gelang", "Cincin", "Scrunchie", "Bando")
AlatTulis <- c("Pulpen", "Pensil", "Penghapus", "Tipe-X", "Lem", "Pensil warna", "Krayon",
               "Cutter", "Penggaris", "Stapler")

product <- product%>%
  mutate(ProductName = case_when(
    ProductCategoryName == "Mainan" ~ sample(Mainan, size = 1000, replace = TRUE),
    ProductCategoryName == "Perabotan Rumah Tangga" ~ sample(Perabotan, size = 1000, replace = TRUE),
    ProductCategoryName == "Makanan dan Minuman" ~ sample(Makanan, size = 1000, replace = TRUE),
    ProductCategoryName == "Elektronik" ~ sample(Elektronik, size = 1000, replace = TRUE),
    ProductCategoryName == "Pakaian Wanita" ~ sample(pW, size = 1000, replace = TRUE),
    ProductCategoryName == "Pakaian Pria" ~ sample(pP, size = 1000, replace = TRUE),
    ProductCategoryName == "Aksesoris" ~ sample(Aksesoris, size = 1000, replace = TRUE),
    ProductCategoryName == "Alat Tulis" ~ sample(AlatTulis, size = 1000, replace = TRUE)
  ))

write_csv(product, "Product Dimension.csv")
