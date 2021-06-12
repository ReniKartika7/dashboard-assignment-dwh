library(lubridate)

#Time Dimension
timeDimension <- 
  TrPurchaseHeader %>%
  select(PurchaseDate) %>%
  distinct(PurchaseDate)%>%
  mutate(PurchaseDate = as.Date(PurchaseDate, format = "%d/%m/%Y"),
         Date = PurchaseDate,
         Day = day(PurchaseDate),
         Month = month(PurchaseDate),
         Quarter = quarter(PurchaseDate),
         Year = year(PurchaseDate),
         Weekdays = weekdays(PurchaseDate, abbreviate = TRUE),
         IsWeekend = ifelse(Weekdays %in% c("Sat", "Sun"), TRUE, FALSE))%>%
  arrange(PurchaseDate)%>%
  select(-c(PurchaseDate, Weekdays))%>%
  mutate(TimeID = row_number())%>%
  select(TimeID, Date, Day:IsWeekend)

write_csv(timeDimension, "Time Dimension.csv")

#User Dimension
user <- read_csv("MsUser.csv")
userDimension <- user %>%
  rename(UserNo = RowNumber)%>%
  select(UserID, UserNo, UserName:UserDOB)

write_csv(userDimension, "User Dimension.csv")

#Shop Dimension
shop <- read_csv("MsShop.csv")
shopDimension <- shop %>%
  rename(ShopNo = RowNumber)%>%
  select(ShopID, ShopNo, ShopName:ShopRating)

write_csv(shopDimension, "Shop Dimension.csv")


#Product Dimension
product <- read_csv("MsProduct.csv")
productDimension <- product %>%
  rename(ProductNo = RowNumber)%>%
  select(ProductID, ProductNo, ProductName:ProductCategoryName)

write_csv(productDimension, "Product Dimension.csv")

#Fact Table
productSale <- 
TrPurchaseHeader %>%
  left_join(TrPurchaseDetail, by=("PurchaseID"))%>%
  left_join(timeDimension, by=c("PurchaseDate" = "Date"))%>%
  select(TimeID, ShopID, UserID, ProductID, Quantity)%>%
  left_join(productDimension, by=("ProductID"))%>%
  mutate(TotalIncome = Quantity * ProductPrice,
         TotalProductOrdered = Quantity)%>%
  select(TimeID:ProductID, TotalIncome, TotalProductOrdered)

write_csv(productSale, "Product Sale Fact.csv")
