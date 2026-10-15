twan=read.xls("C:/Users/riza4/Downloads/twan.xlsx")

  
fishPikenew<- read_csv("C:/Users/riza4/Downloads/Fish.csv")
View(fishPike)
# Filter hanya untuk spesies 'Smelt'
smelt_data11 <- fishPikenew %>%
  filter(Species == "Bream") %>%
  select(-Species)  # drop kolom Species
View(smelt_data22)

# Buat model kosong dan model penuh
modelPikenew <- lm(Weight ~ Length1+ Length2 +Length3+Height+Width, data = smelt_data11)
summary(modelPikenew)
X <- cbind(1,Length1, Length2,Length3,Height,Width,)
Y <- Weight
b <- solve(t(X)%*%X)%*%t(X)%*%Y
b