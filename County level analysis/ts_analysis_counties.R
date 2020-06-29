
# filter to MN counties
# Anoka
MN_Anoka <- kaggle %>% filter(Province_State == "Minnesota") %>% filter(County == "Anoka")
tsAnokaMN <- ts(MN_Anoka[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsAnokaMN, main = "COVID-19 Cases and Deaths - Anoka, MN")
# Clay
MN_Clay <- kaggle %>% filter(Province_State == "Minnesota") %>% filter(County == "Clay")
tsClayMN <- ts(MN_Clay[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsClayMN, main = "COVID-19 Cases and Deaths - Clay, MN")
# Dakota
MN_Dakota <- kaggle %>% filter(Province_State == "Minnesota") %>% filter(County == "Dakota")
tsDakotaMN <- ts(MN_Dakota[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsDakotaMN, main = "COVID-19 Cases and Deaths - Dakota, MN")
# Hennepin
MN_Hennepin <- kaggle %>% filter(Province_State == "Minnesota") %>% filter(County == "Hennepin")
tsHennepinMN <- ts(MN_Hennepin[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsHennepinMN, main = "COVID-19 Cases and Deaths - Hennepin, MN")
# Kandiyohi
MN_Kandiyohi <- kaggle %>% filter(Province_State == "Minnesota") %>% filter(County == "Kandiyohi")
tsKandiyohiMN <- ts(MN_Kandiyohi[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsKandiyohiMN, main = "COVID-19 Cases and Deaths - Kandiyohi, MN")
# Nobles
MN_Nobles <- kaggle %>% filter(Province_State == "Minnesota") %>% filter(County == "Nobles")
tsNoblesMN <- ts(MN_Nobles[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsNoblesMN, main = "COVID-19 Cases and Deaths - Nobles, MN")
# Ramsey
MN_Ramsey <- kaggle %>% filter(Province_State == "Minnesota") %>% filter(County == "Ramsey")
tsRamseyMN <- ts(MN_Ramsey[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsRamseyMN, main = "COVID-19 Cases and Deaths - Ramsey, MN")
# Stearns
MN_Stearns <- kaggle %>% filter(Province_State == "Minnesota") %>% filter(County == "Stearns")
tsStearnsMN <- ts(MN_Stearns[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsStearnsMN, main = "COVID-19 Cases and Deaths - Stearns, MN")
# Washington
MN_Washington <- kaggle %>% filter(Province_State == "Minnesota") %>% filter(County == "Washington")
tsWashingtonMN <- ts(MN_Washington[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsWashingtonMN, main = "COVID-19 Cases and Deaths - Washington, MN")

# filter to CO counties
# Adams
CO_Adams <- kaggle %>% filter(Province_State == "Colorado") %>% filter(County == "Adams")
tsAdamsCO <- ts(CO_Adams[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsAdamsCO, main = "COVID-19 Cases and Deaths - Adams, CO")
# Arapahoe
CO_Arapahoe <- kaggle %>% filter(Province_State == "Colorado") %>% filter(County == "Arapahoe")
tsArapahoeCO <- ts(CO_Arapahoe[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsArapahoeCO, main = "COVID-19 Cases and Deaths - Arapahoe, CO")
# Boulder
CO_Boulder <- kaggle %>% filter(Province_State == "Colorado") %>% filter(County == "Boulder")
tsBoulderCO <- ts(CO_Boulder[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsBoulderCO, main = "COVID-19 Cases and Deaths - Boulder, CO")
# Denver
CO_Denver <- kaggle %>% filter(Province_State == "Colorado") %>% filter(County == "Denver")
tsDenverCO <- ts(CO_Denver[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsDenverCO, main = "COVID-19 Cases and Deaths - Denver, CO")
# Douglas
CO_Douglas <- kaggle %>% filter(Province_State == "Colorado") %>% filter(County == "Douglas")
tsDouglasCO <- ts(CO_Douglas[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsDouglasCO, main = "COVID-19 Cases and Deaths - Douglas, CO")
# El Paso
CO_ElPaso <- kaggle %>% filter(Province_State == "Colorado") %>% filter(County == "El Paso")
tsElPasoCO <- ts(CO_ElPaso[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsElPasoCO, main = "COVID-19 Cases and Deaths - El Paso, CO")
# Jefferson
CO_Jefferson <- kaggle %>% filter(Province_State == "Colorado") %>% filter(County == "Jefferson")
tsJeffersonCO <- ts(CO_Jefferson[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsJeffersonCO, main = "COVID-19 Cases and Deaths - Jefferson, CO")
# Weld
CO_Weld <- kaggle %>% filter(Province_State == "Colorado") %>% filter(County == "Weld")
tsWeldCO <- ts(CO_Weld[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsWeldCO, main = "COVID-19 Cases and Deaths - Weld, CO")

# filter to MI counties
# Monroe
MI_Monroe <- kaggle %>% filter(Province_State == "Michigan") %>% filter(County == "Monroe")
tsMonroeMI <- ts(MI_Monroe[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsMonroeMI, main = "COVID-19 Cases and Deaths - Monroe, MI")
# Macomb
MI_Macomb <- kaggle %>% filter(Province_State == "Michigan") %>% filter(County == "Macomb")
tsMacombMI <- ts(MI_Macomb[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsMacombMI, main = "COVID-19 Cases and Deaths - Macomb, MI")
# Oakland
MI_Oakland <- kaggle %>% filter(Province_State == "Michigan") %>% filter(County == "Oakland")
tsOaklandMI <- ts(MI_Oakland[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsOaklandMI, main = "COVID-19 Cases and Deaths - Oakland, MI")
# Wayne
MI_Wayne <- kaggle %>% filter(Province_State == "Michigan") %>% filter(County == "Wayne")
tsWayneMI <- ts(MI_Wayne[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsWayneMI, main = "COVID-19 Cases and Deaths - Wayne, MI")

# filter to NE counties
# Dakota
NE_Dakota <- kaggle %>% filter(Province_State == "Nebraska") %>% filter(County == "Dakota")
tsDakotaNE <- ts(NE_Dakota[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsDakotaNE, main = "COVID-19 Cases and Deaths - Dakota, NE")
# Douglas
NE_Douglas <- kaggle %>% filter(Province_State == "Nebraska") %>% filter(County == "Douglas")
tsDouglasNE <- ts(NE_Douglas[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsDouglasNE, main = "COVID-19 Cases and Deaths - Douglas, NE")
# Hall
NE_Hall <- kaggle %>% filter(Province_State == "Nebraska") %>% filter(County == "Hall")
tsHallNE <- ts(NE_Hall[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsHallNE, main = "COVID-19 Cases and Deaths - Hall, NE")
# Scotts Bluff
NE_Scotts <- kaggle %>% filter(Province_State == "Nebraska") %>% filter(County == "Scotts Bluff")
tsScottsNE <- ts(NE_Scotts[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsScottsNE, main = "COVID-19 Cases and Deaths - Scotts Bluff, NE")

# filter to PA counties
# Bucks
PA_Bucks <- kaggle %>% filter(Province_State == "Pennsylvania") %>% filter(County == "Bucks")
tsBucksPA <- ts(PA_Bucks[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsBucksPA, main = "COVID-19 Cases and Deaths - Bucks, PA")
# Montgomery
PA_Montgomery <- kaggle %>% filter(Province_State == "Pennsylvania") %>% filter(County == "Montgomery")
tsMontgomeryPA <- ts(PA_Montgomery[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsMontgomeryPA, main = "COVID-19 Cases and Deaths - Montgomery, PA")
# Philadelphia
PA_Philadelphia <- kaggle %>% filter(Province_State == "Pennsylvania") %>% filter(County == "Philadelphia")
tsPhiladelphiaPA <- ts(PA_Philadelphia[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsPhiladelphiaPA, main = "COVID-19 Cases and Deaths - Philadelphia, PA")

# filter to SD counties
# Brown
SD_Brown <- kaggle %>% filter(Province_State == "South Dakota") %>% filter(County == "Brown")
tsBrownSD <- ts(SD_Brown[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsBrownSD, main = "COVID-19 Cases and Deaths - Brown, SD")
# Minnehaha
SD_Minnehaha <- kaggle %>% filter(Province_State == "South Dakota") %>% filter(County == "Minnehaha")
tsMinnehahaSD <- ts(SD_Minnehaha[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsMinnehahaSD, main = "COVID-19 Cases and Deaths - Minnehaha, SD")

# filter to TX counties
# Dallas
TX_Dallas <- kaggle %>% filter(Province_State == "Texas") %>% filter(County == "Dallas")
tsDallasTX <- ts(TX_Dallas[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsDallasTX, main = "COVID-19 Cases and Deaths - Dallas, TX")
# Ellis
TX_Ellis <- kaggle %>% filter(Province_State == "Texas") %>% filter(County == "Ellis")
tsEllisTX <- ts(TX_Ellis[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsEllisTX, main = "COVID-19 Cases and Deaths - Ellis, TX")
# Harris
TX_Harris <- kaggle %>% filter(Province_State == "Texas") %>% filter(County == "Harris")
tsHarrisTX <- ts(TX_Harris[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsHarrisTX, main = "COVID-19 Cases and Deaths - Harris, TX")
# Tarrant
TX_Tarrant <- kaggle %>% filter(Province_State == "Texas") %>% filter(County == "Tarrant")
tsTarrantTX <- ts(TX_Tarrant[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsTarrantTX, main = "COVID-19 Cases and Deaths - Tarrant, TX")
