library(readr)
DataNB5I  <- read_csv("~/Dropbox/Forskning/SPSS filer/Personlighetstest/NB5I/Engelsk/IPIPNEWNeutralEng11 2021.txt")
View(DataNb5I)
names(DataNB5I)

library(devtools)
devtools::install_github("psykmba/DoPsychometric", force = T)
library(DoPsychometric)

object <- GetPsychometric(DataNB5I, scaleNames =  c("Achievment","Dutiful", "Organized", "SelfEffective" ),
                          responseScale = list(c(0,4)),
                          itemLength = 6,
                          itemList = list(c(2:10), c(74:88), c(223:236), c(237:249)),
                          reverse = F,
                          missing = -99)
object <- GetPsychometric(DataNB5I, scaleNames =  c("Altruism","Cooperative", "Moral", "Sympathetic","SelfEffective" ),
                          responseScale = list(c(0,4)),
                          itemLength = 4,
                          itemList = list(c(33:37), c(62:69), c(161:166), c(250:255), c(74:88)),
                          reverse = F,
                          missing = -99)

summary.Psychometric(object)
testF <-TestFacets.Psychometric(object, scale = "Conscientousness",
                                subscales = c("Achievment","Dutiful", "SelfEffective", "Organized" ),
                                parcel = F, fixed = T)
testF <-TestFacets.Psychometric(object, scale = "Agreeableness",
                                subscales = c("Altruism","Cooperative", "Moral", "Sympathetic" ),
                                parcel = F, fixed = F, fixedScales = F)
summary.TestFacets(testF, model = 1)
summary.TestFacets(testF, model = 2)
summary.TestFacets(testF, model = 3)
summary.TestFacets(testF, model = 4)
summary.TestFacets(testF, model = 5)
summary.TestFacets(testF, model = 6)
summary.TestFacets(testF, model = 7)
anova.TestFacets(testF, type = "B")
testF <- DoPsychometric::RunCFA(testF, model = 1, what = "SetNegativeVar")
testF <- DoPsychometric::RunCFA(testF, model = 4, what = "SetNegativeVar")

library(haven)
# Below reads a text file with item names on first row
SelfRatingsIPIP300 <- read_sav("~/Dropbox/Forskning/SPSS filer/Personlighetstest/IPIP120_300/IPIP-NEO-300.sav")

SelfRatingsIPIP300 <-  SelfRatingsIPIP300[1:20000,1:300]
View(SelfRatingsIPIP300)
library(mice)

SelfRatingsIPIP300 <- SelfRatingsIPIP300[complete.cases(SelfRatingsIPIP300),]
IPIPReverse <- c("V1",
                 "V31", "V61", "V91", "V121","V151","V181","V211","V241","V271","V6",  "V36",
                 "V66", "V96", "V126","V156","V186","V216","V246","V276","V11", "V41", "V71",
                 "V101","V131","V161","V191","V221","V251","V281","V16", "V46", "V76", "V106",
                 "V136","V166","V196","V226","V256","V286","V21", "V51", "V81", "V111","V141",
                 "V171","V201","V231","V261","V291","V26", "V56", "V86", "V116","V146","V176" ,
                 "V206","V236","V266","V296")






IPIP_300_names = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15",
                   "V16","V17","V18","V19","V20","V21","V22","V23","V24","V25","V26","V27","V28",
                   "V29","V30","V31","V32","V33","V34","V35","V36","V37","V38","V39","V40","V41",
                   "V42","V43","V44","V45","V46","V47","V48","V49","V50","V51","V52","V53","V54",
                   "V55","V56","V57","V58","V59","V60","V61","V62","V63","V64","V65","V66","V67",
                   "V68","V69","V70","V71","V72","V73","V74","V75","V76","V77","V78","V79","V80",
                   "V81","V82","V83","V84","V85","V86","V87","V88","V89","V90","V91","V92","V93",
                   "V94","V95","V96","V97","V98","V99","V100","V101","V102","V103","V104","V105",
                   "V106","V107","V108","V109","V110","V111","V112","V113","V114","V115","V116",
                   "V117","V118","V119","V120","V121","V122","V123","V124","V125","V126","V127",
                   "V128","V129","V130","V131","V132","V133","V134","V135","V136","V137","V138",
                   "V139","V140","V141","V142","V143","V144","V145","V146","V147","V148","V149",
                   "V150","V151","V152","V153","V154","V155","V156","V157","V158","V159","V160",
                   "V161","V162","V163","V164","V165","V166","V167","V168","V169","V170","V171",
                   "V172","V173","V174","V175","V176","V177","V178","V179","V180","V181","V182",
                   "V183","V184","V185","V186","V187","V188","V189","V190","V191","V192","V193",
                   "V194","V195","V196","V197","V198","V199","V200","V201","V202","V203","V204",
                   "V205","V206","V207","V208","V209","V210","V211","V212","V213","V214","V215",
                   "V216","V217","V218","V219","V220","V221","V222","V223","V224","V225","V226",
                   "V227","V228","V229","V230","V231","V232","V233","V234","V235","V236","V237",
                   "V238","V239","V240","V241","V242","V243","V244","V245","V246","V247","V248",
                   "V249","V250","V251","V252","V253","V254","V255","V256","V257","V258","V259",
                   "V260","V261","V262","V263","V264","V265","V266","V267","V268","V269","V270",
                   "V271","V272","V273","V274","V275","V276","V277","V278","V279","V280","V281",
                   "V282","V283","V284","V285","V286","V287","V288","V289","V290","V291","V292",
                   "V293","V294","V295","V296","V297","V298","V299","V300")
IPIP_300_OriginalNames <- c("H1157","H29","X14","H113","H1285","H754","H78","X123","X136","X118","X74","H334",
                            "X92","H30","X150","H991","H58","X204","H882","H365","X145","H438","X201","X169",
                            "E119","H948","H32","X218","X259","H252","H999","H52","X238","X189","H1174","H761",
                            "X83","X81","H294","H244","X15","H524","E136","E124","H149","H905","H54","X104",
                            "X206","H1206","E57","H446","X211","X212","X87","H959","H1094","X184","E115",
                            "H673","H968","X112","X71","E157","H1354","X95","H2016","E105","X91","E11",
                            "H640","H1137","E174","H1108","X127","H592","H79","H2021","E31","H402","MB3",
                            "H67","H1276","X125","H511","H954","H66","X178","X240","H367","X107","H41",
                            "E163","X80","H352","X84","H1106","X140","H416","H309","H646","H773","E133",
                            "H1100","H145","H655","H457","H441","H917","H69","X133","E56","H1322","X247",
                            "H517","H901","H23","X35","H988","E46","X120","H1151","H1119","H114","H2009",
                            "H755","E55","X261","H1327","H1351","E92","H325","E123","H22","H153","H656",
                            "H1324","X30","H808","H2014","H2029","E35","E145","H549","H258","H950","H33",
                            "X126","X227","H853","X138","H596","X114","H43","H1333","X265","H587","X86",
                            "X198","X82","H947","H976","X29","H1136","H1346","H1205","E113","H1063","H453",
                            "E140","X181","H871","X228","H2043","H1171","X79","H42","X20","E121","E122","E141",
                            "H909","X135","H1195","H1395","X231","H660","X45","H774","H822","X205","H154",
                            "X202","X203","H1018","X242","X111","H910","H809","H288","X274","X3","X248","H738",
                            "H1186","H470","H12","X219","X173","H862","H1046","H648","X272","H604","H1438",
                            "X23","X99","X235","X113","H823","X156","H1039","X10","H607","H1084","X197","X226",
                            "H1460","H1103","X110","X251","H894","X176","H1267","H969","H1281","H1093","X130",
                            "E166","H870","E150","X165","X175","H1001","H1032","X191","H650","X144","H427",
                            "E100","X129","X68","X267","H1430","H497","X209","E84","E36","H699","X115","X216",
                            "X250","X239","H746","H621","H44","X180","X157","H736","H846","H2000","H704",
                            "H1382","X70","H1393","E120","H602","X243","H494","E7","H737","H679","E156",
                            "H918","H1182","H1197","H631","H312","X217","X167","E30","X215","X109","H535",
                            "H1180","E64","X137","X254","E78","E66")
#SelfRatingsIPIP300T <- transpose(SelfRatingsIPIP300)
#row.names(SelfRatingsIPIP300T)
#SelfRatingsIPIP300T$V1
names(SelfRatingsIPIP300)

IPIP300FacetNames<- c("Anxiety", "Friendliness","Imagination","Trust","Self-Consciousness",
                      "Anger","Gregariousness",
                      "Artistic Interests","Morality","Orderliness","Depression","Assertiveness",
                      "Emotionality","Altruism","Dutifulness","Self-Consciousness","Activity Level",
                      "Adventurousness","Cooperation","Achievement-Striving","Immoderation",
                      "Excitement-Seeking","Intellect","Modesty","Self-Discipline","Vulnerability",
                      "Cheerfulness","Liberalism","Sympathy","Cautiousness","Anxiety","Friendliness",
                      "Imagination","Trust","Self-Efficacy","Anger","Gregariousness","Artistic Interests",
                      "Morality","Orderliness","Depression","Assertiveness","Emotionality","Altruism",
                      "Dutifulness","Self-Consciousness","Activity Level","Adventurousness","Cooperation",
                      "Achievement-Striving","Immoderation","Excitement-Seeking","Intellect","Modesty",
                      "Self-Discipline","Vulnerability","Cheerfulness","Liberalism","Sympathy","Cautiousness",
                      "Anxiety","Friendliness","Imagination","Trust","Self-Efficacy","Anger",
                      "Gregariousness","Artistic Interests","Morality","Orderliness","Depression",
                      "Assertiveness","Emotionality","Altruism","Dutifulness","Self-Consciousness",
                      "Activity Level","Adventurousness","Cooperation","Achievement-Striving",
                      "Immoderation","Excitement-Seeking","Intellect","Modesty","Self-Discipline",
                      "Vulnerability","Cheerfulness","Liberalism","Sympathy","Cautiousness","Anxiety",
                      "Friendliness","Imagination","Trust","Self-Efficacy","Anger","Gregariousness",
                      "Artistic Interests","Morality","Orderliness","Depression","Assertiveness",
                      "Emotionality","Altruism","Dutifulness","Self-Consciousness","Activity Level",
                      "Adventurousness","Cooperation","Achievement-Striving","Immoderation",
                      "Excitement-Seeking","Intellect","Modesty","Self-Discipline","Vulnerability",
                      "Cheerfulness","Liberalism","Sympathy","Cautiousness","Anxiety","Friendliness",
                      "Imagination","Trust","Self-Efficacy","Anger","Gregariousness","Artistic Interests",
                      "Morality","Orderliness","Depression","Assertiveness","Emotionality","Altruism",
                      "Dutifulness","Self-Consciousness","Activity Level","Adventurousness","Cooperation",
                      "Achievement-Striving","Immoderation","Excitement-Seeking","Intellect","Modesty",
                      "Self-Discipline","Vulnerability","Cheerfulness","Liberalism","Sympathy",
                      "Cautiousness","Anxiety","Friendliness","Imagination","Trust","Self-Efficacy",
                      "Anger","Gregariousness","Artistic Interests","Morality","Orderliness","Depression",
                      "Assertiveness","Emotionality","Altruism","Dutifulness","Self-Consciousness",
                      "Activity Level","Adventurousness","Cooperation","Achievement-Striving","Immoderation",
                      "Excitement-Seeking","Intellect","Modesty","Self-Discipline","Vulnerability",
                      "Cheerfulness","Liberalism","Sympathy","Cautiousness","Anxiety","Friendliness",
                      "Imagination","Trust","Self-Efficacy","Anger","Gregariousness","Artistic Interests",
                      "Morality","Orderliness","Depression","Assertiveness","Emotionality","Altruism",
                      "Dutifulness","Self-Consciousness","Activity Level","Adventurousness","Cooperation",
                      "Achievement-Striving","Immoderation","Excitement-Seeking","Intellect","Modesty",
                      "Self-Discipline","Vulnerability","Cheerfulness","Liberalism","Sympathy",
                      "Cautiousness","Anxiety","Friendliness","Imagination","Trust","Self-Efficacy",
                      "Anger","Gregariousness","Artistic Interests","Morality","Orderliness","Depression",
                      "Assertiveness","Emotionality","Altruism","Dutifulness","Self-Consciousness",
                      "Activity Level","Adventurousness","Cooperation","Achievement-Striving","Immoderation",
                      "Excitement-Seeking","Intellect","Modesty","Self-Discipline","Vulnerability",
                      "Cheerfulness","Liberalism","Sympathy","Cautiousness","Anxiety","Friendliness",
                      "Imagination","Trust","Self-Efficacy","Anger","Gregariousness","Artistic Interests",
                      "Morality","Orderliness","Depression","Assertiveness","Emotionality","Altruism",
                      "Dutifulness","Self-Consciousness","Activity Level","Adventurousness","Cooperation",
                      "Achievement-Striving","Immoderation","Excitement-Seeking","Intellect","Modesty",
                      "Self-Discipline","Vulnerability","Cheerfulness","Liberalism","Sympathy",
                      "Cautiousness","Anxiety","Friendliness","Imagination","Trust","Self-Efficacy",
                      "Anger","Gregariousness","Artistic Interests","Morality","Orderliness","Depression",
                      "Assertiveness","Emotionality","Altruism","Dutifulness","Self-Consciousness",
                      "Activity Level","Adventurousness","Cooperation","Achievement-Striving",
                      "Immoderation","Excitement-Seeking","Intellect","Modesty","Self-Discipline",
                      "Vulnerability","Cheerfulness","Liberalism","Sympathy","Cautiousness")



CreateFacetsFromItems <- function(data, facets)
{
  facetResults <- data.frame(data[1])
  names(facetResults) <- facets[1]
  for (x in 2:length(facets))
  {
    localNames <- colnames(facetResults)
    if (any(localNames == facets[x]))
    {
      facetResults[facets[x]] <- facetResults[facets[x]] + data[x]
    }
    else
    {
      facetResults <- as.data.frame(cbind(facetResults,  data[x]))
      names(facetResults)[ncol(facetResults)] <- facets[x]
    }
  }
  return (facetResults)
}

SelfRatingsIPIP300[IPIPReverse] <- apply(SelfRatingsIPIP300[IPIPReverse], FUN =
                                           function(t){ return (6-t)}, MARGIN = c(1,2))

newdata <- CreateFacetsFromItems(SelfRatingsIPIP300, facets = IPIP300FacetNames)
names(newdata)
names(newdata) <- c("NAnxiety","EFriendliness","OImagination","ATrust","NSelfConsciousness","NAnger",
                                            "EGregariousness","OArtisticInterests","AMorality","COrderliness","NDepression","EAssertiveness",
                                        "OEmotionality","AAltruism","CDutifulness","EActivityLevel","OAdventurousness","ACooperation",
                                       "CAchievementStriving","NImmoderation","EExcitementSeeking","OIntellect","AModesty","CSelfDiscipline",
                                         "NVulnerability","ECheerfulness","OLiberalism","ASympathy","CCautiousness","CSelfEfficacy")


object <- GetPsychometric(newdata, scaleNames=c("E", "A", "C", "N", "O"),
                                                responseScale = list(c(10-50)),
                          reverse = F,
                          itemLength = 1)

View(object$ScaleItemFrames[[1]])
summary.Psychometric(object)


tObject <- TestFacets.Psychometric(object, scale = "GFP", subscales = c("E", "A", "C",  "O"),
                                   parcel = F)
summary.TestFacets(tObject, model = 1, standardized = T)
te <- getGFP.Psychometric(object, fixedScales = F, fixedSubScales = "E")
summary.TestFacets(te, model = 2, standardized = T)
te$ResultList

mygPca <- te$ResultList[[9]]
myPca <- te$ResultList[[10]]
myPca$scores
myOmega <- te$ResultList[[11]]

debugonce(cor2.Psychometric)
cor2.Psychometric(te)
myOmega$scores[,1]

cor(getPredict.TestFacets(te))
summary.TestFacets(te, model = 2)

getCommand.TestFacets(tObject)
anova.TestFacets(tObject, type = "H")
install.packages ( "reshape2")
library(data.table)
library(reshape2)
library(dplyr)
library(psych)
library(haven)


#Loading the data file (is in the folder so you have to rewrite the address, search it with IMPORT dataset)
IPIP_RatingsWide <- read_sav("~/Dropbox/Forskning/SPSS filer/Personlighetstest/Competence/RatingEngIPIP300/RawRatingsRestructIPIP300.sav")
View(IPIP_RatingsWide)
#To estimate to what extent the rators thought an item was competence
# I took the mean of all ratings of all items
# Items are in Rows, see above, the first row is item name, and na.rm instructs the function to
# calculate even if thera are NaN (missing)
IPIP_RatingsWideMean <-  rowMeans(IPIP_RatingsWide[2:ncol(IPIP_RatingsWide)], na.rm = TRUE)







# this is a dplyr funtion 'sort' to sort all kolumn in ascending order
#SelfRatingsIPIP300 <- SelfRatingsIPIP300[1:ncol(SelfRatingsIPIP300)] %>%
#  select(sort(names(.)))

# which give the TRUE indices of a logical object, allowing for array indices.
# which(BFI_RatingsWideMean > 1.6 )
# =  4  6  7  9 13 15 18 21 23 24 29 32 33 38 40 43 44 52 53 56 60
# that is variables with a rating mean above 1.6, suggestsing a competent item
# remember that BFI_RatingsWideMean from above is from competence ratings and SelfRatings comes from self-ratings

#Nedanstående reverserar kolumner som skall reverseras


#Nedanstående väljer ut vilka av skalorna som har en skattning över ett visst värde mellan 1 och 2
setnames(SelfRatingsIPIP300, IPIP_300_names, IPIP_300_OriginalNames)
SelfRatingsIPIP300 <- SelfRatingsIPIP300[1:300]

FacetNameFrame <-as.data.frame(IPIP300FacetNames)
IPIP300OrginalNameFrame <- as.data.frame(IPIP_300_OriginalNames)
FacetNameFrame <- cbind(FacetNameFrame, IPIP300OrginalNameFrame) # creates a frame with two columns, one being the names of the item
FacetNameFrame <- FacetNameFrame[with(FacetNameFrame, order(IPIP_300_OriginalNames)),]

SelfRatingsIPIP300 <- SelfRatingsIPIP300[1:ncol(SelfRatingsIPIP300)] %>%
  select(sort(names(.)))

FacetNameCol <- as.vector(FacetNameFrame[[1]])
FacetNameCol <- as.vector(FacetNameCol)
SelfRatingsIPIP300Above <-  SelfRatingsIPIP300[which(IPIP_RatingsWideMean >= 1.6 )]
SelfRatingsIPIP300Below <-  SelfRatingsIPIP300[which(IPIP_RatingsWideMean < 1.4 )]

ncol(SelfRatingsIPIP300Above)
length(SelfRatingsIPIP300Above)
pca(SelfRatingsIPIP300Above[1:10000,], impute = "mean",  use = "pairwise")
pca(SelfRatingsIPIP300Below[1:10000,], impute = "mean",  use = "pairwise")




newdata <- CreateFacetsFromItems(SelfRatingsIPIP300Above, FacetNameCol[which(IPIP_RatingsWideMean >= 1.6)])
newdataBelow <- CreateFacetsFromItems(SelfRatingsIPIP300Below, FacetNameCol[which(IPIP_RatingsWideMean < 1.4)])
pca(newdata,  use = "pairwise")
pca(newdataBelow,  use = "pairwise")

ncol(SelfRatingsIPIP300Above)
ncol(SelfRatingsIPIP300Below)
ncol(newdata)
ncol(newdataBelow)

getwd()
setwd("/Users/psymba/Dropbox/Mplus/Competence")
write.csv(newdata, file = "NewDataIPIPNaiveAbove.csv")
write.csv(newdataBelow, file = "NewDataIPIPNaiveBelow.csv")


#Now, do they differ in mean ratings
meanAbove <- apply(SelfRatingsIPIP300Above, FUN=mean, MARGIN = 2, na.rm = TRUE)
mean(as.vector(meanAbove))
meanBelow <- apply(SelfRatingsIPIP300Below, FUN=mean, MARGIN = 2, na.rm = TRUE)
mean(as.vector(meanBelow))

meanAbove <- apply(newdata, FUN=mean, MARGIN = 2, na.rm = TRUE)
mean(as.vector(meanAbove)) / length(SelfRatingsIPIP300Above)
meanBelow <- apply(newdataBelow, FUN=mean, MARGIN = 2, na.rm = TRUE)
mean(as.vector(meanBelow)) / length(SelfRatingsIPIP300Below)

write.csv(newdata, file = "CheckNewVar.csv")


#Loading the data file (is in the folder so you have to rewrite the address, search it with IMPORT dataset)
IPIP_RatingsWide <- read_sav("~/Dropbox/Forskning/SPSS filer/Personlighetstest/Competence/RatingEngIPIP300/ExpertRatingsIPIP300.sav")

#To estimate to what extent the rators thought an item was competence
# I took the mean of all ratings of all items
# Items are in Rows, see above, the first row is item name, and na.rm instructs the function to
# calculate even if thera are NaN (missing)
IPIP_RatingsWideMean <-  rowMeans(IPIP_RatingsWide[1:4], na.rm = TRUE)





# Below reads a text file with item names on first row
SelfRatingsIPIP300 <- read_sav("~/Dropbox/Forskning/SPSS filer/Personlighetstest/IPIP-NEO-300.sav")







#Nedanstående reverserar kolumner som skall reverseras
SelfRatingsIPIP300[IPIPReverse] <- apply(SelfRatingsIPIP300[IPIPReverse], FUN =
                                           function(t){ return (6-t)}, MARGIN = c(1,2))


#Nedanstående väljer ut vilka av skalorna som har en skattning över ett visst värde mellan 1 och 2
setnames(SelfRatingsIPIP300, IPIP_300_names, IPIP_300_OriginalNames)
SelfRatingsIPIP300 <- SelfRatingsIPIP300[1:300]

FacetNameFrame <-as.data.frame(IPIP300FacetNames)
IPIP300OrginalNameFrame <- as.data.frame(IPIP_300_OriginalNames)
FacetNameFrame <- cbind(FacetNameFrame, IPIP300OrginalNameFrame) # skapar en frame med två kolumner,en är namnen på item
FacetNameFrame <- dfOrder(FacetNameFrame,2) #en är namnen på fasetterna. Här sorteras denna frame enligt item
SelfRatingsIPIP300 <- SelfRatingsIPIP300[1:ncol(SelfRatingsIPIP300)] %>%
  select(sort(names(.)))

FacetNameCol <- as.vector(FacetNameFrame[[1]])
FacetNameCol <- as.vector(FacetNameCol)
SelfRatingsIPIP300Above <-  SelfRatingsIPIP300[which(IPIP_RatingsWideMean >= 1.6 )]
SelfRatingsIPIP300Below <-  SelfRatingsIPIP300[which(IPIP_RatingsWideMean < 1.4 )]





newdata <- CreateFacetsFromItems(SelfRatingsIPIP300Above, FacetNameCol[which(IPIP_RatingsWideMean >= 1.6)])
newdataBelow <- CreateFacetsFromItems(SelfRatingsIPIP300Below, FacetNameCol[which(IPIP_RatingsWideMean < 1.4)])

ncol(SelfRatingsIPIP300Above)
ncol(SelfRatingsIPIP300Below)
ncol(newdata)
ncol(newdataBelow)
pca(newdata,  use = "pairwise")
pca(newdataBelow,  use = "pairwise")
getwd()
setwd("/Users/martinbackstrom/dropbox/mplus/competence/")
write.csv(newdata[sample(nrow(newdataBelow), 10000),], file = "NewDataIPIPExpertAbove.csv")
write.csv(newdataBelow[sample(nrow(newdataBelow), 10000),], file = "NewDataIPIPExpoertBelow.csv")
ncol(newdata)
ncol(newdataBelow)
ncol(SelfRatingsIPIP300Above)
ncol(SelfRatingsIPIP300Below)

#Now, do they differ in mean ratings
meanAbove <- apply(SelfRatingsIPIP300Above, FUN=mean, MARGIN = 2, na.rm = TRUE)
mean(as.vector(meanAbove))
meanBelow <- apply(SelfRatingsIPIP300Below, FUN=mean, MARGIN = 2, na.rm = TRUE)
mean(as.vector(meanBelow))


meanAbove <- apply(newdata, FUN=mean, MARGIN = 2, na.rm = TRUE)
mean(as.vector(meanAbove)) / length(SelfRatingsIPIP300Above)
meanBelow <- apply(newdataBelow, FUN=mean, MARGIN = 2, na.rm = TRUE)
mean(as.vector(meanBelow)) / length(SelfRatingsIPIP300Below)


length(SelfRatingsIPIP300Above)
pca(SelfRatingsIPIP300Above[1:10000,], impute = "mean",  use = "pairwise")
pca(SelfRatingsIPIP300Below[1:10000,], impute = "mean",  use = "pairwise")

names(SelfRatingsIPIP300Above)



SocRatingIPIPNamesAbove <- select(SocRatingIPIPNames, names(SelfRatingsIPIP300Above))

getIPIP <- function()
{
  start <-c(1,31,61,91,121,151,181,211,241,271)
  res <- list()
  for (index in 0:29)
  {
    res <- append(res,list(start + index))
  }
  return (res)
}

ipipNum <- getIPIP()

object <- GetPsychometric(SelfRatingsIPIP300,
         scaleNames = c("NAnxiety","EFriendliness","OImagination","ATrust","NSelfConsciousness","NAnger",
                          "EGregariousness","OArtisticInterests","AMorality","COrderliness","NDepression","EAssertiveness",
                          "OEmotionality","AAltruism","CDutifulness","EActivityLevel","OAdventurousness","ACooperation",
                          "CAchievementStriving","NImmoderation","EExcitementSeeking","OIntellect","AModesty","CSelfDiscipline",
                          "NVulnerability","ECheerfulness","OLiberalism","ASympathy","CCautiousness","CSelfEfficacy"),
         itemList = getIPIP(),
       itemLength = 4,
         responseScale = list(c(1,5)))

debugonce(GetPsychometric)
gfp <- getGFP.Psychometric(object, scales = c("NAnxiety","NAnger","NDepression","NImmoderation","NVulnerability","NSelfConsciousness"),
                           parceling = T)

getGFP.Psychometric
