# An example of building an HPD from scratch

### Step 0. Get to know your data.
data(HairEyeColor) # see ?HairEyeColor for background
df <- data.frame(HairEyeColor) # str(df) is useful



#df is look like this
#       Hair   Eye    Sex Freq
#   1  Black Brown   Male   32
#   2  Brown Brown   Male   53
#   3    Red Brown   Male   10
#   4  Blond Brown   Male    3
#   5  Black  Blue   Male   11
#   6  Brown  Blue   Male   50
#   7    Red  Blue   Male   10
#   8  Blond  Blue   Male   30
#   9  Black Hazel   Male   10
#   10 Brown Hazel   Male   25
#   11   Red Hazel   Male    7
#   12 Blond Hazel   Male    5
#   13 Black Green   Male    3
#   14 Brown Green   Male   15
#   15   Red Green   Male    7
#   16 Blond Green   Male    8
#   17 Black Brown Female   36
#   18 Brown Brown Female   66
#   19   Red Brown Female   16
#   20 Blond Brown Female    4
#   21 Black  Blue Female    9
#   22 Brown  Blue Female   34
#   23   Red  Blue Female    7
#   24 Blond  Blue Female   64
#   25 Black Hazel Female    5
#   26 Brown Hazel Female   29
#   27   Red Hazel Female    7
#   28 Blond Hazel Female    5
#   29 Black Green Female    2
#   30 Brown Green Female   14
#   31   Red Green Female    7
#   32 Blond Green Female    8


# Frequencies of the colors can be found with:
#aggregate(汇总数据~分类项,data=df ,FUN="sum")
eyeF <- aggregate(Freq ~ Eye, data = df, FUN = 'sum') 
hairF <- aggregate(Freq ~ Hair, data = df, FUN = 'sum')

#eyeF looks like this
#	    Eye Freq
#	1 Brown  220
#	2  Blue  215
#	3 Hazel   93
#	4 Green   64

#hairF looks like this
#	   Hair Freq
#	1 Black  108
#	2 Brown  286
#	3   Red   71
#	4 Blond  127

#根据node出现的频率来分配Node的大小
es <- eyeF$Freq/eyeF$Freq[4] # node sizes for eye
hs <- hairF$Freq/hairF$Freq[3] # node sizes for hair


### Step 1. Assemble a data frame of the nodes.
# There are 32 rows in the data frame, but we are going to
# separate the hair color from the eye color and thus
# double the number of rows in the node data frame

nodes <- data.frame(
id = 1:64,
lab = paste(rep(c("hair", "eye"), each = 32), 1:64, sep = "_"),
axis = rep(1:2, each = 32),
radius = rep(NA, 64))

#nodes looks like this 
#	   id     lab axis radius
#	1   1  hair_1    1     NA
#	2   2  hair_2    1     NA
#	3   3  hair_3    1     NA
#	4   4  hair_4    1     NA
#	5   5  hair_5    1     NA
#	6   6  hair_6    1     NA
#	7   7  hair_7    1     NA
#	8   8  hair_8    1     NA
#	9   9  hair_9    1     NA
#	10 10 hair_10    1     NA
#	11 11 hair_11    1     NA
#	12 12 hair_12    1     NA
#	13 13 hair_13    1     NA
#	14 14 hair_14    1     NA
#	15 15 hair_15    1     NA
#	16 16 hair_16    1     NA
#	17 17 hair_17    1     NA
#	18 18 hair_18    1     NA
#	19 19 hair_19    1     NA
#	20 20 hair_20    1     NA
#	21 21 hair_21    1     NA
#	22 22 hair_22    1     NA
#	23 23 hair_23    1     NA
#	24 24 hair_24    1     NA
#	25 25 hair_25    1     NA
#	26 26 hair_26    1     NA
#	27 27 hair_27    1     NA
#	28 28 hair_28    1     NA
#	29 29 hair_29    1     NA
#	30 30 hair_30    1     NA
#	31 31 hair_31    1     NA
#	32 32 hair_32    1     NA
#	33 33  eye_33    2     NA
#	34 34  eye_34    2     NA
#	35 35  eye_35    2     NA
#	36 36  eye_36    2     NA
#	37 37  eye_37    2     NA
#	38 38  eye_38    2     NA
#	39 39  eye_39    2     NA
#	40 40  eye_40    2     NA
#	41 41  eye_41    2     NA
#	42 42  eye_42    2     NA
#	43 43  eye_43    2     NA
#	44 44  eye_44    2     NA
#	45 45  eye_45    2     NA
#	46 46  eye_46    2     NA
#	47 47  eye_47    2     NA
#	48 48  eye_48    2     NA
#	49 49  eye_49    2     NA
#	50 50  eye_50    2     NA
#	51 51  eye_51    2     NA
#	52 52  eye_52    2     NA
#	53 53  eye_53    2     NA
#	54 54  eye_54    2     NA
#	55 55  eye_55    2     NA
#	56 56  eye_56    2     NA
#	57 57  eye_57    2     NA
#	58 58  eye_58    2     NA
#	59 59  eye_59    2     NA
#	60 60  eye_60    2     NA
#	61 61  eye_61    2     NA
#	62 62  eye_62    2     NA
#	63 63  eye_63    2     NA
#	64 64  eye_64    2     NA


for (n in 1:32) {
# assign node radius based most common colors
#只是表示节点在轴上的位置排序，半径即离 轴初始点 的位置。
if (df$Hair[n] == "Black") nodes$radius[n] <- 2
if (df$Hair[n] == "Brown") nodes$radius[n] <- 4
if (df$Hair[n] == "Red") nodes$radius[n] <- 1
if (df$Hair[n] == "Blond") nodes$radius[n] <- 3

if (df$Eye[n] == "Brown") nodes$radius[n + 32] <- 4
if (df$Eye[n] == "Blue") nodes$radius[n + 32] <- 3
if (df$Eye[n] == "Hazel") nodes$radius[n + 32] <- 2
if (df$Eye[n] == "Green") nodes$radius[n + 32] <- 1
# now do node sizes
if (df$Hair[n] == "Black") nodes$size[n] <- hs[1]
if (df$Hair[n] == "Brown") nodes$size[n] <- hs[2]
if (df$Hair[n] == "Red") nodes$size[n] <- hs[3]
if (df$Hair[n] == "Blond") nodes$size[n] <- hs[4]

if (df$Eye[n] == "Brown") nodes$size[n + 32] <- es[1]
if (df$Eye[n] == "Blue") nodes$size[n + 32] <- es[2]
if (df$Eye[n] == "Hazel") nodes$size[n + 32] <- es[3]
if (df$Eye[n] == "Green") nodes$size[n + 32] <- es[4]
#设置一下颜色 
if (df$Hair[n] == "Black") nodes$color[n] <- 'Black'
if (df$Hair[n] == "Brown") nodes$color[n] <- 'Brown'
if (df$Hair[n] == "Red") nodes$color[n] <- 'Red'
if (df$Hair[n] == "Blond") nodes$color[n] <- 'Yellow'

if (df$Eye[n] == "Brown") nodes$color[n + 32] <- 'Brown'
if (df$Eye[n] == "Blue") nodes$color[n + 32] <- 'Blue'
if (df$Eye[n] == "Hazel") nodes$color[n + 32] <- 'lightsalmon3'
if (df$Eye[n] == "Green") nodes$color[n + 32] <- 'Green'
}


#nodes$color <- rep("Brown", 64)
nodes$lab <- as.character(nodes$lab) # clean up some data types is very important
nodes$radius <- as.numeric(nodes$radius)

### Step 2. Assemble a data frame of the edges.

edges <- data.frame( # There will be 32 edges, corresponding to the original 32 rows
id1 = c(1:16,49:64), # This will set up edges between each eye/hair pair
id2 = c(33:48,17:32), # & put the males above and the females below

#id1 = c(1:16,17:32), # This will set up edges between each eye/hair pair
#id2 = c(33:48,49:64), # & put the males above and the females below
weight = df$Freq,
weight = df$Freq,
color = rep(c("lightblue", "pink"), each = 16))
edges$color <- as.character(edges$color)
# Scale the edge weight (det'd by trial & error to emphasize differences)
edges$weight <- 0.25*log(edges$weight)^2.25

#edges looks like this 之所以这样分配id1 id2是因为为了按照顺序，先从轴1 画蓝色的线到轴2，再从轴2（eye）反方向画粉色的线到轴1（hair）使得粉线在蓝色下面
#如果也是从hair开始画女性的曲线，图线也会在上面，从而把蓝色线条覆盖住。
#	   id1 id2    weight weight.1     color
#	1    1  33 4.0971306       32 lightblue
#	2    2  34 5.5627622       53 lightblue
#	3    3  35 1.6327711       10 lightblue
#	4    4  36 0.3089157        3 lightblue
#	5    5  37 1.7887843       11 lightblue
#	6    6  38 5.3807543       50 lightblue
#	7    7  39 1.6327711       10 lightblue
#	8    8  40 3.9274588       30 lightblue
#	9    9  41 1.6327711       10 lightblue
#	10  10  42 3.4695609       25 lightblue
#	11  11  43 1.1180630        7 lightblue
#	12  12  44 0.7293853        5 lightblue
#	13  13  45 0.3089157        3 lightblue
#	14  14  46 2.3518933       15 lightblue
#	15  15  47 1.1180630        7 lightblue
#	16  16  48 1.2981358        8 lightblue
#	17  49  17 4.4170960       36      pink
#	18  50  18 6.2782852       66      pink
#	19  51  19 2.4798892       16      pink
#	20  52  20 0.5213325        4      pink
#	21  53  21 1.4694591        9      pink
#	22  54  22 4.2601521       34      pink
#	23  55  23 1.1180630        7      pink
#	24  56  24 6.1750092       64      pink
#	25  57  25 0.7293853        5      pink
#	26  58  26 3.8399261       29      pink
#	27  59  27 1.1180630        7      pink
#	28  60  28 0.7293853        5      pink
#	29  61  29 0.1095967        2      pink
#	30  62  30 2.2192175       14      pink
#	31  63  31 1.1180630        7      pink
#	32  64  32 1.2981358        8      pink
### Step 3. Now assemble the HivePlotData (HPD) object.
HEC <- list()
HEC$nodes <- nodes

HEC$edges <- edges
HEC$type <- "2D"
HEC$desc <- "HairEyeColor data set"
HEC$axis.cols <- c("grey", "grey")
class(HEC) <- "HivePlotData"
### Step 4. Check it & summarize
chkHPD(HEC) # answer of FALSE means there are no problems
sumHPD(HEC)
### Step 5. Plot it.
# A minimal plot
plotHive(HEC, ch = 0.1, bkgnd = "white")
# See ?plotHive for fancier options