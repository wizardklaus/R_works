
# ���� ��� ID Ȯ��(getBusRouteList)

library("XML")

API_key <- "bmbBRsCJOA1EpIK8jgnm6LPMzcKqAyQyewr%2F7U9dTMepFlMRE%2FQttSNB7gD2p7r1sxuiCuybpJUfDFq1pQlnUA%3D%3D"

url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getBusRouteList?ServiceKey=",API_key,sep="")
# sep=""�� ���ڿ� ���̸� ������� �ٿ� �����϶�� �ǹ�.

xmlfile <- xmlParse(url)
# xml������ R���� �ν��� �� �ְ� ���� ����
xmltop <- xmlRoot(xmlfile)
# XML ���� ��ü�� ��Ʈ ��忡 ����, �ʿ��� �������� �������� ����.
xmltop

# �뼱�� ������ ��θ� ���� (getRoutePathList)

busRouteId <- "204000041"
# 9004���� ������ ID��ȣ

Route_url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getRoutePath?ServiceKey=",API_key, "&busRouteId=", busRouteId, sep="")

xmlfile2 <- xmlParse(Route_url)
xmltop2 <- xmlRoot(xmlfile2)
xmltop2



# ���� �ǽð� ��ġ�� ���

install.packages("ggmap")
#google map�� �̿��ϱ� ���� ��Ű��

library("XML")
library("ggmap")

API_key <- "bmbBRsCJOA1EpIK8jgnm6LPMzcKqAyQyewr%2F7U9dTMepFlMRE%2FQttSNB7gD2p7r1sxuiCuybpJUfDFq1pQlnUA%3D%3D"

bus_no <- "462"
# ã���� �ϴ� ���� ��ȣ

url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getBusRouteList?ServiceKey=",API_key,"&strSrch=",bus_no,sep="")
# sep=""�� ���ڿ� ���̸� ������� �ٿ� �����϶�� �ǹ�.

xmlfile <- xmlParse(url)
# xml������ R���� �ν��� �� �ְ� ���� ����
xmltop <- xmlRoot(xmlfile)
# XML ���� ��ü�� ��Ʈ ��忡 ����, �ʿ��� �������� �������� ����.

DF <- xmlToDataFrame(getNodeSet(xmlfile,"//itemList"))
# xmlfile�� ��� �̸��� ItemList�� ��� ����� ��������� �׸������ �ϰ�, �� �����͵�� �����Ǵ� ���������������� ��ȯ
DF

busRouteId <- "100100458"
# 462�� ���� ID��ȣ

final_url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getRoutePath?ServiceKey=",API_key, "&busRouteId=", busRouteId, sep="")

doc <- xmlParse(final_url)
top <- xmlRoot(doc)
top
# ���� ���� 462�� ������ ���� ��ġ������

DF2 <- xmlToDataFrame(getNodeSet(doc,"//itemList"))
DF2
str(DF2)

DF2$gpsX <- as.numeric(as.character(DF2$gpsX))
DF2$gpsY <- as.numeric(as.character(DF2$gpsY))
str(DF2)
# factor�� ���ڿ��� ��ȯ �� ���ڷ� ��ȯ.

gc <- data.frame(lon=DF2$gpsX,lat=DF2$gpsY)
gc

cent <- c(mean(gc$lon),mean(gc$lat))
# ������ �߽��� ǥ���ϰ� ���� �ܽ���ǥ
gc <- gc[c(1:80),]
# �����Ͱ� �ʹ� ���� �޸� �ʰ��� ���� �ϱ� ���� �տ� ��� ǥ����
map <- get_googlemap(center=cent,maptype="roadmap",zoom=14 ,marker=gc)
# ������ �߽��� cent, type�� �ε��, 
ggmap(map,extent = "device")