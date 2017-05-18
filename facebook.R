

install.packages(c('Rfacebook','KoNLP','wordcloud2'))
# Rfacebook : facebook API �� ����ϱ� ���� ��Ű��
# KoNLP : Korea Natural Language Processing Package (�ѱ��� �ڿ��� ó�� ��Ű��)
library(Rfacebook)
library(KoNLP)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_121")
# rjava ������ �߻����� �� �ذ� ���.
library(wordcloud2)

Token <- 'EAACEdEose0cBANzsUAV1qkzSGNxNomxvm6oHjdZBrTsgh1a2rj6hBLb0QdwfFWC9Ex4ZCjDQF7L7skgefwpC3D0xoMRjOgQvnjgILkRmZC73YOY3try9XFvZBZBRD68x4BGR2mXgt1l4krpqRYvUZA5cCQZAOjEtQiujA0DVnNYmsniEeo7UbkIdD3RFpkC924ZD'
# fackebook developer �� ���� ����� ������ ��ū�� �޾ƿ;� �� 
# https://developers.facebook.com/tools/explorer

fb_page <- getPage(page="moonbyun1",token=Token, n=2000, since='2016/01/01',until='2017/5/5')
# getPage�� �� ������ �����´ٴ� ������ me�Է�. moonbyun1�� ����
# n�� �ִ� ������ post ������ ����, since�� until�� ���������� �������� �Ⱓ.

fb_page$message
#�ۼ��� �� Ȯ��

useNIADic()
#�ؽ�Ʈ �м��� ���� ���¼� �м��� �ʿ��� ���¼� ������ �ٿ�޾ƾ� ��.

all.reviews <- fb_page$message
all.reviews <- all.reviews[!is.na(all.reviews)]
#na�� ����

nouns <- extractNoun(all.reviews)
#���� ����
nouns_norm <- Map(function(x){if(!is.na(x)&&is.ascii(x))toupper(x)else x},unlist(nouns))
#���ʿ��� ������ ����

cnts <- table(unlist(nouns_norm))
cnts_ <- cnts[cnts > 2 & nchar(names(cnts))>1]

wordcloud2(data.frame(word=names(cnts_),freq=as.numeric(cnts_)), color="random-light",backgroundColor = "black", shape = "cloud")
# ��� ����� �󵵼��� ���� �۾� ũ�Ⱑ �޶����� ������ random������ ���c. 