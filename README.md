# 資料科學程式設計 Data Science Programming
我是**Weber**，很高興認識你。 
我正在學習資料科學，這是我2018年在**台灣大學**修習 「資料科學程式設計」的**作品頁**。

Hello, I'm Weber. I'm learning data science. Nice to meet you! 
This page contains all the **works** for the class ''**2018 spring Data Science Programming**'' at National Taiwan University (NTU)

## 目錄
[資料前處理練習](#資料前處理練習)

1. [Week 1 : 練習 R 語言](#Week1)
2. [Week 2 : 資料爬蟲練習](#week2)
3. [Week 3 : 資料視覺化練習](#week3)
4. [Week 4 : 爬蟲與視覺化小結](#week4)
5. [Week 5 : 文本分析練習](#week5)
6. [Week 6 : 期中個人專題撰寫](#week6)

[機器學習練習](#機器學習練習)

7. [Week 7 : 資料科學作品分析](#week7)
8. [Week 8 : 建模練習 :  SVM 與 GB](#week8)
9. [Week 9 : 建模練習 : Arules](#week9)
10. [Week 10 : 建模練習 : nnet](#week10)

[期末專題](#期末專題)

1. [摘要](#一、摘要)
2. [作品連結](#二、作品連結)
3. [作品介紹](#三、作品介紹)
4. [結論](#四、結論)

# 資料前處理練習
### Week 1 : 練習 R 語言
  作業連結 : [HW_1](https://github.com/Weber12321/Weber1234/tree/master/HW_1)     
  寫作情況 : 自行完成    
  任務目標 : R 語言練習     
  
    1. Practice 1
        運用if-else句，並且輸入身高資料，來撰寫一個BMI計算器
    2. Practice 2
        計算30位學生的成績，找出不同的統計值；並且分析高於80分的人數
    3. Practice 3
        學習用資料框找出person.df的細節資料
    4. HW_1
        查看並且歸類IRIS資料組，觀察細節資訊
        使用for-loop 印出九九乘法表
        學習用sample()
        撰寫閏年判斷器
        寫猜數字遊戲

### Week 2 : 資料爬蟲練習 I
  作業連結 : [HW_2](https://weber12321.github.io/Weber12 34/HW_2/HW_2_R.html)    
  寫作情況 : 自行完成   
  任務目標 : 使用**rvest**以及**CSS selector**練習爬取網頁資訊     
  資料來源 : [Dcard](https://www.dcard.tw/f)   
  
	爬取 :		  
	 1. 文章標題
	 2. 文章主題分類
	 3. 文章學校來源
	 4. 文章讚數
	 5. 文章回應數
 >  遇到問題 :  
   原本想用XML與xpath語法來爬取資料，不過結果不慎理想。相較之下，rvest語法簡易許多QQ
   
     
     
       
### Week 3 : 資料視覺化練習
  作業連結 : [HW_3](https://weber12321.github.io/Weber1234/HW_3/HW3_DataVisualizationRmd.html)    
  寫作情況 : 自行完成   
  任務目標 : 使用 **ggplot**與 **ggmap** 比較人口密度與公共圖書館密度     
  資料來源 :   
  [台北市立圖書館](https://tpml.gov.taipei/News.aspx?n=4F66F55F388033A7&sms=CFFFC938B352678A)    
   [台北市各行政區人口數及戶數](https://ca.gov.taipei/News_Content.aspx?n=F98484FF6E3A5230&sms=D19E9582624D83CB&s=EE7D5719108F4026)    
[3碼郵遞區號與行政區中心點經緯度對照表](https://data.gov.tw/dataset/25489)   


### Week 4 : 爬蟲與視覺化小結
  作業連結 : [HW_4](https://weber12321.github.io/Weber1234/HW_4/facebookDATAwordclould.html)   
  寫作情況 : 自行完成   
  任務目標 :   
  1. 使用 **facebook graphic api explorer** 與 **Rfacebook** 處理爬蟲資料   
  2. 使用 **tm、NLP、jieba** 等套件清理文本內容  
  3. 使用 **wordclould** 套件繪製文字雲   
 
  資料來源 : [柯文哲臉書粉絲專頁](https://www.facebook.com/DoctorKoWJ/)
  > 遇到問題 :  
	1. R版本問題，slam、tm、NLP無法安裝 (解決方法 : 用installr升級)  
	2. 字體問題，warning message : font family not found in Windows font database 
	3. 迴圈無法output大量資料 
	4. Rjava 無法安裝
	5. segmentCN 無法使用
 
### Week 5 : 文本分析練習

  作業連結 : [HW_5](https://weber12321.github.io/Weber1234/HW_5/HW_5.html)    
  寫作情況 : 自行完成   
  任務目標 : 柯文哲市長 2018 臉書分粉絲專頁發文文本分析**TF-IDF**   
  資料來源 : [柯文哲臉書粉絲專頁](https://www.facebook.com/DoctorKoWJ/)  
  > 遇到問題  
   切完的詞在建立詞頻矩陣步驟時，總會出現些許亂碼

### Week 6 : 期中個人專題撰寫
  作品連結 : [Project 1](https://weber12321.github.io/Weber1234/Project1/project1.html)   
  寫作情況 : 自行完成   
  任務目標 : 撰寫柯文哲市長 2018 1-5月 聯合報新聞文本分析專題   
  資料來源 : [柯文哲聯合報](https://udn.com/search/result/2/%E6%9F%AF%E6%96%87%E5%93%B2)   
  > 遇到問題
      1.  詞頻出現英文不知是不是編碼問題，切詞應該去除所有的英文符號
      2.  DF、TF、TFIDF都無法用head()顯示內容，推測資料量過大無法顯示
 
 P.s 這周停課

# 機器學習練習

### Week 7 : 資料科學作品分析
作業連結 :  [project 2](https://github.com/Weber12321/Weber1234/blob/master/Project2/Group-5-Project-2.pdf)   
寫作情況 : 小組完成   
任務目標 : 鐵達尼號罹難者建模 - **作品分析** (以 Kaggle 比賽參賽者程式為對象)     
資料來源 : [Titanic: 2nd degree families and majority voting](https://www.kaggle.com/erikbruin/titanic-2nd-degree-families-and-majority-voting)

### Week 8 : 建模練習 :  SVM 與 GB
作業連結 :  [project 3](https://weber12321.github.io/Weber1234/Project3/project_3.nb.html)   
寫作情況 : 小組完成   
任務目標 : 使用**鐵達尼號乘客原始資料**練習建模    
資料來源 : [Titanic](https://github.com/NTU-CSX-DataScience/106-2RSampleCode/tree/master/week_8/task_8)   
  
>遇到的問題 :  
無法清楚得知body確切意義，不過可以存訓練資料中看出，有明確被標出body實質資訊的乘客，都沒有**生還**  
顯示body的重要性十足，已具有成為預測參數的資格。初步猜測body為遺體資訊，具體規律無從得知。

### Week 9 : 建模練習 : Arules
作業連結 :  [project 4](https://weber12321.github.io/Weber1234/project4/project4.html)    
寫作情況 : 小組完成   
任務目標 : 使用R資料集 **iris 資料** 練習建模    
>遇到的問題 :  
	1. iris資料集的資料類型都是數值化的，而apriori不能使用數值資料判斷， 因此撰寫函數，將所有資料由小而大分成三個區間，並且類別化，從而得到結果。 
	2. 另外一個問題在於apriori()方法中Appearence中也遇到錯誤，原本以為真的是鬼打牆，但是結果竟然是此方法 中不能有任何縮排==“刪去縮排就能跑出結果。     
結果 :    
	iris資料組其實蠻一翻兩瞪眼的，資料間confidence都很高，甚至有一些到10，因此我們得出結論是，這三種花卉(setosa、versicolor、virginica)的生物特性其實分類也很直觀，不同種類有相異的長寬區間，所以出來的結果這麼極端。

### Week 10 : 建模練習 : nnet
作業連結 :  [project 5](https://weber12321.github.io/Weber1234/Project5/project.html)  
寫作情況 : 小組完成   
任務目標 : 使用 **wine 資料** 練習建模   
資料來源 : [wine](https://rdrr.io/cran/rattle.data/man/wine.html)  

# 期末專題
## 選戰懶人包 - 以2018台北市長候選人為分析對象
### 一、 摘要 :

依據2018年1-5月四大報新聞媒體文章、Facebook社群做文本分析與情緒分析，來探究台北市三大候選人(柯文哲、丁守中、姚文智)的關注議題、受眾(社群)有感議題、以及媒體對於這三位候選人的關注議題，與報導是否有偏頗傾向。

### 二、作品連結 :
寫作情況 : 小組完成   
1. R shiny : [https://dppss90008.shinyapps.io/news_shiny/](https://dppss90008.shinyapps.io/news_shiny/)  
2. PPT簡報 : [https://docs.google.com/presentation/d/1p2vua3FNWMaGRWYAarCJ8WTwRFNiI9LmvhUbOLukDQY/edit?usp=sharing](https://docs.google.com/presentation/d/1p2vua3FNWMaGRWYAarCJ8WTwRFNiI9LmvhUbOLukDQY/edit?usp=sharing)  

### 三、作品介紹 :

#### 1. 選題動機 :

1. 協助公眾認識候選人所關心的議題  
2. 比較四大報如何報導台北市三位候選人  
3. 公眾在社群平台對於三位候選人的討論度  

#### 2. 預期得到結果 :

1. 從情緒分析結果來看，四大報對於各個候選人的報導是否有所偏頗？  
2. 報紙報導的候選人關注議題與候選人本身關注議題是否相關？  
3. 公眾偏好以及強烈有感的議題為何？  

#### 3. 資料蒐集 :

1. 蒐集蘋果日報、自由時報、聯合報網站 1月至5月 有關於柯文哲、丁守中、姚文智的新聞  
2. 蒐集中國時報報系1月至5月有關於柯文哲、丁守中、姚文智的平面報導  
3. 蒐集柯文哲、丁守中、姚文智從今年1月至今的臉書發文與每則發文的分享量、按讚量等資訊  

#### 4. 資料處理與預期呈現

1. 四大報文本 :  
﹒將1月至5月的新聞文本  
﹒做情緒分析與LDA  
﹒逐月做新聞文本的文字雲  
2. 臉書粉專文章 :  
﹒情緒分析  
﹒臉書文章LDA  
﹒逐月做文章內容文字雲  
3. 公眾想法 :  
﹒臉書文章按讚量  
﹒與網民情緒量的趨勢呈現  

#### 5. 資料呈現方式 :

1. R shiny :   
﹒趨勢圖  
﹒盒狀圖  
﹒文字雲  
﹒長條圖  

#### 6. 專題流程

 議題設定 --> 資料收集 : 媒體與粉專資料爬蟲--> 資料清理 --> 資料分析 --> 資料呈現 : 文字雲、LDA、情緒分析

#### 7. 結論

分析完所有數值資料，用一句話形容候選人 :  
1. 柯文哲 : 媒體寵兒，有做事然後會希望讓大家看到的人，務實的事務官形象。  
2. 丁守中：存在感比較低，沒太多亮點，新聞幾乎都跟選舉連在一起，臉書發文的字中規中矩。  
3. 姚文智：力求突破，高飛理念型，希望爭取注意和支持。

