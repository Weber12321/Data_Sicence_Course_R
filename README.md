# Weber1234
<h1>Hello, I'm Weber. I'm learning data sicence. Nice to meet you! </h1>
<h4>This is the link for course of Data Science Programming from National Taiwan Univerity(NTU) 2018 spring</h4>
<br/>


<h2> Week 1</h2>
<p>
  HW_1 : 練習一些R語言基礎<br/>
    >>>作業連結 : https://github.com/Weber12321/Weber1234/tree/master/HW_1<br/>
  寫作情況 : 自行完成<br>
1.Practice 1<br/>
    運用if-else句，並且輸入身高資料，來撰寫一個BMI計算器<br/>
2.Practice 2<br/>
    計算30位學生的成績，找出不同的統計值；並且分析高於80分的人數<br/>
3.Practice 3<br/>
    學習用資料框找出person.df的細節資料<br/>
4.HW_1<br/>
    查看並且歸類IRIS資料組，觀察細節資訊<br/>
    使用for-loop 印出九九乘法表<br/>
    學習用sample()<br/>
    撰寫閏年判斷器<br/>
    寫猜數字遊戲<br/>
</p>

<h2> Week 2</h2>
<p>
 HW_2 : 使用<b>rvest</b>以及<b>CSS selector</b>練習爬取網頁資訊 (Dcard主頁熱門文章為目標網址)<br/>
 >>>作業連結 : https://weber12321.github.io/Weber1234/HW_2/HW_2_R.html<br/>
  寫作情況 : 自行完成<br>
 爬取內容為 :<br/>
 1. 文章標題<br/>
 2. 文章主題分類<br/>
 3. 文章學校來源<br/>
 4. 文章讚數<br/>
 5. 文章回應數<br/>
 >遇到問題 : <br/>
 原本想用XML與xpath語法來爬取資料，不過結果不慎理想。相較之下，rvest語法簡易許多QQ
</p> 
 
<h2> Week 3</h2>
<p>
 HW_3 : 練習ggplot與ggmap (北市106年度各區人口數與106年度公共圖書館位址做比較)<br/>
 >>>作業連結 : https://weber12321.github.io/Weber1234/HW_3/HW3_DataVisualizationRmd.html<br/>
  寫作情況 : 自行完成<br>
 目的 : 首先匯入北市範圍為基準的地圖，再運用熱量圖作為人口數底圖，並用點陣圖標示圖書館位置以利觀察。<br/>
 參考資料來源 :<br/>
 1. 台北市各行政區人口數及戶數<https://ca.gov.taipei/News_Content.aspx?n=F98484FF6E3A5230&sms=D19E9582624D83CB&s=EE7D5719108F4026><br/>
 2. 3碼郵遞區號與行政區中心點經緯度對照表<https://data.gov.tw/dataset/25489><br/>
 資料經過處理維度均改為<b>經緯度</b>
</p>

<h2> Week 4</h2>
<p>
    HW_4 : 練習使用woldcloud套件化文字雲 (以柯文哲臉書發文為分析對象)<br/>
    >>>作業連結 : https://weber12321.github.io/Weber1234/HW_4/facebookDATAwordclould.html<br/>
  寫作情況 : 自行完成<br>
    任務目標 :<br/>
    1. 使用<i>facebook graphic api explorer</i>爬取資料，用<b>Rfacebook處理</b><br/>
    2. 使用<b>tm、NLP、結巴</b>等套件清理文本內容<br/>
    3. 使用wordclould套件繪製文字雲<br/>
    使用資源 :<br/>
    1. "https://www.facebook.com/DoctorKoWJ/" <i>柯文哲市長臉書專頁</i><br/>
    2. <b>facebook graphic api explorer</b><br/>
    遇到問題 :<br/>
    1. R版本問題，slam、tm、NLP無法安裝 (解決方法 : 用installr升級)<br/>
    2. 字體問題，warning message : font family not found in Windows font database (未解決)<br/>
    3. 迴圈無法output大量資料 (未解決)<br/>
    4. Rjava 無法安裝(未解決)<br/>
    5. segmentCN 無法使用(未解決)<br/>
</p>
 
<h2> Week 5</h2>
<p>
 HW_5 : 練習使用<b>TFIDF</b> (使用柯文哲2018年臉所有發文內容為分析對象)<br>
 >>>作業連結 : https://weber12321.github.io/Weber1234/HW_5/HW_5.html<br/>
  寫作情況 : 自行完成<br>
</p>

<h2> Week 6</h2>
 <p>
 project1 : 寫一個視覺化的文本分析專題 (使用柯文哲2018年聯合報新聞為分析對象)<br>
 >>>作業連結 : https://weber12321.github.io/Weber1234/Project1/project1.html<br>
 寫作情況 : 自行完成<br>
 P.s. 這周放假~~ 
 </p>
<h2> Week 7</h2>
 <p>
 project2 : 鐵達尼號罹難者建模分析 (以 Kaggle 比賽參賽者程式為對象)<br>
 >>>作業連結 : https://github.com/Weber12321/Weber1234/blob/master/Project2/Group-5-Project-2.pdf<br>
 寫作情況 : 小組完成<br>
 </p>
<h2> Week 8</h2>
 <p>
 project3 : 實際用鐵達尼號練習建模 (使用課程給予的資料為建模對象)<br/>
 >>>作業連結 : https://weber12321.github.io/Weber1234/Project3/project_3.nb.html<br/>
 寫作情況 : 小組完成<br/>
 遇到的問題 : <br>
 問題一 :<br/>
 無法清楚得知body確切意義，不過可以存訓練資料中看出，有明確被標出body實質資訊的乘客，都沒有<b>生還</b><br/>
 顯示body的重要性十足，已具有成為預測參數的資格。初步猜測body為遺體資訊，具體規律無從得知。<br/>
 </p>

<h2> Week 9</h2>
 <p>
 project 4 : arules 建模練習 (以iris資料組為練習對象)<br/>
 >>>作業連結 : https://weber12321.github.io/Weber1234/project4/project4.html<br/>
 寫作情況 : 小組完成<br/>
 遇到的問題 :<br>
 問題一 :<br/>
 發現問題所在乃iris資料集的資料類型都是數值化的，而apriori不能使用數值資料判斷，<br/>
 因此我們撰寫函數，將所有資料由小而大分成三個區間，並且類別化，從而得到結果。<br/>
 問題二 :<br/>
 另外一個問題在於apriori()方法中Appearence中也遇到錯誤，原本以為真的是鬼打牆，但是結果竟然是此方法中不能有任何縮排==“刪去縮排就能跑出結果。<br/>
 寫作情況 : 與組員討論互相完成<br/>
 結果 :<br/>
 iris資料組其實蠻一翻兩瞪眼的，資料間confidence都很高，甚至有一些到10，因此我們得出結論是，這三種花卉(setosa、versicolor、virginica)的生物特性其實分類也很直觀，不同種類有相異的長寬區間，所以出來的結果這麼極端。<br>
 </p>

<h2> Week 10</h2>
 <p>
 project 5 : nnet建模練習 (以wine為建模練習對象)<br/>
 >>>作業連結 : https://weber12321.github.io/Weber1234/Project5/project.html<br/>
 寫作情況 : 小組完成<br/>
</p>

<h2> 期末專題 : 待補</h2>
