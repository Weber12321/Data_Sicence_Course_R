# Weber1234
<h3> 圖資四 黃彥鈞 </h3>
<h4> for data science class</h4>
<br/>
<h4> Week 1</h4><br/>
<p>
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

<h4> Week 2</h4><br/>
<p>
 使用<b>rvest</b>以及<b>CSS selector</b>練習爬取網頁資訊，以Dcard主頁熱門文章為目標網址<br/>
 爬取內容為 :<br/>
 1. 文章標題<br/>
 2. 文章主題分類<br/>
 3. 文章學校來源<br/>
 4. 文章讚數<br/>
 5. 文章回應數<br/>
 
 建立並且匯出dataframe以儲存上述資料<br/>
 最後，編些R Markdown內容與Knit HTML
 <br/>
 >>遇到問題 : 原本想用XML與xpath語法來爬取資料，不過結果不慎理想。相較之下，rvest語法簡易許多QQ
</p> 
 
<h4> Week 3</h4><br/>
<p>
 目標 :<br/>
 運用ggplot2與ggmap將北市106年度各區人口數與106年度公共圖書館位址做比較。首先匯入北市範圍為基準的地圖，再運用熱量圖作為人口數底圖，並用點陣圖標示圖書館位置以利觀察。<br/>
 參考資源 :<br/>
 參考資料來源 :<br/>
 1. 台北市各行政區人口數及戶數<https://ca.gov.taipei/News_Content.aspx?n=F98484FF6E3A5230&sms=D19E9582624D83CB&s=EE7D5719108F4026><br/>
 2. 3碼郵遞區號與行政區中心點經緯度對照表<https://data.gov.tw/dataset/25489><br/>
 資料經過處理維度均改為<b>經緯度</b>

</p>

<h4> Week 4</h4><br/>
<p>
    任務目標 :<br/>
    1. 使用<i>facebook graphic api explorer</i>爬取資料，用<b>Rfacebook處理</b><br/>
    2. 使用<b>tm、NLP、結巴</b>等套件清理文本內容<br/>
    3. 使用wordclould套件繪製文字雲<br/>
</p>
<p>
    參考資源 :<br/>
    1. "https://www.facebook.com/DoctorKoWJ/" <i>柯文哲市長臉書專頁</i><br/>
    2. <b>facebook graphic api explorer</b><br/>
</p> 
<p>
    遇到問題 :
    1. R版本問題，slam、tm、NLP無法安裝 (解決方法 : 用installr升級)<br/>
    2. 字體問題，warning message : font family not found in Windows font database (未解決)<br/>
    3. 迴圈無法output大量資料 (未解決)<br/>
    4. Rjava 無法安裝(未解決)<br/>
    5. segmentCN 無法使用(未解決)<br/>
</p>
 
<h4> Week 5</h4><br/>
<p>
 目標:這周目的是要對文章作詞頻分析<br/>
 心得 : 由於我對這周主題真的不是很在行，所以就以助教的範例為練習對象，自己找文章寫了一個<br/>
 參考資源 : PPT男女版文章 https://www.ptt.cc/bbs/Boy-Girl/index.html <br/>
 遇到問題:<br/>
   PPT文章xpath路徑並不是完全標準，e.g.time tag就不同
</p>
<h4>project 2</h4>
 <p>
 作業連結 : https://github.com/Weber12321/Weber1234/blob/master/Project%202/Group-5-Project-2.pdf
 </p>
<h4>project 3</h4>
 <p>
 作業連結 : [project3(titanic)](https://weber12321.github.io/Weber1234/Project3/project_3.nb.html)<br/>
 作業說明 : <br/>
 使用給定train 資料來預測test 資料生還結果，有別於project2 這次資料多了boat、body、home.dest欄位<br/>
 寫作情況 : 與組員討論互相完成<br/>
 遇到的問題 : 無法清楚得知body確切意義，不過可以存訓練資料中看出，有明確被標出body實質資訊的乘客，都沒有<b>生還</b><br/>
 顯示body的重要性十足，已具有成為預測參數的資格。初步猜測body為遺體資訊，具體規律無從得知。
 </p>

<h4>project 4</h4>
 <p>
 作業連結 : [project4(iris)](https://weber12321.github.io/Weber1234/project4/project4.html)<br/>
 作業說明 : <br/>
 此第四次作業將使用自訂資料組，來練習關聯式規則arules 我們這組互相討論，將使用R內建資料庫iris來嘗試。<br/>
 在這次作業，首先我們將所有資料因素化來求結果，但跑不出任何內容。<br/>
 問題一 :<br/>
 發現問題所在乃iris資料集的資料類型都是數值化的，而apriori不能使用數值資料判斷，<br/>
 因此我們撰寫函數，將所有資料由小而大分成三個區間，並且類別化，從而得到結果。<br/>
 問題二 :<br/>
 另外一個問題在於apriori()方法中Appearence中也遇到錯誤，原本以為真的是鬼打牆，但是結果竟然是此方法中不能有任何縮排==“刪去縮排就能跑出結果。<br/>
 寫作情況 : 與組員討論互相完成<br/>
 
 結果 :<br/>
 iris資料組其實蠻一翻兩瞪眼的，資料間confidence都很高，甚至有一些到10，因此我們得出結論是，這三種花卉(setosa、versicolor、virginica)的生物特性其實分類也很直觀，不同種類有相異的長寬區間，所以出來的結果這麼極端。
 </p>

 
