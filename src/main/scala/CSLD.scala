package tw.`3du`

import com.github.tototoshi.csv._
import java.io._
import scala.xml._

/*
0: 稿件版本
1: 稿件階段
2: 稿件狀態
3: 字詞流水序
4: 正體字形
5: 簡化字形
6: 音序
7: 臺／陸特有詞
8: 臺／陸特有音
9: 臺灣音讀
10: 臺灣漢拼
11: 大陸音讀
12: 大陸漢拼
13: 釋義１
14: 釋義２
15: 釋義３
16: 釋義４
17: 釋義５
18: 釋義６
19: 釋義７
20: 釋義８
21: 釋義９
22: 釋義１０
23: 釋義１１
24: 釋義１２
25: 釋義１３
26: 釋義１４
27: 釋義１５
28: 釋義１６
29: 釋義１７
30: 釋義１８
31: 釋義１９
32: 釋義２０
33: 釋義２１
34: 釋義２２
35: 釋義２３
36: 釋義２４
37: 釋義２５
38: 釋義２６
39: 釋義２７
40: 釋義２８
41: 釋義２９
42: 釋義３０

Total: 77629 entries. Each row has exactly 43 columns.

Length of entries:
14 : 57762
15 : 13696
16 : 3504
17 : 1150
18 : 552
19 : 315
20 : 199
21 : 130
22 : 99
23 : 64
24 : 51
25 : 35
26 : 18
27 : 17
28 : 10
29 : 6
30 : 5
31 : 4
32 : 1
33 : 2
34 : 1
35 : 3
36 : 1
40 : 1
43 : 3
*/

trait Specialized {
  def tag: scala.xml.Elem
}

case object 臺灣 extends Specialized {
  def tag = <span class="taiwan_specialized">臺</span>
}
case object 大陸 extends Specialized {
  def tag = <span class="china_specialized">陸</span>
}

case class Entry(
  稿件版本: String, 
  稿件階段: String, // 終定稿
  稿件狀態: String,
  字詞流水序: String, // 1000010000, 1000010070.001, 每個 row 都有
  正體字形: String,
  簡化字形: String,
  音序: Option[Int], // 1, 2. Sometimes 1., 2. 同一詞彙有不同發音, 字詞流水序相同的收集起來
  臺陸特有詞: Option[Specialized], // ▲ or ★, ▲=Taiwan, ★=China
  臺陸特有音: Option[Specialized], // ▲ or ★
  臺灣音讀: String, // 注音，有時也會沒有。例如大陸特有音的犯難（ㄈㄢˋㄋㄢˊ）
  臺灣漢拼: String, // 漢拼
  大陸音讀: String, // 注音，如果和臺灣音讀不同才會顯示
  大陸漢拼: String,
  釋義: Vector[String]
) {
  def pronounceXML(pr: String, cssClass: String)(t: String, c: String) = (t, c) match {
    case (t, "") ⇒
      <span d:pr={pr} class={cssClass}>{t}</span>
    case ("", c) ⇒
      <span d:pr={pr} class={cssClass}>{c}</span>
  }
}

object Entry {
  def apply(row: List[String]): Entry = {
    Entry(
      稿件版本 = row(0), 
      稿件階段 = row(1),
      稿件狀態 = row(2),
      字詞流水序 = row(3),
      正體字形 = row(4),
      簡化字形 = row(5),
      音序 = row(6) match { case "" => None case s => Some(s.toDouble.toInt) },
      臺陸特有詞 = row(7) match { case "▲" => Some(臺灣) case "★" => Some(大陸) case _ => None },
      臺陸特有音 = row(8) match { case "▲" => Some(臺灣) case "★" => Some(大陸) case _ => None },
      臺灣音讀 = row(9),
      臺灣漢拼 = row(10),
      大陸音讀 = row(11),
      大陸漢拼 = row(12),
      釋義 = row.drop(13).reverse.dropWhile(_ == "").reverse.toVector
    )
  }
}

object MoeDictDataCSLD extends App {
  val reader = CSVReader.open(new File("data/兩岸常用詞典2013.csv"))

  val out = new PrintWriter("template/MoeDictionary.xml", "UTF-8")
  out.println("""<?xml version="1.0" encoding="UTF-8"?>""")
  out.println("""<d:dictionary xmlns="http://www.w3.org/1999/xhtml" xmlns:d="http://www.apple.com/DTDs/DictionaryService-1.0.rng">""")

  
  reader.toStream
  .drop(1) // drop the first line (schema)
  // .take(20000)
  .map(Entry.apply)
  .groupBy(_.字詞流水序.take(10))  // 有時會有 1000010070.001 這種形式
  .map { case (id, entries) => {
    val indexes = (entries.toList.flatMap(e => List(e.正體字形, e.簡化字形)).toSet - "").toList
    val title = if (entries.head.正體字形 != "") entries.head.正體字形 else entries.head.簡化字形

    <d:entry id={id} title={title}>
    {indexes.map(idx => <d:index d:value={idx}/>)}
    {
      entries.sortBy(_.音序).map(e => {
        <h1>{e.正體字形}{e.臺陸特有詞.map(s => s.tag).orElse(e.臺陸特有音.map(s => s.tag)).getOrElse(<span></span>)}</h1>
        <span d:pr="bpmf" class="bopomofo">
        {if (e.臺灣音讀 != "" && e.大陸音讀 != "") 臺灣.tag else <span></span>}{e.臺灣音讀}
        </span>
        <span d:pr="bpmf" class="bopomofo">
        {if (e.臺灣音讀 != "" && e.大陸音讀 != "") 大陸.tag else <span></span>}{e.大陸音讀}
        </span>
        <span d:pr="pinyin" class="pinyin">
        {if (e.臺灣漢拼 != "" && e.大陸漢拼 != "") 臺灣.tag else <span></span>}{e.臺灣漢拼}
        </span>
        <span d:pr="pinyin" class="pinyin">
        {if (e.臺灣漢拼 != "" && e.大陸漢拼 != "") 大陸.tag else <span></span>}{e.大陸漢拼}
        </span>
        <div>
          <ol>{
            e.釋義.map(definition =>
              <li><p class="definition">{
                Unparsed("""^\d+\.""".r.replaceFirstIn(definition, ""))
              }</p></li>
            )
          }</ol>
        </div>
      })
    }
    </d:entry>
  }}
  .foreach(out.println(_))
  out.println("""</d:dictionary>""")
  out.close
  reader.close
}