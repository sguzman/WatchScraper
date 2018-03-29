package com.github.sguzman.scraper.watch

import io.circe.syntax._
import io.circe.parser.decode
import io.circe.generic.auto._
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.elementList
import net.ruippeixotog.scalascraper.dsl.DSL._

object Main {
  def main(args: Array[String]): Unit = {
    final case class Anime(title: String, link: String)

    val url = "https://www.watchcartoononline.io/subbed-anime-list"
    def proc(doc: Browser#DocumentType): String = {
      doc.>>(elementList("#ddmcc_container > .ddmcc > ul > ul > li > a[href]"))
        .map(a => Anime(a.attr("title"), a.attr("href")))
        .asJson.spaces4
    }

    def dec(s: String): List[Anime] = decode[List[Anime]](s).right.get
    val pages = Init.cascade(url, proc, dec).par

    print(pages.toList.asJson.spaces4)
  }
}
