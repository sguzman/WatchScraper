package com.github.sguzman.scraper.watch

import java.util.Base64

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.elementList
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.element
import org.apache.commons.lang3.StringUtils

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
    val pages = Init.cascade(url, proc, dec)
      .filter(ep => ep.link != "https://www.watchcartoononline.io/anime/pripri-chii-chan-english-subbed")
      .par

    print(pages.toList.asJson.spaces4)

    final case class AnimeInfo(img: String, desc: Option[String], genres: List[String], eps: List[String])

    val episodes = pages
      .map{ep =>
      def proc(doc: Browser#DocumentType): String = {
        AnimeInfo(
          doc.>>(element("#cat-img-desc > div > img[src]")).attr("src"),
          for {
            desc <- doc.>?>(element("#cat-img-desc > div.iltext > p"))
          } yield desc.text,
          doc.>>(elementList("#cat-genre > div.wcobtn > a")).map(_.text),
          doc.>>(elementList("#catlist-listview > ul > li > a[href]")).map(_.attr("href"))
        ).asJson.spaces4
      }

      def dec(s: String): AnimeInfo = decode[AnimeInfo](s).right.get
      Init.cascade(ep.link, proc, dec)
    }

    Init.removeHttp(pages.toList.map(_.link))

    val iframe = episodes
      .flatMap(_.eps)
      .map{a =>
      def proc(doc: Browser#DocumentType): String = {
        val script = doc.>>(element("""meta[itemprop="embedURL"] + script""")).innerHtml
        val array = script.split(";")

        val sub = array(2).split("\\)").last.trim.stripPrefix("-").trim.toLong

        val data = StringUtils.substringBetween(array(1), "[", "]")
        val dataArray = data.replaceAll("\"", "").split(", ").toList
        val done = dataArray
          .map(Base64.getDecoder.decode)
          .map(_.map(_.toChar).mkString)
          .map(_.replaceAll("[^0-9]", ""))
          .map(_.toInt)
          .map(_ - sub)
          .map(_.toChar)
          .mkString

        JsoupBrowser().parseString(done).>>(element("iframe[src]")).attr("src")
      }

      def dec(s: String): String = s
      Init.cascade(a, proc, dec)
    }

    Init.removeHttp(episodes.toList.flatMap(_.eps))
  }
}
