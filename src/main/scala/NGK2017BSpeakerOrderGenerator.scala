import java.util.concurrent.TimeUnit
import java.security.SecureRandom

import scala.annotation.tailrec
import scala.concurrent.duration.{ Duration, FiniteDuration }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Await, Future, TimeoutException }
import scala.util.{ Properties, Random }

object NGK2017BSpeakerOrderGenerator {

  type SpeakerName = String
  case class Speaker(name: SpeakerName, sponsor: Option[String] = None)

  type GroupIndex = Int
  case class Group(speakers: Seq[Speaker], index: GroupIndex)

  type Constraint = (Speaker, Group) => Boolean
  type Constraints = Map[SpeakerName, Constraint]

  private implicit def implicitAsOption(s: String): Option[String] = Option(s)


  def generate(speakers: Seq[Speaker], constraints: Constraints, speakerPerGroup: GroupIndex): Future[Seq[Group]] = {
    if (speakerPerGroup <= 0) {
      return Future.failed(new IllegalArgumentException("speakerPerGroupは0より大きくしてください."))
    }

    val defaultConstraints: Constraint = (_, _) => true
    val random = new Random(new SecureRandom())

    @tailrec
    def generateGroups(): Seq[Group] = {
      val generated = random.shuffle(speakers)
        .grouped(5).toList
        .zipWithIndex
        .map { case (group, index) => Group(group, index + 1) }

      val allConstraintSatisfied = generated.forall(group => {
        group.speakers.forall(speaker => {
          val constraint = constraints.getOrElse(speaker.name, defaultConstraints)
          constraint(speaker, group)
        })
      })

      if (allConstraintSatisfied) {
        generated
      } else {
        generateGroups()
      }
    }

    Future(generateGroups())
  }


  def main(args: Array[String]): Unit = {
    val speakers = Seq(
      Speaker("sqm8", "株式会社春木メディカルサービス"),
      Speaker("ysk-tngc", "フェンリル株式会社"),
      Speaker("youhei_yamaguchi", "有限会社来栖川電算"),
      Speaker("aua2008"),
      Speaker("mzp"),
      Speaker("kaizen_nagoya"),
      Speaker("noob"),
      Speaker("mituhiromatuura"),
      Speaker("y_taka_23"),
      Speaker("sh-ogawa"),
      Speaker("Kuxumarin"),
      Speaker("hirokundayon"),
      Speaker("yoshihiro503"),
      Speaker("Ryuichirou"),
      Speaker("kawaji_scratch"),
      Speaker("katzueno"),
      Speaker("MasakiOhta"),
      Speaker("niwasawa"),
      Speaker("tinymouse_jp"),
      Speaker("maeda_"),
      Speaker("yusuke_kokubo"),
      Speaker("kazuki_kachi"),
      Speaker("smogami"),
      Speaker("藤井智康", "トビラシステムズ株式会社"),
      Speaker("ninochi", "Yahoo! JAPAN"),
      Speaker("eightball", "株式会社Misoca"),
      Speaker("garriguejej"),
      Speaker("niccolli"),
      Speaker("terurou", "デンキヤギ株式会社"),
      Speaker("bleis-tift")
    )

    val constraints = Map[SpeakerName, Constraint](
      // 毎朝体操杯は食後の眠くなりそうな時間帯LT#2の1発目
      "youhei_yamaguchi" -> ( (speaker, group) => group.index == 3 && group.speakers.headOption.contains(speaker) ),

      // 日中予定があるため最後の回をご希望
      "aua2008" -> ( (speaker, group) => group.index == 6 ),

      // 家族サービスのため15時以降ご希望
      "kawaji_scratch" -> ( (speaker, group) => group.index >= 3 )
    )

    val generateSpeakerOrderFuture = try {
      val speakerOrderFuture = NGK2017BSpeakerOrderGenerator.generate(speakers, constraints, speakerPerGroup = 5)
      Await.ready(speakerOrderFuture, FiniteDuration(5, TimeUnit.SECONDS))
    } catch {
      case e: TimeoutException =>
        val msg = s"制約条件を満たす発表順が時間内に見つかりませんでした. 同時に満たせない条件がありませんか？: ${ e.getLocalizedMessage }"
        Future.failed(new TimeoutException(msg))
    }

    val printSpeakerOrderFuture = for {
      speakerOrder <- generateSpeakerOrderFuture
    } yield {
      val indent = " " * 4
      speakerOrder.foreach { case Group(group, index) =>
        println(s"LT#${ index }")
        println(group.map {
          case Speaker(name, Some(org)) => s"${ indent }${ name }(${ org })"
          case Speaker(name, None) => s"${ indent }${ name }"
        }.mkString(Properties.lineSeparator))
      }
    }

    Await.result(printSpeakerOrderFuture, Duration.Inf)
  }
}