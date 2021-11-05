
object OptionFriends extends App {
  def lookup(xs: List[(String, String)], key: String): Option[String] = {
    if(xs.isEmpty) None else {
      if(xs.head._1 == key) Some(xs.head._2) else lookup(xs.tail, key)
    }
  }

  def resolve(userIdFromLoginName: String => Option[String],
              majorFromUserId: String => Option[String],
              divisionFromMajor: String => Option[String],
              averageScoreFromDivision: String => Option[Double],
              loginName: String): Double = userIdFromLoginName(loginName).flatMap(majorFromUserId).flatMap(divisionFromMajor).flatMap(averageScoreFromDivision).fold(0.0)(x => x + 0)
}
