import scala.io.Source
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

object main {
  // INPUT Format
  case class Location(uuid: String, label: String, surface: Double, typeLocation: String)
  case class Lot(label: String, lignes: Seq[Ligne])
  case class Ligne(designation: String, description: Option[String], uuid: String, locationsLine: Seq[LocationLine] = Seq.empty[LocationLine])
  case class LocationLine(uuid: String, quantite: Double)

  //OUTPUT Format
  case class OutputLot(designation: String, uuid: String)
  case class OutputRoom(label: String, lots: List[OutputLot])

  // JSON Reads Format
  implicit val locationReads: Reads[Location] = (
    (JsPath \ "uuid").read[String] and
    (JsPath \ "label").read[String] and
    (JsPath \ "surface").read[Double] and
    (JsPath \ "typeLocation").read[String]
    )(Location.apply _)

  implicit val LocationLineReads:Reads[LocationLine] = (
    (JsPath \ "uuid").read[String] and
      (JsPath \ "quantite").read[Double]
    )(LocationLine.apply _ )

  implicit val LignesReads:Reads[Ligne] = (
    (JsPath \ "designation").read[String] and
      (JsPath \ "description").readNullable[String] and
      (JsPath \ "uuid").read[String] and
      ((JsPath \ "locationsDetails") \ "locations").read[Seq[LocationLine]]
    )(Ligne.apply _ )

  implicit val Lotreads:Reads[Lot] = (
    (JsPath \ "label").read[String] and
      (JsPath \ "lignes").read[Seq[Ligne]]
    )(Lot.apply _)

  implicit val outputLotWrites: Writes[OutputLot] = (
    (JsPath \ "designation").write[String] and
    (JsPath \ "uuid").write[String]
    )(unlift(OutputLot.unapply))

  implicit val outputRoomsWrites: Writes[OutputRoom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "lots").write[List[OutputLot]]
    )(unlift(OutputRoom.unapply))

  //Main Application
  def main(args: Array[String]): Unit = {
    if (args.length > 0){
      val url = args(0)
      val content = Source.fromURL(url).mkString
      val json = Json.parse(content)

      //parse Json Content
      val locations = (json \ "locations").as[Seq[Location]]
      //Define Reference Locations (Chambre, Salle d'eau, ...)
      val referenceLocations = locations.map(elem => elem.uuid -> elem.label).toMap
      val lots = (json \ "lots").as[Seq[Lot]]
      //Fetch data from input
      val result = lots.map(lot => {
        //Aggregate Ligne & Locations data by locations
        val ligneResult = lot.lignes.map(ligne => {
          val locationsUUID = ligne.locationsLine.filter(elem => referenceLocations.keys.toSeq.contains(elem.uuid))
          if (locationsUUID.isEmpty) List("Autres prestations" -> OutputLot(ligne.designation, ligne.uuid))
          else locationsUUID.map(elem => referenceLocations.getOrElse(elem.uuid, "") -> OutputLot(ligne.designation, ligne.uuid)).toList
        })
        //Group Ligne Aggregate Data by location
        val locationsByLigne = ligneResult.map(pr => {
          pr.groupBy(_._1).toList.map(l => l._1 -> l._2.map(_._2))
        }) reduce (_ ++ _)
        //Group Lots Aggregate Data by Location
        val locationsByLot = locationsByLigne.groupBy(_._1).toList.map(l => l._1 -> l._2.flatMap(_._2))
        locationsByLot
      }).toList
      val merged = result reduce (_ ++ _)
      val rooms = merged.map(elem => OutputRoom(elem._1, elem._2))
      //Create JSON Output
      println(Json.toJson(rooms))
    } else println("Need Url Argument !")
  }
}
