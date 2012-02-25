package code.lib

/**
 * Created by IntelliJ IDEA.
 * User: dpp
 * Date: 2/6/12
 * Time: 1:02 PM
 * To change this template use File | Settings | File Templates.
 */

import net.liftweb.util.Helpers._
import net.liftweb._
import http._
import sitemap._
import common._
import scala.xml._
import sitemap.Menu.ParamsMenuable._

/* my comment */

case class Album(name: String)

case class Topic(album: Album, name: String)

case class Foto(topic: Topic, name: String)


object AlbumMenu {
	
  // define the foto menu with no further submenus
  lazy val foto = Menu.params[Foto](
		"Foto", 
		Loc.LinkText(f => Text("Foto " + f.name)),
		{
			case album :: topic :: foto :: Nil =>
				Full(Foto(Topic(Album(album), topic),foto))
			case _ => Empty
		}, f => List(f.topic.album.name, f.topic.name, f.name)) / "gallery" / * / * / *
		
		def currentFotoValue(): Box[Foto] = {
			val uri = S.uri.split("/")
			if(uri.length>4)Full(Foto(Topic(Album(uri(2)),uri(3)),uri(4)))
			else Empty
		}

  // define the topic menu which has foto-s as submenus
  lazy val topic = Menu.params[Topic](
		"Topic",
		Loc.LinkText(t => Text("Topic " + t.name)),
		{
			case album :: topic :: Nil => Full(Topic(Album(album), topic))
			case _ => Empty
		}, t => List(t.album.name, t.name)) / "gallery" / * / *  >>
			Loc.CalcValue(currentTopicValue) submenus(foto)

		def currentTopicValue(): Box[Topic] = {
			val uri = S.uri.split("/")
			if(uri.length>3)Full(Topic(Album(uri(2)),uri(3)))
			else Empty
		}

  // define the album menu which has topic-s as submenus
  lazy val album = Menu.params[Album](
		"Album",
		Loc.LinkText(a => Text("Album " + a.name)),
		{
			case album :: Nil => Full(Album(album))
			case _ => Empty
		}, a => List(a.name)) / "gallery" / *  >>
			Loc.CalcValue(currentAlbumValue) submenus(topic)
			
		def currentAlbumValue(): Box[Album] = {
			val uri = S.uri.split("/")
			if(uri.length>2)Full(Album(uri(2)))
			else Empty
		}


}

