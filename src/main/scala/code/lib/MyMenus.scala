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
import sitemap.Menu
import sitemap.Menu.ParamsMenuable._
import net.liftweb.util.NamedPF

abstract class Item{}

case class Album(name: String) extends Item

case class Topic(album: Album, name: String) extends Item

case class Foto(topic: Topic, name: String) extends Item


object GalleryPage extends Loc[Item] {
	
	def defaultValue = Empty
	
	val link = new Loc.Link[Item](List("gallery"), false)
	
	def calcLinkPath(in: Item): List[String] = currentValue match {
		case Full(in: Album) => List("gallery", in.name)
		case Full(in: Topic) => List("gallery", in.album.name, in.name)
		case Full(in: Foto) => List("gallery", in.topic.album.name, in.topic.name, in.name)
	}
	
	val name = "albumLoc"
	val params = Nil
	
	val text = new Loc.LinkText(calcLinkText _)
	def calcLinkText(in: Item): NodeSeq = currentValue match {
		case Full(in:Album) => Text("Album " + in.name)
		case Full(in:Topic) => Text("Topic " + in.name)
		case Full(in:Foto) => Text("Foto " + in.name)
	}
	
	override def rewrite: LocRewrite = Full(NamedPF(name) {
		
		case RewriteRequest(ParsePath(
			"gallery" :: album :: topic :: foto :: Nil, _, _, _), _, _) ⇒
			(RewriteResponse("gallery" :: Nil, true), Foto(Topic(Album(album),topic),foto))
		
		case RewriteRequest(ParsePath(
			"gallery" :: album :: topic :: Nil, _, _, _), _, _) ⇒
			(RewriteResponse("gallery" :: Nil, true), Topic(Album(album),topic))
			
		case RewriteRequest(ParsePath(
			"gallery" :: album :: Nil, _, _, _), _, _) ⇒
			(RewriteResponse("gallery" :: Nil, true), Album(album))
			
	})
	
	override def calcTemplate = currentValue match {
		case Full(in:Album) =>	S.runTemplate("gallery" :: "album" :: Nil)
		case Full(in:Topic) =>	S.runTemplate("gallery" :: "topic" :: Nil)
		case Full(in:Foto) =>	S.runTemplate("gallery" :: "foto" :: Nil)
	}
	/*
	override def menu = currentValue match {
		 
		  * 
		case Full(in:Album) =>	???
		case Full(in:Topic) =>	???
		case Full(in:Foto) =>	  ???

		 * what should I put here to have same menu rendering and 
		 * nesting of Foto inside Topic inside Album via Menu.builder
		 * that I got from the AlbumMenu code posted previously ? 
		 
	}
	*/

}


