package a8.tools

import com.twelvemonkeys.imageio.plugins.psd.PSDMetadata

import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.{ImageIO, ImageReader}
import scala.jdk.CollectionConverters.*

@main def hello = {
  println("Tools for Atari 8-bit. Hello!")

  val inputFile = new File("/home/kuba/work/atari/a8clock/src/main/resources/AtariGr0Clock.psd")
//  val inputFile = new File("/home/kuba/work/atari/a8clock/src/main/resources/AtariGr0Clock_3.png")
  val imageInputStream = ImageIO.createImageInputStream(inputFile)
  val imageReaders = ImageIO.getImageReaders(imageInputStream)
  println(s"readers: $imageReaders")

  def alphaShade(color: Color): Option[Char] ={
    val alpha = color.getAlpha
    if(alpha<50) None
    else if( alpha <100) Some('.')
    else if( alpha <200) Some('x')
    else Some('X')
  }

  def exportImage(
                   img: BufferedImage,
                   outputVarName: String,
                   colorFunction:
                   (Color)=>Option[Char] ):Unit={
    case class ColorStep( steps: Int, color: Option[Color], rgb: Int)
    case class AsciiStep( steps: Int, c: Option[Char])

    val colorPoints = for {
      y <- (0 to (img.getHeight-1))
      x <- (0 to (img.getWidth-1))
      rgb = img.getRGB(x,y)

      cp = ColorStep(steps = 1, color = if(rgb == 0) None else Some(new Color(rgb, true)), rgb = rgb)
    } yield cp

    val asciiPoints = colorPoints
      .map( cp => AsciiStep(cp.steps, cp.color.flatMap( c => colorFunction(c) ) ) )
      .toList


    var apt = asciiPoints
    while(apt.nonEmpty){
      val line = apt.take(img.getWidth)
//      println( "; "+ line.map( ap => ap.c.getOrElse(' ') ).mkString(""))

      apt = apt.drop(img.getWidth)
    }

//    println(s"w:${img.getWidth}, h:${img.getHeight}")
//    println("Uncompressed: "+colorPoints.toList)


    val asciiPointsToProcess = asciiPoints.reverse

    val lastPoint = asciiPointsToProcess.head

    val allCompressed = asciiPointsToProcess.tail
      .foldLeft( List(lastPoint) )(
        (compressed, cp) => cp.c match {
          case Some(value) => compressed.prepended(cp)
          case None => {
            val h = compressed.head
            val t = compressed.tail
            t.prepended( h.copy( steps = h.steps + 1) )
          }

        } ).toList

    //println(allCompressed)
    //first step can be translated into  x/y of the first point

    val head = allCompressed.head
    val y = head.steps / img.getWidth
    val x = head.steps % img.getWidth
    println(s"; first point at: $x,$y")

    val steps = allCompressed
      .tail
      .dropRight(1)
      .flatMap( sp => List( sp.steps, sp.c.get.toInt - 32 ) )
    val gr0Compressed = List(x,y)
      .appendedAll(steps)


//    println(gr0Compressed.mkString(" "))

    print(s" BYTE ARRAY $outputVarName = [ ")
    val entriesPerLine = 10
    var allPoints = gr0Compressed
    while(allPoints.nonEmpty){
      val line = allPoints.take(entriesPerLine)
      println( line.mkString(" "))

      allPoints = allPoints.drop(entriesPerLine)
    }
    println(" 0 0 ]")



  }

  def showLayer(ir: ImageReader, r: Int): Unit ={

//    val irp = ir.getDefaultReadParam

    val imgBuff = ir.read(r)
    val streamMData = ir.getStreamMetadata
//    val imgMeta: PSDMetadata = ir.getImageMetadata(r).asInstanceOf[PSDMetadata]

    val imgPNames = imgBuff.getPropertyNames
//    println(s"; ImgBuff $r : $imgBuff")
//    println(s"ImgMeta $r : $imgMeta")
//    println(s"; streamMData $r : $streamMData")
    var idx = r - 3;
    var outputVarName = s"s$idx"
    exportImage(imgBuff, outputVarName, alphaShade)
  }

  def showImgData(ir: ImageReader):Unit={
    val imageNum = ir.getNumImages(false)
    println(s"ImageReader: $ir, $imageNum")
    val range = Range(ir.getMinIndex,imageNum)
    range.foreach( r => showLayer(ir, r))

  }

  while (imageReaders.hasNext) {
    val ir =  imageReaders.next()
    ir.setInput(imageInputStream)

    showImgData(ir)
  }


  val readers = ImageIO.getImageReadersByFormatName("JPEG").asScala.toList
  readers.foreach(r => println(s"Reader: $r"))
  val image = ImageIO.read( inputFile)
  println(s"Image: $image")
}
