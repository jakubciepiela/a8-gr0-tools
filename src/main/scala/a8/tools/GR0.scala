package a8.tools

import com.twelvemonkeys.imageio.plugins.psd.PSDMetadata

import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.{ImageIO, ImageReader}
import scala.jdk.CollectionConverters.*

object GR0 {

  def hello = {
    println("Tools for Atari 8-bit. Hello!")

    val inputFile = new File("/home/kuba/work/atari/a8clock/src/main/resources/AtariGr0Clock.psd")
    //  val inputFile = new File("/home/kuba/work/atari/a8clock/src/main/resources/AtariGr0Clock_3.png")
    val imageInputStream = ImageIO.createImageInputStream(inputFile)
    val imageReaders = ImageIO.getImageReaders(imageInputStream)
    println(s"readers: $imageReaders")

    def alphaShade(color: Color): Option[Char] = {
      val alpha = color.getAlpha
      if (alpha < 50) None
      else if (alpha < 100) Some('.')
      else if (alpha < 200) Some('x')
      else Some('X')
    }

    def exportImage(
                     img: BufferedImage,
                     outputVarName: String,
                     colorFunction:
                     (Color) => Option[Char]): Unit = {
      case class ColorStep(steps: Int, color: Option[Color], rgb: Int)
      case class AsciiStep(steps: Int, c: Option[Char])

      val colorPoints = for {
        y <- (0 to (img.getHeight - 1))
        x <- (0 to (img.getWidth - 1))
        rgb = img.getRGB(x, y)

        cp = ColorStep(steps = 1, color = if (rgb == 0) None else Some(new Color(rgb, true)), rgb = rgb)
      } yield cp

      val asciiPoints = colorPoints
        .map(cp => AsciiStep(cp.steps, cp.color.flatMap(c => colorFunction(c))))
        .toList


      var apt = asciiPoints
      while (apt.nonEmpty) {
        val line = apt.take(img.getWidth)
        //      println( "; "+ line.map( ap => ap.c.getOrElse(' ') ).mkString(""))

        apt = apt.drop(img.getWidth)
      }

      //    println(s"w:${img.getWidth}, h:${img.getHeight}")
      //    println("Uncompressed: "+colorPoints.toList)


      val asciiPointsToProcess = asciiPoints.reverse

      val lastPoint = asciiPointsToProcess.head

      val allCompressed = asciiPointsToProcess.tail
        .foldLeft(List(lastPoint))(
          (compressed, cp) => cp.c match {
            case Some(value) => compressed.prepended(cp)
            case None => {
              val h = compressed.head
              val t = compressed.tail
              t.prepended(h.copy(steps = h.steps + 1))
            }

          }).toList

      //println(allCompressed)
      //first step can be translated into  x/y of the first point

      val head = allCompressed.head
      val y = head.steps / img.getWidth
      val x = head.steps % img.getWidth
      println(s"; first point at: $x,$y")

      val steps = allCompressed
        .tail
        .dropRight(1)
        .flatMap(sp => List(sp.steps, sp.c.get.toInt - 32))
      val gr0Compressed = List(x, y)
        .appendedAll(steps)


      //    println(gr0Compressed.mkString(" "))

      print(s" BYTE ARRAY $outputVarName = [ ")
      val entriesPerLine = 10
      var allPoints = gr0Compressed
      while (allPoints.nonEmpty) {
        val line = allPoints.take(entriesPerLine)
        println(line.mkString(" "))

        allPoints = allPoints.drop(entriesPerLine)
      }
      println(" 0 0 ]")


    }

    def showLayer(ir: ImageReader, r: Int): Unit = {

      //    val irp = ir.getDefaultReadParam

      val imgBuff = ir.read(r)
      val streamMData = ir.getStreamMetadata
      //    val imgMeta: PSDMetadata = ir.getImageMetadata(r).asInstanceOf[PSDMetadata]

      val imgPNames = imgBuff.getPropertyNames
      //    println(s"; ImgBuff $r : $imgBuff")
      //    println(s"ImgMeta $r : $imgMeta")
      //    println(s"; streamMData $r : $streamMData")
      var idx = r - 3;
      var outputVarName = s"m$idx"
      exportImage(imgBuff, outputVarName, alphaShade)
    }

    def showImgData(ir: ImageReader): Unit = {
      val imageNum = ir.getNumImages(false)
      println(s"ImageReader: $ir, $imageNum")
      val range = Range(ir.getMinIndex, imageNum)
      range.foreach(r => showLayer(ir, r))

    }

    while (imageReaders.hasNext) {
      val ir = imageReaders.next()
      ir.setInput(imageInputStream)

      showImgData(ir)
    }


    val readers = ImageIO.getImageReadersByFormatName("JPEG").asScala.toList
    readers.foreach(r => println(s"Reader: $r"))
    val image = ImageIO.read(inputFile)
    println(s"Image: $image")
  }

  case class GR8Options (
                        inputFileName: String = "",
                        outputFileName: String = "",
                        fromLayer: Option[Int] = None,
                        toLayer: Option[Int] = None
                        )

  def main(args: Array[String]): Unit = {
    import scopt.OParser
    val builder = OParser.builder[GR8Options]
    val parser1 = {
      import builder._
      OParser.sequence(
        programName("GR0"),
        head("GR0", "1.0.0"),
        note("Utility tool to covert psd formatting graphics into Atari GR0 ascii art"),
        note("Generated files are for use with Atari Action! GR0Face library"),
        opt[String]('i',  "input")
          .text("Input file name in psd format")
          .action( (x, c) => c.copy(inputFileName = x))
          .required(),
        opt[String]('o',  "output")
          .text("Output file name in txt format")
          .action( (x, c) => c.copy(outputFileName = x))
          .required(),
        opt[Int]('s',  "fromLayer")
          .text("first layer to include. indexing from 0. Default 0")
          .action( (x, c) => c.copy(fromLayer = Some(x)))
          .optional(),
        opt[Int]('t',  "toLayer")
          .text("last layer to include. indexing from 0. Default last layer")
          .action( (x, c) => c.copy(toLayer = Some(x)))
          .optional(),
        help("help").text("prints this usage text")
      )
    }

    OParser.parse(parser1, args, GR8Options()) match {
      case Some(config) =>
        println("Parameters: "+ config)
      case _ =>
      // arguments are bad, error message will have been displayed
    }
//    hello
  }
}