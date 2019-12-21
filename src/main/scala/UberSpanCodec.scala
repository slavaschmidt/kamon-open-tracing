package kamon.context

import java.net.{URLDecoder, URLEncoder}

import kamon.Kamon
import kamon.context.HttpPropagation.{HeaderReader, HeaderWriter}
import kamon.trace._
import kamon.trace.Trace.SamplingDecision

import scala.util.Try

// see https://www.jaegertracing.io/docs/1.7/client-libraries/#propagation-format for more details
object UberSpanCodec {
  def apply(): UberSpanCodec = new UberSpanCodec()
  val HeaderName = "uber-trace-id"
  val Separator = ":"
  val Default = "0"
  val DebugFlag = "d"
}

class UberSpanCodec extends Propagation.EntryReader[HeaderReader] with Propagation.EntryWriter[HeaderWriter] {
  import UberSpanCodec._

  override def write(context: Context, writer: HttpPropagation.HeaderWriter): Unit = {
    val span = context.get(Span.Key)

    if (span != Span.Empty) {
      val parentContext = if (span.parentId != Identifier.Empty) span.parentId.string else Default
      val sampling = encodeSamplingDecision(span.trace.samplingDecision)
      val debug: Byte = 0
      val flags = (sampling + (debug << 1)).toHexString
      val headerValue = Seq(span.trace.id.string, span.id.string, parentContext, flags).mkString(Separator)

      writer.write(HeaderName, urlEncode(headerValue))
    }

  }

  override def read(reader: HttpPropagation.HeaderReader, context: Context): Context = {
    val identifierScheme = Kamon.identifierScheme
    val header = reader.read(HeaderName)
    val headerParts = header.toList.map(urlDecode).flatMap(_.split(':'))
    val parts = headerParts ++ List.fill(4)("") // all parts are mandatory, but we want to be resilient

    val List(traceID, spanID, parentContext, flags) = parts.take(4)
    val trace = stringToId(identifierScheme, traceID)
    val span = stringToId(identifierScheme, spanID)

    if (trace != Identifier.Empty && span != Identifier.Empty) {
      val parent = stringToId(identifierScheme, parentContext)
      val samplingDecision = decodeSamplingDecision(flags)
      context.withEntry(Span.Key, Span.Remote(span, parent, Trace(trace, samplingDecision)))
    } else {
      context
    }
  }

  private def stringToId(identifierScheme: Identifier.Scheme, s: String) = {
    val str = if (s == null || s.isEmpty) None else Option(s)
    val id = str.map(urlDecode).map(identifierScheme.traceIdFactory.from)
    id.getOrElse(Identifier.Empty)
  }

  private def lowestBit(s: String) = Try(Integer.parseInt(s, 16) % 2).toOption

  private def decodeSamplingDecision(flags: String) =
    if (flags.equalsIgnoreCase(DebugFlag)) SamplingDecision.Sample
    else if (lowestBit(flags).contains(1)) SamplingDecision.Sample
    else if (lowestBit(flags).contains(0)) SamplingDecision.DoNotSample
    else SamplingDecision.Unknown

  private def encodeSamplingDecision(samplingDecision: SamplingDecision): Byte = samplingDecision match {
    case SamplingDecision.Sample      => 1
    case SamplingDecision.DoNotSample => 0
    case SamplingDecision.Unknown     => 1 // the sampling decision is mandatory in this format
  }


  private def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")
  private def urlDecode(s: String): String = URLDecoder.decode(s, "UTF-8")
}


