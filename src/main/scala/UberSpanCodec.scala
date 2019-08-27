import java.net.{URLDecoder, URLEncoder}

import kamon.Kamon
import kamon.context._
import kamon.context.HttpPropagation.{HeaderReader, HeaderWriter}
import kamon.trace._
import kamon.trace.Trace.SamplingDecision

// see https://www.jaegertracing.io/docs/1.7/client-libraries/#propagation-format for more details
object UberSpanCodec {
  def apply(): UberSpanCodec = new UberSpanCodec()
  val HeaderName = "uber-trace-id"
}

class UberSpanCodec extends Propagation.EntryReader[HeaderReader] with Propagation.EntryWriter[HeaderWriter] {
  import UberSpanCodec.HeaderName

  override def write(context: Context, writer: HttpPropagation.HeaderWriter): Unit = {
    val span = context.get(Span.Key)

    if (span != Span.Empty) {
      val parentContext = if (span.parentId != Identifier.Empty) span.parentId.string else "0"
      val sampling = encodeSamplingDecision(span.trace.samplingDecision)
      val debug: Byte = 0
      val flags = (sampling + (debug << 1)).toHexString
      val headerValue = s"${span.trace.id.string}:${span.id.string}:$parentContext:$flags"

      writer.write(HeaderName, urlEncode(headerValue))
    }

  }

  override def read(reader: HttpPropagation.HeaderReader, context: Context): Context = {
    val identifierScheme = Kamon.identifierScheme
    val header = reader.read(HeaderName)
    val parts = header.toSeq.flatMap(_.split(':')).toList

    parts match {
      case traceID :: spanID :: parentContext :: tail =>
        val trace = string2Identifier(traceID).map(id => identifierScheme.traceIdFactory.from(urlDecode(id)))

        val span = string2Identifier(spanID).map(id => identifierScheme.traceIdFactory.from(urlDecode(id)))

        if (trace.isDefined && span.isDefined) {
          val parent = string2Identifier(parentContext).map(id => identifierScheme.traceIdFactory.from(urlDecode(id)))

          val samplingDecision = tail match {
            case List(f) if f.nonEmpty && f.equalsIgnoreCase("d") => SamplingDecision.Sample
            case List(f) if f.nonEmpty && Integer.parseInt(f, 16) % 2 == 1 => SamplingDecision.Sample
            case List(f) if f.nonEmpty && Integer.parseInt(f, 16) % 2 == 0 => SamplingDecision.DoNotSample
            case _ => SamplingDecision.Unknown
          }
          context.withEntry(Span.Key, Span.Remote(get(span), get(parent), Trace(get(trace), samplingDecision)))
        } else context
      case _ => context
    }
  }

  private def get(s: Option[Identifier]): Identifier = s.getOrElse(Identifier.Empty)

  private def string2Identifier(s: String) = if (s == null || s.isEmpty) None else Option(s)

  private def encodeSamplingDecision(samplingDecision: SamplingDecision): Byte = samplingDecision match {
    case SamplingDecision.Sample      => 1
    case SamplingDecision.DoNotSample => 0
    case SamplingDecision.Unknown     => 1
  }

  private def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")
  private def urlDecode(s: String): String = URLDecoder.decode(s, "UTF-8")
}


