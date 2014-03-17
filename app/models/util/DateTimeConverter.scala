package models.util

import javax.persistence.{Converter, AttributeConverter}
import org.joda.time.DateTime
import java.sql.Timestamp

@Converter
class DateTimeConverter extends AttributeConverter[DateTime, Timestamp] {

  override def convertToEntityAttribute(ts: Timestamp): DateTime = {
    if(ts != null) { new DateTime(ts.getTime) } else { null }
  }

  override def convertToDatabaseColumn(dt: DateTime): Timestamp = {
    if (dt != null) { new Timestamp(dt.getMillis) } else { null }
  }
}
