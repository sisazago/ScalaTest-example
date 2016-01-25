package co.com.scalatest.generator

object TestCodeGenerator {
  
  val separator = "\t"
  
  case class Verificacion(test:String, id:String, detalle:String, detalleAdicional: String)
  
  def extraer(linea:String) = {
    linea.split(separator).toList match {
      case test :: id :: det :: det2  :: rest => 
        Some(Verificacion(test, id, det, det2))
      case test :: id :: det :: rest => 
        Some(Verificacion(test, id, det, ""))
      case test :: id :: rest =>
        Some(Verificacion(test, id, "",""))
      case _ =>
        None
    }
  }
}