module type DateConverterType = sig
  exception InvalidDate of string

  val string_to_date : string -> float
  val date_to_string : float -> string
end

module DateConverter : DateConverterType
