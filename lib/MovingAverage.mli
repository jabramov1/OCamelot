open CsvReader

module type MovingAverageType = sig
  val simple_moving_avg : CsvReader.t -> int -> float option list
  (** [simple_moving_avg data n] calculates the simple moving average (SMA) for
      the closing prices in a CSV data representation [data] over a window size
      [n].

      @see https://www.fmlabs.com/reference/default.htm SMA formula reference 
      (select from dropdown).
 
      @param data The CSV data representation.

      @param n The window size for calculating the moving average.

      @return
        A list of optional float values representing the SMAs calculated for
        each consecutive window in the dataset. If [n] is less than or equal to
        0, the empty list is returned. If [n] is greater than the number of
        closing prices, then the [n] will be reassigned to a value equal to the
        number of closing prices.

        If all the values in a window are [None], the SMA for that window will
        also be [None]. If some of the values in a window are [None], then only
        the ones equivalent to [Some x] will be included in the average. *)

  val exp_moving_avg : CsvReader.t -> int -> float option list
  (** [exp_moving_avg data n] calculates the exponential moving average (EMA)
      for the closing prices in a CSV data representation [data] over a window
      size [n].

      @see https://www.fmlabs.com/reference/default.htm EMA formula reference 
      (select from dropdown).

      @param data The CSV data representation.

      @param n The window size for calculating the moving average.

      @return
        A list of optional float values representing the EMA calculated for each
        consecutive windows in the dataset. If [n] is less than or equal to 0,
        the empty list is returned. If [n] is greater than the number of closing
        prices, then the [n] will be reassigned to a value equal to the number
        of closing prices.

        If all the values in a window are [None], the EMA for that window will
        also be [None]. If some of the values in a window are [None], then only
        the ones equivalent to [Some x] will be included in the average. *)

  val weighted_moving_avg : CsvReader.t -> int -> float option list
  (** [weighted_moving_avg data n] calculates the weighted moving average (WMA)
      for the closing prices in a CSV data representation [data] over a window
      size [n].

      @see https://www.fmlabs.com/reference/default.htm WMA formula reference 
      (select from dropdown).

      @param data The CSV data representation.

      @param n The window size for calculating the moving average.

      @return
        A list of optional float values representing the WMA calculated for each
        consecutive window in the dataset. If [n] is less than or equal to 0,
        the empty list is returned. If [n] is greater than the number of closing
        prices, then the [n] will be reassigned to a value equal to the number
        of closing prices.

        If all the values in a window are [None], the WMA for that window will
        also be [None]. If some of the values in a window are [None], then only
        the ones equivalent to [Some x] will be included in the average. *)

  val triangular_moving_avg : CsvReader.t -> int -> float option list
  (** [triangular_moving_avg data n] calculates the triangular moving average
      (TMA) for the closing prices in a CSV data representation [data] over a
      window size [n].

      @see https://www.fmlabs.com/reference/default.htm TMA formula reference 
      (select from dropdown). 

      @param data The CSV data representation.

      @param n The window size for calculating the moving average.

      @return
        A list of optional float values representing the TMA calculated for each
        consecutive window in the dataset. If [n] is less than or equal to 0,
        the empty list is returned. If [n] is greater than the number of closing
        prices, then the [n] will be reassigned to a value equal to the number
        of closing prices.

        If all the values in a window are [None], the TMA for that window will
        also be [None]. If some of the values in a window are [None], then only
        the ones equivalent to [Some x] will be included in the average. *)

  val vol_adj_moving_avg : CsvReader.t -> int -> float option list
  (** [vol_adj_moving_avg data n] calculates the volume-adjusted moving average
      (VAMA) for the closing prices in a CSV data representation [data] over a
      window size [n].

      @see https://www.fmlabs.com/reference/default.htm VAMA formula reference 
      (select from dropdown).

      @param data The CSV data representation.

      @param n The window size for calculating the moving average.

      @return
        A list of optional float values representing the VAMA calculated for
        each consecutive window in the dataset. If [n] is less than or equal to
        0, the empty list is returned. If [n] is greater than the number of
        closing prices, then the [n] will be reassigned to a value equal to the
        number of closing prices.

        If all the values in a window are [None], the VAMA for that window will
        also be [None]. If some of the values in a window are [None], then only
        the ones equivalent to [Some x] will be included in the average. *)
end

module MovingAverage : MovingAverageType
(** A module that calculates various moving averages listed above. *)
