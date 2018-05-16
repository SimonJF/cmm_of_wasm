module type INT32MAP = (Map.S with type key = Int32.t)
module Int32Map = Map.Make(Int32)
