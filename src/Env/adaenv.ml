
type env =
  { date: float }

let delay env d =
  assert (d >= 0.0) ;
  { date = env.date +. d }


