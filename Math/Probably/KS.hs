module Math.Probably.KS where

--translated from from http://root.cern.ch/root/html/src/TMath.cxx.html#RDBIQ

fj = [-2,-8,-18,-32]

w = 2.50662827;
-- c1 - -pi**2/8, c2 = 9*c1, c3 = 25*c1
c1 = -1.2337005501361697;
c2 = -11.103304951225528;
c3 = -30.842513753404244;

oddNeg j | odd j = -1
         | otherwise = 1

kprob :: Double -> Double
kprob z = 
    let u = abs z in
    case u of
      _ | u <0.2    -> 1
        | u <0.755  -> let v = 1/(u*u) in
                       1 - w * (exp(c1*v)+exp(c3*v))/u
        | u <6.8116 -> let v = u*u
                           maxj = max 1 $ round (3/u)
                           rf j = if j<maxj
                                     then exp((fj!!j)*v)
                                     else 0
                       in (2*) . sum $ map rf [0..3]
        | otherwise -> 0
                                

