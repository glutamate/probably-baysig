{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, ImplicitParams, PackageImports, ViewPatterns #-}
module Target.Heston where

import Target.Prelude
import qualified Data.Record as R
import Prelude hiding (scanl,repeat,replicate)
import Data.Record.Combinators ((!!!))
import Data.Kind
import Data.List (transpose)
import Data.TypeFun
import Numeric.LinearAlgebra hiding (diag, linspace, svd, )
import Math.Probably.Sampler hiding (uniform,primOneOf,logNormal,invGamma,binomial,gamma,oneOf,bernoulli, normal, unit, unormal,multiNormal)
import qualified Data.Text as T
import Foreign.Storable (Storable)
import Data.STRef
import Control.Monad.ST
import Control.Monad (forM, forM_)
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Storable as VS
import Foreign.Storable.Tuple

(.==.) :: (Eq a) => (a -> ((a -> Double)))
(.==.) = \((x::a)) -> \((y::a)) -> if (x==y) then 1.000 else 0.000

scanl :: (((a -> ((b -> a)))) -> ((a -> ((([b]) -> ([a]))))))
scanl = \(((_arg0)::(a -> ((b -> a))))) -> \((_arg1)) -> \(((_arg2)::[b])) -> case (_arg0,((_arg1,_arg2))) of {((op::(a -> ((b -> a)))),(acc,[])) -> acc:[]; ((op::(a -> ((b -> a)))),(acc,(x:(xs::[b])))) -> acc:((scanl op (op acc x)) xs)}

fromTo :: (BayNum a) => (a -> ((a -> ([a]))))
fromTo = \((n::a)) -> \((m::a)) -> if (n<m) then (n:(fromTo (n+1) m)) else (n:[])

bigSum :: (Int -> ((Int -> ((((Int -> Double)) -> Double)))))
bigSum = \((lo::Int)) -> \((hi::Int)) -> \((f::(Int -> Double))) -> sum (map f (fromTo lo hi))

replicate :: (Int -> ((a -> ([a]))))
replicate = \(((_arg0)::Int)) -> \((_arg1)) -> case (_arg0,_arg1) of {(0,x) -> []; ((n::Int),x) -> x:(replicate (n-1) x)}

neg :: (BayNum a) => (a -> a)
neg = \((x::a)) -> 0-x

ix :: (Int -> ((([a]) -> a)))
ix = \(((_arg0)::Int)) -> \(((_arg1)::[a])) -> case (_arg0,_arg1) of {(0,(x:_)) -> x; ((n::Int),(_:(xs::[a]))) -> ix (n-1) xs}

linspace :: (Double -> ((Double -> ((Double -> ([Double]))))))
linspace = \((from::Double)) -> \((to::Double)) -> \((num::Double)) -> let {(dt::Double) = (to-from)/num;
 (is::[Double]) = fromTo 0 (num-1);
 } in map (\((i::Double)) -> ((unround i)*dt)+from) is

countSamples :: ((Prob a) -> (Maybe Int))
countSamples = \(((_arg0)::Prob a)) -> case _arg0 of {Samples (xs::[a]) -> Just (length xs); Sampler _ -> Nothing}

unit :: Prob Double
unit = Sampler primUnit

primOneOf :: (([a]) -> ((Seed -> ((a,Seed)))))
primOneOf = \((xs::[a])) -> \((seed::Seed)) -> let {((u::Double),(nextSeed::Seed)) = primUnit seed;
 (idx::Int) = floor (u*(unround (length xs)));
 } in ((ix idx xs),nextSeed)

append :: (([a]) -> ((([a]) -> ([a]))))
append = \(((_arg0)::[a])) -> \(((_arg1)::[a])) -> case (_arg0,_arg1) of {([],(ys::[a])) -> ys; ((x:(xs::[a])),(ys::[a])) -> x:(append xs ys)}

invlogit :: (Double -> Double)
invlogit = \((x::Double)) -> 1/(1+(exp (0.000-x)))

logit :: (Double -> Double)
logit = \((x::Double)) -> log (x/(1-x))

boolToReal :: (Bool -> Double)
boolToReal = \(((_arg0)::Bool)) -> case _arg0 of {True  -> 1.000; False  -> 0.000}

for :: (Int -> ((Int -> ((((Int -> (Prob a))) -> (Prob ([a])))))))
for = \((n::Int)) -> \((m::Int)) -> \((s::(Int -> (Prob a)))) -> if (n<m) then ((s n)>>=(\(x) -> ((for (n+1) m) s)>>=(\((xs::[a])) -> return (x:xs)))) else ((s n)>>=(\(v) -> return (v:[])))

repeat :: (Int -> (((Prob a) -> (Prob ([a])))))
repeat = \((n::Int)) -> \((sam::Prob a)) -> (for 1 n) (\((i::Int)) -> sam)

square :: (BayNum a) => (a -> a)
square = \((x::a)) -> x*x

step :: (BayNum a,BayNum b) => (a -> b)
step = \((x::a)) -> if (x<0) then 0 else 1

fac :: (Int -> Int)
fac = \(((_arg0)::Int)) -> case _arg0 of {1 -> 1; (n::Int) -> n*(fac (n-1))}

zipWithNats :: (BayNum b) => (([a]) -> ((b -> ([(b,a)]))))
zipWithNats = \(((_arg0)::[a])) -> \(((_arg1)::b)) -> case (_arg0,_arg1) of {([],_) -> []; ((x:(xs::[a])),(n::b)) -> ((n,x)):(zipWithNats xs (n+1))}

unSamples :: ((Prob a) -> ([a]))
unSamples = \(Samples (xs::[a])) -> xs

chainPlot :: ((Prob Double) -> Plot)
chainPlot = \(Samples (xs::[Double])) -> Plot [] (return ((Points (zipWithNats xs 0)):[]))

style :: (([(T.Text,T.Text)]) -> ((Plot -> Plot)))
style = \((opts::[(T.Text,T.Text)])) -> \(Plot (pos::[(T.Text,T.Text)]) (plr::Prob ([Radian]))) -> Plot pos (fmap (\((lrs::[Radian])) -> (Options opts lrs):[]) plr)

distPlot0 :: ((Prob Double) -> Plot)
distPlot0 = \(((_arg0)::Prob Double)) -> case _arg0 of {Samples (xs::[Double]) -> Plot ((((T.pack "range-y"),(T.pack "0"))):[]) (return ((Histogram xs):[])); (sampler::Prob Double) -> Plot ((((T.pack "range-y"),(T.pack "0"))):[]) ((repeat 2000 sampler)>>=(\((xs::[Double])) -> return ((Histogram xs):[])))}

distPlot :: ((Prob Double) -> Plot)
distPlot = \((p::Prob Double)) -> style ((((T.pack "fill"),(T.pack "pop_colour"))):((((T.pack "fill-opacity"),(T.pack "0.3"))):((((T.pack "stroke"),(T.pack "none"))):((((T.pack "bar-width"),(T.pack "0.8"))):[])))) (distPlot0 p)

histogram :: (BayNum a) => (([a]) -> Plot)
histogram = \((xs::[a])) -> Plot [] (return ((Options ((((T.pack "fill"),(T.pack "pop_colour"))):((((T.pack "fill-opacity"),(T.pack "0.3"))):((((T.pack "stroke"),(T.pack "none"))):((((T.pack "bar-width"),(T.pack "0.8"))):[])))) ((Histogram (map unround xs)):[])):[]))

unPlot :: (Plot -> (Prob ([Radian])))
unPlot = \(Plot _ (x::Prob ([Radian]))) -> x

over :: (([Plot]) -> Plot)
over = \((plots::[Plot])) -> Plot [] ((mapM unPlot plots)>>=(\((items::[[Radian]])) -> return (map (\((Plot (os::[(T.Text,T.Text)]) _,(rdns::[Radian]))) -> Options os rdns) (zip plots items))))

scatterPlot :: (BayNum a,BayNum b) => (([(a,b)]) -> Plot)
scatterPlot = \((xys::[(a,b)])) -> style ((((T.pack "fill"),(T.pack "pop_colour"))):((((T.pack "marker-size"),(T.pack "30"))):((((T.pack "marker"),(T.pack "circle"))):((((T.pack "stroke"),(T.pack "none"))):[])))) (Plot [] (return ((Points (map (\(((x::a),(y::b))) -> ((unround x),(unround y))) xys)):[])))

linePlot :: (BayNum a,BayNum b) => (([(a,b)]) -> Plot)
linePlot = \((xys::[(a,b)])) -> style ((((T.pack "stroke"),(T.pack "pop_colour"))):((((T.pack "stroke-width"),(T.pack "2"))):[])) (Plot [] (return ((Lines (map (\(((x::a),(y::b))) -> ((unround x),(unround y))) xys)):[])))

plines :: (BayNum a,BayNum b) => ((Prob ([(a,b)])) -> Plot)
plines = \((plns::Prob ([(a,b)]))) -> style ((((T.pack "stroke-opacity"),(T.pack "0.2"))):((((T.pack "stroke-width"),(T.pack "2"))):[])) (Plot [] (repeat 50 (fmap (\((xys::[(a,b)])) -> Lines (map (\(((x::a),(y::b))) -> ((unround x),(unround y))) xys)) plns)))

ppoints :: ((Prob ([(Double,Double)])) -> Plot)
ppoints = \((ppts::Prob ([(Double,Double)]))) -> Plot ((((T.pack "fill"),(T.pack "pop_colour"))):[]) (repeat 50 (fmap Points ppts))

sigPlot :: (((Double -> Double)) -> Plot)
sigPlot = \((sig::(Double -> Double))) -> Plot ((((T.pack "fill"),(T.pack "pop_colour"))):((((T.pack "stroke-width"),(T.pack "2"))):[])) (return ((Timeseries sig):[]))

thin :: (Int -> ((([a]) -> ([a]))))
thin = \((skip::Int)) -> \((xs::[a])) -> map snd (filter (\(((i::Int),x)) -> (mod i (skip+1))==0) (zip (fromTo 0 ((length xs)-1)) xs))

thinTo :: (Int -> ((([a]) -> ([a]))))
thinTo = \((n::Int)) -> \((xs::[a])) -> let {(nxs::Int) = length xs;
 (ratio::Int) = round ((unround nxs)/(unround n));
 } in thin ratio xs

psigPlot :: ((Prob ((Double -> Double))) -> Plot)
psigPlot = \(((_arg0)::Prob ((Double -> Double)))) -> case _arg0 of {Samples (sigs::[(Double -> Double)]) -> Plot ((((T.pack "fill"),(T.pack "pop_colour"))):((((T.pack "stroke-opacity"),(T.pack "0.2"))):((((T.pack "stroke-width"),(T.pack "2"))):[]))) (return (map Timeseries (thinTo 20 sigs))); Sampler (f::(Seed -> ((((Double -> Double)),Seed)))) -> Plot ((((T.pack "fill"),(T.pack "pop_colour"))):((((T.pack "stroke-opacity"),(T.pack "0.2"))):((((T.pack "stroke-width"),(T.pack "2"))):[]))) ((repeat 20 (Sampler f))>>=(\((sigs::[(Double -> Double)])) -> return (map Timeseries sigs)))}

psigNPlot :: (Int -> (((Prob ((Double -> Double))) -> Plot)))
psigNPlot = \(((_arg0)::Int)) -> \(((_arg1)::Prob ((Double -> Double)))) -> case (_arg0,_arg1) of {((n::Int),Samples (sigs::[(Double -> Double)])) -> Plot ((((T.pack "fill"),(T.pack "pop_colour"))):((((T.pack "stroke-opacity"),(T.pack "0.2"))):((((T.pack "stroke-width"),(T.pack "2"))):[]))) (return (map Timeseries (thinTo n sigs))); ((n::Int),Sampler (f::(Seed -> ((((Double -> Double)),Seed))))) -> Plot ((((T.pack "fill"),(T.pack "pop_colour"))):((((T.pack "stroke-opacity"),(T.pack "0.1"))):((((T.pack "stroke-width"),(T.pack "2"))):[]))) ((repeat n (Sampler f))>>=(\((sigs::[(Double -> Double)])) -> return (map Timeseries sigs)))}

probBoolToP :: ((Prob Bool) -> (Prob Double))
probBoolToP = \(((_arg0)::Prob Bool)) -> case _arg0 of {Sampler (f::(Seed -> ((Bool,Seed)))) -> (repeat 200 (Sampler f))>>=(\((bs::[Bool])) -> probBoolToP (Samples bs)); Samples (bs::[Bool]) -> let {(yeas::[Bool]) = filter id bs;
 } in return ((unround (length yeas))/(unround (length bs)))}

pcurve :: (([(Double,(Prob Bool))]) -> Plot)
pcurve = \((xps::[(Double,(Prob Bool))])) -> Plot [] (let {(xs::[Double]) = map fst xps;
 } in (mapM (probBoolToP.snd) xps)>>=(\((ps::[Double])) -> return ((Lines (zip xs ps)):[])))

plotStyle :: (([(T.Text,T.Text)]) -> ((Plot -> Plot)))
plotStyle = \(((_arg0)::[(T.Text,T.Text)])) -> \(((_arg1)::Plot)) -> case (_arg0,_arg1) of {((opts::[(T.Text,T.Text)]),Plot (pos::[(T.Text,T.Text)]) (plr::Prob ([Radian]))) -> Plot (append opts pos) plr; ((opts::[(T.Text,T.Text)]),PlotRow (pos::[(T.Text,T.Text)]) (plr::[Plot])) -> PlotRow (append opts pos) plr; ((opts::[(T.Text,T.Text)]),PlotColumn (pos::[(T.Text,T.Text)]) (plr::[Plot])) -> PlotColumn (append opts pos) plr; ((opts::[(T.Text,T.Text)]),PlotStack (pos::[(T.Text,T.Text)]) (plr::[(T.Text,Plot)])) -> PlotStack (append opts pos) plr; ((opts::[(T.Text,T.Text)]),PlotGrid (pos::[(T.Text,T.Text)]) (x::Int) (y::Int) (plr::[(T.Text,Plot)])) -> ((PlotGrid (append opts pos) x) y) plr}

wide :: (Plot -> Plot)
wide = plotStyle ((((T.pack "width"),(T.pack "750"))):[])

aspect :: (Double -> ((Plot -> Plot)))
aspect = \((x::Double)) -> plotStyle ((((T.pack "aspect"),(showReal x))):[])

besides :: (([Plot]) -> Plot)
besides = PlotRow []

above :: (([Plot]) -> Plot)
above = PlotColumn []

axisLabels :: (T.Text -> ((T.Text -> ((Plot -> Plot)))))
axisLabels = \((xlab::T.Text)) -> \((ylab::T.Text)) -> \(Plot (popts::[(T.Text,T.Text)]) (plns::Prob ([Radian]))) -> Plot ((((T.pack "axis-x-label"),xlab)):((((T.pack "axis-y-label"),ylab)):popts)) plns

unPlotOpts :: (Plot -> ([(T.Text,T.Text)]))
unPlotOpts = \(Plot (os::[(T.Text,T.Text)]) (x::Prob ([Radian]))) -> os

sigLast :: (((Double -> Double)) -> Double)
sigLast = \((sig::(Double -> Double))) -> case observeSig sig of {ObservedSignal (dt::Double) (t0::Double) (pts::Vector Double) -> pts@>((dim pts)-1); ObservedXYSignal (pts::Vector ((Double,Double))) -> snd (pts@>((dim pts)-1))}

sigTail :: (((Double -> Double)) -> ([Double]))
sigTail = \((sig::(Double -> Double))) -> case observeSig sig of {ObservedSignal (dt::Double) (t0::Double) (pts::Vector Double) -> vecToList pts}

sigNPts :: (((Double -> Double)) -> Int)
sigNPts = \((sig::(Double -> Double))) -> case observeSig sig of {ObservedSignal (dt::Double) (t0::Double) (pts::Vector Double) -> dim pts; ObservedXYSignal (pts::Vector ((Double,Double))) -> dim pts}

between :: (BayNum a) => (a -> ((a -> ((a -> Bool)))))
between = \((lo::a)) -> \((hi::a)) -> \((x::a)) -> (x>lo)&&(x<hi)

runP :: (a -> ((((a -> ((c,d)))) -> c)))
runP = \(xs) -> \((my::(a -> ((c,d))))) -> fst (my xs)

runP1 :: (((a -> ((b,c)))) -> ((a -> b)))
runP1 = \((my::(a -> ((b,c))))) -> \(xs) -> fst (my xs)

returnP :: (a -> ((b -> ((a,b)))))
returnP = \(x) -> \(xs) -> (x,xs)

bindP :: (((a -> ((b,c)))) -> ((((b -> ((c -> f)))) -> ((a -> f)))))
bindP = \((f::(a -> ((b,c))))) -> \((g::(b -> ((c -> f))))) -> \(xs) -> let {(x,xs') = f xs;
 } in g x xs'

headP :: (([a]) -> ((a,([a]))))
headP = \(((_arg0)::[a])) -> case _arg0 of {(x:(xs::[a])) -> (x,xs); [] -> bayError (T.pack "headP: empty list")}

takeP :: (Int -> ((([a]) -> ((([a]),([a]))))))
takeP = \((n::Int)) -> \((xs::[a])) -> ((take n xs),(drop n xs))

forP :: (([a]) -> ((((a -> ((c -> ((d,c)))))) -> ((c -> ((([d]),c)))))))
forP = \(((_arg0)::[a])) -> \(((_arg1)::(a -> ((c -> ((d,c))))))) -> \((_arg2)) -> case (_arg0,((_arg1,_arg2))) of {([],((f::(a -> ((c -> ((d,c)))))),xs)) -> ([],xs); ((a:(as::[a])),((f::(a -> ((c -> ((d,c)))))),xs)) -> (bindP (f a) (\(y) -> bindP (forP as f) (\((ys::[d])) -> returnP (y:ys)))) xs}

noP :: (a -> (((),a)))
noP = \(xs) -> ((),xs)

fmapP :: (((a -> b)) -> ((((c -> ((a,e)))) -> ((c -> ((b,e)))))))
fmapP = \((f::(a -> b))) -> \((mx::(c -> ((a,e))))) -> bindP mx (\(x) -> returnP (f x))

fixP :: (Int -> (((Prob a) -> (Prob (Prob a)))))
fixP = \(((_arg0)::Int)) -> \(((_arg1)::Prob a)) -> case (_arg0,_arg1) of {((n::Int),Sampler (f::(Seed -> ((a,Seed))))) -> (repeat n (Sampler f))>>=(\((xs::[a])) -> return (Samples xs)); ((n::Int),Samples (xs::[a])) -> return (Samples (thinTo n xs))}

diag :: ((Vector Double) -> (Matrix Double))
diag = \((v::Vector Double)) -> fillM (((dim v),(dim v))) (\(((i::Int),(j::Int))) -> if (i==j) then (v@>i) else 0.000)

diagL :: (([Double]) -> (Matrix Double))
diagL = diag.listToVec

mmap :: (BayNum a,BayNum b) => (((a -> b)) -> (((Matrix a) -> (Matrix b))))
mmap = \((f::(a -> b))) -> \((m::Matrix a)) -> fillM (mdims m) (\(((i::Int),(j::Int))) -> f (m@@>((i,j))))

transM :: (BayNum a) => ((Matrix a) -> (Matrix a))
transM = \((m::Matrix a)) -> fillM (mdims m) (\(((i::Int),(j::Int))) -> m@@>((j,i)))

ident :: (BayNum a) => (a -> (Matrix Double))
ident = \((n::a)) -> diagL (map (const 1) (fromTo 1 n))

meanL :: (([Double]) -> Double)
meanL = \((xs::[Double])) -> (sum xs)/(unround (length xs))

varL :: (([Double]) -> Double)
varL = \((xs::[Double])) -> let {(mu::Double) = meanL xs;
 } in (sum (map (\((x::Double)) -> (x-mu)*(x-mu)) xs))/(unround ((length xs)-1))

popvarL :: (([Double]) -> Double)
popvarL = \((xs::[Double])) -> let {(mu::Double) = meanL xs;
 } in (sum (map (\((x::Double)) -> (x-mu)*(x-mu)) xs))/(unround (length xs))

minL :: (([Double]) -> Double)
minL = \(((x::Double):(xs::[Double]))) -> (foldl min x) xs

maxL :: (([Double]) -> Double)
maxL = \(((x::Double):(xs::[Double]))) -> (foldl max x) xs

getT :: ((a) -> (((b) -> ((a,b)))))
getT = \(dt) -> \(tmax) -> (dt,tmax)

pearson :: (([(Double,Double)]) -> Double)
pearson = \((xys::[(Double,Double)])) -> let {(xs::[Double]) = map fst xys;
 (ys::[Double]) = map snd xys;
 (mx::Double) = meanL xs;
 (my::Double) = meanL ys;
 (sx::Double) = sqrt (varL xs);
 (sy::Double) = sqrt (varL ys);
 (invN::Double) = 1.000/(unround ((length xys)-1));
 } in invN*(sum (map (\(((x::Double),(y::Double))) -> ((x-mx)/sx)*((y-my)/sy)) xys))

data Strategy a = 
   GStrategy (((((Vector Double) -> ((Double,(Vector Double))))) -> (((Vector Double) -> ((a -> ((((Double,(Vector Double))) -> (Prob (((((Vector Double),a)),((Double,(Vector Double)))))))))))))) (((Vector Double) -> a))
   |VStrategy (((((Vector Double) -> Double)) -> (((Vector Double) -> ((a -> ((Double -> (Prob (((((Vector Double),a)),Double))))))))))) (((Vector Double) -> a))
intBetweenLogPdf :: (BayNum d) => (a -> ((b -> ((c -> d)))))
intBetweenLogPdf = \(lo) -> \(hi) -> \(x) -> 1

anyLogPdf :: (a -> Double)
anyLogPdf = \(x) -> 1.000

d :: ((Double) -> ((((Double -> Double)) -> ((Double -> Double)))))
d = \((dt::Double)) -> \((w::(Double -> Double))) -> \((t::Double)) -> ((w t)-(w (t-dt)))/dt

unormal :: Prob Double
unormal = unit>>=(\((u1::Double)) -> unit>>=(\((u2::Double)) -> return ((sqrt ((0.000-2.000)*(log u1)))*(cos ((2.000*pi)*u2)))))

gammaAux :: (Double -> ((Double -> (Prob Double))))
gammaAux = \((a::Double)) -> \((b::Double)) -> let {(d::Double) = a-(1.000/3.000);
 (c::Double) = 1.000/(3.000*(sqrt d));
 } in unormal>>=(\((x::Double)) -> let {(cx::Double) = c*x;
 (v::Double) = (1.000+cx)**3.000;
 (x_2::Double) = x*x;
 (x_4::Double) = x_2*x_2;
 } in if (cx<(-1.000)) then (gammaAux a b) else (unit>>=(\((u::Double)) -> if ((u<(1.000-(3.310e-2*x_4)))||((log u)<((0.500*x_2)+(d*((1.000-v)+(log v)))))) then (return ((b*d)*v)) else (gammaAux a b))))

gamma :: (Double -> ((Double -> (Prob Double))))
gamma = \((k::Double)) -> \((theta::Double)) -> if (k<1.000) then (unit>>=(\((u::Double)) -> (gamma (1.000+k) theta)>>=(\((x::Double)) -> return (x*(u**(1.000/k)))))) else (gammaAux k theta)

improper_uniform :: Prob Double
improper_uniform = gamma 1 0.100

improper_uniformLogPdf :: (a -> Double)
improper_uniformLogPdf = \(_) -> 1.000

improper_uniform_positive :: Prob Double
improper_uniform_positive = gamma 1 1

unfoldN :: (Int -> ((Int -> ((a -> ((((Int -> ((a -> (Prob a))))) -> (Prob ([a])))))))))
unfoldN = \((n::Int)) -> \((m::Int)) -> \(lastx) -> \((s::(Int -> ((a -> (Prob a)))))) -> if (n<m) then ((s n lastx)>>=(\(x) -> (((unfoldN (n+1) m) x) s)>>=(\((xs::[a])) -> return (x:xs)))) else ((s n lastx)>>=(\(v) -> return (v:[])))

unfold :: (Int -> ((a -> ((((a -> (Prob a))) -> (Prob ([a])))))))
unfold = \((n::Int)) -> \(x0) -> \((s::(a -> (Prob a)))) -> ((unfoldN 1 n) x0) (\((i::Int)) -> s)

improper_uniformInit :: Double
improper_uniformInit = 1.000

ser :: Double
ser = 1.000

normal :: (Double -> ((Double -> (Prob Double))))
normal = \((mean::Double)) -> \((variance::Double)) -> unormal>>=(\((u::Double)) -> return ((u*(sqrt variance))+mean))

rwmTrans :: (BayNum a) => ((((Vector Double) -> Double)) -> (((Vector Double) -> ((((Double,((Double,a)))) -> ((Double -> (Prob (((((Vector Double),((Double,((Double,a)))))),Double))))))))))
rwmTrans = \((posterior::((Vector Double) -> Double))) -> \((xi::Vector Double)) -> \(((sigma::Double),((i::Double),(iaccept::a)))) -> \((pi::Double)) -> (fmap listToVec (mapM (\((x::Double)) -> normal x sigma) (vecToList xi)))>>=(\((xstar::Vector Double)) -> let {(pstar::Double) = posterior xstar;
 (ratio::Double) = exp (pstar-pi);
 } in unit>>=(\((u::Double)) -> let {(accept::Bool) = u<ratio;
 (sigmaNext::Double) = if accept then (sigma*((min 1.400 (1+(5.000/i)))**3)) else (sigma*(max 0.714 (1-(5.000/i))));
 } in return (if accept then ((((xstar,((sigmaNext,(((i+1),(iaccept+1))))))),pstar)) else ((((xi,((sigmaNext,(((i+1),iaccept)))))),pi)))))

rwmIni :: (a -> ((Double,((Double,Double)))))
rwmIni = \(_) -> (0.100,((1.000,0.000)))

rwm :: Strategy ((Double,((Double,Double))))
rwm = VStrategy rwmTrans rwmIni

malaTrans :: ((((Vector Double) -> ((Double,a)))) -> (((Vector Double) -> ((Double -> ((((Double,a)) -> (Prob (((((Vector Double),Double)),((Double,a))))))))))))
malaTrans = \((postgrad::((Vector Double) -> ((Double,a))))) -> \((xi::Vector Double)) -> \((sigma::Double)) -> \(((pi::Double),gradienti)) -> let {(xstarMean::Vector Double) = xi;
 } in (fmap listToVec (mapM (\((x::Double)) -> normal x sigma) (vecToList xi)))>>=(\((xstar::Vector Double)) -> let {((pstar::Double),gradientStar) = postgrad xstar;
 (ratio::Double) = exp (pstar-pi);
 } in unit>>=(\((u::Double)) -> let {(accept::Bool) = u<ratio;
 (sigmaNext::Double) = sigma;
 } in return (if accept then ((((xstar,sigmaNext)),((pstar,gradientStar)))) else ((((xi,sigmaNext)),((pi,gradienti)))))))

malaIni :: (BayNum b) => (a -> b)
malaIni = \(vini) -> 1

mala :: Strategy Double
mala = GStrategy malaTrans malaIni

uniform :: (Double -> ((Double -> (Prob Double))))
uniform = \((lo::Double)) -> \((hi::Double)) -> unit>>=(\((x::Double)) -> return ((x*(hi-lo))+lo))

oneOf :: (([a]) -> (Prob a))
oneOf = \((xs::[a])) -> (fmap floor (uniform 0.000 (unround (length xs))))>>=(\((idx::Int)) -> return (ix idx xs))

oneOfLogPdf :: (([a]) -> ((b -> Double)))
oneOfLogPdf = \((xs::[a])) -> \(_) -> 1.000/(unround (length xs))

uniformLogPdf :: (Double -> ((Double -> ((Double -> Double)))))
uniformLogPdf = \((lo::Double)) -> \((hi::Double)) -> \((x::Double)) -> if ((x<hi)&&(x>lo)) then ((log 1)-(log (hi-lo))) else (0.000-1.000e20)

intBetween :: (BayNum a) => (a -> ((a -> (Prob Int))))
intBetween = \((lo::a)) -> \((hi::a)) -> unit>>=(\((x::Double)) -> return (floor ((x*(unround ((hi+1)-lo)))+(unround lo))))

any :: Prob a
any = undefined

improper_uniform_positiveLogPdf :: (BayNum a) => (a -> Double)
improper_uniform_positiveLogPdf = \((x::a)) -> if (x>0) then 1.000 else (0.000-1.000e20)

oneTo :: (BayNum a) => (a -> (Prob Int))
oneTo = \((n::a)) -> (uniform 0.500 ((unround n)+0.500))>>=(\((x::Double)) -> return (round x))

oneToLogPdf :: (BayNum a) => (a -> ((a -> Double)))
oneToLogPdf = \((hi::a)) -> \((x::a)) -> if ((x<(hi+1))&&(x>0)) then 1.000 else (0.000-1.000e10)

normalLogPdf :: (Double -> ((Double -> ((Double -> Double)))))
normalLogPdf = \((mean::Double)) -> \((variance::Double)) -> \((x::Double)) -> ((log 1)-(0.500*(log ((2.000*pi)*variance))))-(((x-mean)**2)/(2*variance))

logNormal :: (Double -> ((Double -> (Prob Double))))
logNormal = \((mean::Double)) -> \((variance::Double)) -> fmap exp (normal mean variance)

logNormalLogPdf :: (Double -> ((Double -> ((Double -> Double)))))
logNormalLogPdf = \((mean::Double)) -> \((variance::Double)) -> \((x::Double)) -> (log (1/(sqrt ((2.000*pi)*variance))))+(0.000-((((log x)-mean)*((log x)-mean))/(2*variance)))

normalLines :: (Double -> ((Double -> ([(Double,Double)]))))
normalLines = \((mean::Double)) -> \((v::Double)) -> let {(f::(Double -> ((Double,Double)))) = \((x::Double)) -> (x,(exp ((normalLogPdf mean v) x)));
 } in map f ((linspace (mean-(3*(sqrt v))) (mean+(3*(sqrt v)))) 50)

binomialProb :: (Int -> ((Double -> ((Int -> Double)))))
binomialProb = \((n::Int)) -> \((p::Double)) -> \((k::Int)) -> ((choose n k)*(p^k))*((1.000-p)^(n-k))

binomialLogProb :: (Int -> ((Double -> ((Int -> Double)))))
binomialLogProb = \((n::Int)) -> \((p::Double)) -> \((k::Int)) -> ((log (choose n k))+((unround k)*(log p)))+((unround (n-k))*(log (1.000-p)))

bernoulli :: (Double -> (Prob Bool))
bernoulli = \((p::Double)) -> unit>>=(\((u::Double)) -> return (u<p))

bernoulli01 :: (Double -> (Prob Int))
bernoulli01 = \((p::Double)) -> unit>>=(\((u::Double)) -> if (u<p) then (return 1) else (return 0))

bernoulliLogPdf :: (Double -> ((Bool -> Double)))
bernoulliLogPdf = \((p::Double)) -> \((b::Bool)) -> log (((p*((2*(boolToReal b))-1))+1)-(boolToReal b))

bernoulli01LogPdf :: (Double -> ((Double -> Double)))
bernoulli01LogPdf = \((p::Double)) -> \((b::Double)) -> log (((p*((2*b)-1))+1)-b)

countTrue :: (BayNum a) => (([Bool]) -> a)
countTrue = \(((_arg0)::[Bool])) -> case _arg0 of {[] -> 0; (True :(bs::[Bool])) -> 1+(countTrue bs); (False :(bs::[Bool])) -> countTrue bs}

binomial :: (BayNum a) => (Int -> ((Double -> (Prob a))))
binomial = \((n::Int)) -> \((p::Double)) -> (repeat n (unit>>=(\((u::Double)) -> return (u<p))))>>=(\((bools::[Bool])) -> return (countTrue bools))

exponential :: (Double -> (Prob Double))
exponential = \((lam::Double)) -> unit>>=(\((u::Double)) -> return (neg ((log u)/lam)))

exponentialLogPdf :: (Double -> ((Double -> Double)))
exponentialLogPdf = \((lam::Double)) -> \((x::Double)) -> lam*(exp ((0.000-lam)*x))

poissonAux :: (BayNum a) => (Double -> ((a -> ((Double -> (Prob a))))))
poissonAux = \((bigl::Double)) -> \((k::a)) -> \((p::Double)) -> if (p>bigl) then (unit>>=(\((u::Double)) -> (poissonAux bigl (k+1)) (p*u))) else (return (k-1))

poisson :: (Double -> (Prob Int))
poisson = \((lam::Double)) -> (poissonAux (exp (0.000-lam)) 0) 1

poissonLogPdf :: (Double -> ((Int -> Double)))
poissonLogPdf = \((lam::Double)) -> \((x::Int)) -> ((lam**(unround x))*(exp (0.000-lam)))/(unround (fac x))

betaAux :: (Int -> (Prob Double))
betaAux = \((n::Int)) -> (repeat n unit)>>=(\((us::[Double])) -> return (log (product us)))

beta :: (Int -> ((Int -> (Prob Double))))
beta = \((a::Int)) -> \((b::Int)) -> (betaAux a)>>=(\((g1::Double)) -> (betaAux b)>>=(\((g2::Double)) -> return (g1/(g1+g2))))

cof :: [Double]
cof = 76.180:((-86.505):(24.014:((-1.232):(1.209e-3:((-5.395e-6):[])))))

gammaln :: (Double -> Double)
gammaln = \((xx::Double)) -> let {(tmp'::Double) = (xx+5.500)-((xx+0.500)*(log (xx+5.500)));
 (ser'::Double) = ser+(sum (map (\(((y::Double),(c::Double))) -> c/(xx+y)) (zip (fromTo 1 7) cof)));
 } in (0.000-tmp')+(log ((2.507*ser')/xx))

betaf :: (Double -> ((Double -> Double)))
betaf = \((z::Double)) -> \((w::Double)) -> exp (((gammaln z)+(gammaln w))-(gammaln (z+w)))

betaLogPdf :: (Int -> ((Int -> ((Double -> Double)))))
betaLogPdf = \((a::Int)) -> \((b::Int)) -> \((x::Double)) -> log (((1.000/(betaf (unround a) (unround b)))*(x^(a-1)))*((1.000-x)^(b-1)))

invGamma :: (Double -> ((Double -> (Prob Double))))
invGamma = \((a::Double)) -> \((b::Double)) -> (gamma a (1.000/b))>>=(\((g::Double)) -> return (1.000/g))

wiener :: ((Double) -> (((Double) -> (Prob ((Double -> Double))))))
wiener = \((dt::Double)) -> \((tmax::Double)) -> let {(n::Int) = (round (tmax/dt))+1;
 } in (repeat n unormal)>>=(\((ns::[Double])) -> let {(etas::[Double]) = map (\((u::Double)) -> u*(sqrt dt)) ns;
 } in return ((pack dt (0.000-dt)) (listToVec ((scanl (\((x::Double)) -> \((y::Double)) -> x+y) 0.000) etas))))

multiNormal :: ((Vector Double) -> (((Matrix Double) -> (Prob (Vector Double)))))
multiNormal = \((vmean::Vector Double)) -> \((cov::Matrix Double)) -> (repeat (dim vmean) (normal 0 1))>>=(\((ns::[Double])) -> let {((u::Matrix Double):((s::Matrix Double):((v::Matrix Double):[]))) = svd cov;
 (j::Matrix Double) = mXm (mXm v (mmap sqrt s)) (transM v);
 } in return (vmean+(mXv j (listToVec ns))))

wieners :: ((Double) -> (((Double) -> (((Matrix Double) -> (Prob ([(Double -> Double)])))))))
wieners = \((dt::Double)) -> \((tmax::Double)) -> \((cov::Matrix Double)) -> let {(n_time_pts::Int) = (round (tmax/dt))+2;
 (n_dims::Int) = fst (mdims cov);
 (zeroV::Vector Double) = fillV n_dims (const 0.000);
 (scale_cov::Matrix Double) = dt*%cov;
 } in ((unfold n_time_pts zeroV) (\((vlast::Vector Double)) -> (multiNormal zeroV scale_cov)>>=(\((etas::Vector Double)) -> return (vlast+etas))))>>=(\((vs::[Vector Double])) -> let {(ll::[[Double]]) = transL (map vecToList vs);
 } in return (map (\((ys::[Double])) -> (pack dt (0.000-dt)) (listToVec ys)) ll))

diff :: ((Double) -> ((((Double -> Double)) -> ((Double -> Double)))))
diff = \((dt::Double)) -> \((w::(Double -> Double))) -> \((t::Double)) -> ((w t)-(w (t-dt)))/dt

decide :: (Double -> (((Vector Double) -> (((Prob a) -> (((((Vector Double) -> ((a -> Double)))) -> (Vector Double))))))))
decide = \((tol::Double)) -> \((ini::Vector Double)) -> \((dist::Prob a)) -> \((util::((Vector Double) -> ((a -> Double))))) -> (optimise tol ini) (\((vaction::Vector Double)) -> expect (fmap (util vaction) dist))

nsig :: (((Double -> Double)) -> ((Double -> (Prob ((Double -> Double))))))
nsig = \((sig::(Double -> Double))) -> \((v::Double)) -> case observeSig sig of {ObservedSignal (dt::Double) (t0::Double) (vpts::Vector Double) -> (repeat (dim vpts) (normal 0 v))>>=(\((ns::[Double])) -> return ((pack dt t0) (listToVec (map (\(((x::Double),(y::Double))) -> x+y) (zip ns (vecToList vpts))))))}

quantile :: (Double -> (((Prob Double) -> Double)))
quantile = \((x::Double)) -> \(Samples (xs::[Double])) -> let {(total::Int) = length xs;
 (under::Int) = length (filter (\((y::Double)) -> x<y) xs);
 } in (unround under)/(unround total)

gammaLogPdf :: (Double -> ((Double -> ((Double -> Double)))))
gammaLogPdf = \((k::Double)) -> \((theta::Double)) -> \((x::Double)) -> ((((k-1)*(log x))+((0.000-x)/theta))-(k*(log theta)))-(gammaln k)

invGammaLogPdf :: (Double -> ((Double -> ((Double -> Double)))))
invGammaLogPdf = \((a::Double)) -> \((b::Double)) -> \((x::Double)) -> log ((((b**a)/(exp (gammaln a)))*(x**((0.000-a)-1)))*(exp ((0.000-b)/x)))

cookAssert :: ((Prob Double) -> SamplerDensity)
cookAssert = \((s::Prob Double)) -> SamplerDensity s (uniformLogPdf 0 1)

multiNormalLogPdf :: (a -> ((b -> c)))
multiNormalLogPdf = \(vmean) -> \(cov) -> undefined

invWishart :: (Double -> (((Matrix Double) -> (Prob (Matrix Double)))))
invWishart = \((nu::Double)) -> \((s::Matrix Double)) -> undefined

invWishartLogPdf :: (a -> ((b -> ((c -> d)))))
invWishartLogPdf = \(nu) -> \(s) -> \(w) -> undefined

scandyn' :: (BayNum a) => (((Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes)))))))))) -> ((a -> ((Int -> ((sdes -> ((odes -> ((([odes]) -> ((([sdes]) -> ((a,([odes]))))))))))))))))
scandyn' = \(((_arg0)::(Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes))))))))))) -> \(((_arg1)::a)) -> \(((_arg2)::Int)) -> \((_arg3)) -> \((_arg4)) -> \(((_arg5)::[odes])) -> \(((_arg6)::[sdes])) -> case (_arg0,((_arg1,((_arg2,((_arg3,((_arg4,((_arg5,_arg6))))))))))) of {((f::(Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes)))))))))),((p::a),(_,(_,(_,((odeacc::[odes]),[])))))) -> (p,(reverse odeacc)); ((f::(Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes)))))))))),((p0::a),((i::Int),(sdelast,(odecurr,((odeacc::[odes]),(sde:(sdes::[sdes])))))))) -> let {((p1::a),odenext) = ((f i odecurr) sdelast) sde;
 } in (((((scandyn' f (p0+p1)) (i+1)) sde) odenext) (odenext:odeacc)) sdes}

scandyn :: (BayNum a) => (((Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes)))))))))) -> ((odes -> ((sdes -> ((([sdes]) -> ((a,([odes]))))))))))
scandyn = \((f::(Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes))))))))))) -> \(initode) -> \(initsde) -> \((sdes::[sdes])) -> (((((scandyn' f 0) 0) initsde) initode) []) sdes

tmax :: Double
tmax = 10.000

dt :: Double
dt = 1.000e-2

samples :: (BayNum a) => a
samples = 2000

heston :: Prob ((R.X R.:& V R.::: (Double -> Double) R.:& S R.::: (Double -> Double)) (Id KindStar))
heston = (gamma 1 1)>>=(\((k::Double)) -> (gamma 1 0.100)>>=(\((th::Double)) -> (gamma 1 0.100)>>=(\((eta::Double)) -> (normal 0 0.100)>>=(\((mu::Double)) -> (wiener dt tmax)>>=(\((w1::(Double -> Double))) -> (wiener dt tmax)>>=(\((w2::(Double -> Double))) -> (gamma 1 0.100)>>=(\((v_0::Double)) -> (uniform 0.000 2.000)>>=(\((s_0::Double)) -> let {v = solveODE (\v-> \((t::Double)) -> (k*(th-v))+((eta*(sqrt v))*((d dt w1) t))) tmax dt v_0;
s = solveODE (\s-> \((t::Double)) -> (mu*s)+(((sqrt (v t))*s)*((d dt w2) t))) tmax dt s_0;
} in return (R.X R.:& V R.:= (v) R.:& S R.:= (s))))))))))

heston1 :: Prob ((R.X R.:& V R.::: (Double -> Double) R.:& S R.::: (Double -> Double)) (Id KindStar))
heston1 = (return (R.X R.:& S_0 R.:= (1.000) R.:& V_0 R.:= (2.000e-2) R.:& Mu R.:= (5.000e-2) R.:& Eta R.:= (0.100) R.:& Th R.:= (2.000e-2) R.:& K R.:= (1.000))::Prob ((R.X R.:& S_0 R.::: Double R.:& V_0 R.::: Double R.:& Mu R.::: Double R.:& Eta R.::: Double R.:& Th R.::: Double R.:& K R.::: Double) (Id KindStar)))>>=(\(((_pars)::((R.X R.:& S_0 R.::: Double R.:& V_0 R.::: Double R.:& Mu R.::: Double R.:& Eta R.::: Double R.:& Th R.::: Double R.:& K R.::: Double) (Id KindStar)))) -> let {(k::Double) = _pars!!!K;
 } in let {(th::Double) = _pars!!!Th;
 } in let {(eta::Double) = _pars!!!Eta;
 } in let {(mu::Double) = _pars!!!Mu;
 } in let {(v_0::Double) = _pars!!!V_0;
 } in let {(s_0::Double) = _pars!!!S_0;
 } in (wiener dt tmax)>>=(\((_w_v)) -> (wiener dt tmax)>>=(\((_w_s)) -> let {v = solveODE (\v-> \((t::Double)) -> (k*(th-v))+((eta*(sqrt v))*((d dt _w_v) t))) tmax dt v_0;
s = solveODE (\s-> \((t::Double)) -> (mu*s)+(((sqrt (v t))*s)*((d dt _w_s) t))) tmax dt s_0;
} in return (R.X R.:& V R.:= (v) R.:& S R.:= (s)))))

data S = S deriving Show
instance R.Name S where
   name = S
data V = V deriving Show
instance R.Name V where
   name = V
data K = K deriving Show
instance R.Name K where
   name = K
data Th = Th deriving Show
instance R.Name Th where
   name = Th
data Eta = Eta deriving Show
instance R.Name Eta where
   name = Eta
data Mu = Mu deriving Show
instance R.Name Mu where
   name = Mu
data V_0 = V_0 deriving Show
instance R.Name V_0 where
   name = V_0
data S_0 = S_0 deriving Show
instance R.Name S_0 where
   name = S_0
data Posterior = Posterior deriving Show
instance R.Name Posterior where
   name = Posterior
data Postgrad = Postgrad deriving Show
instance R.Name Postgrad where
   name = Postgrad
data VToRec = VToRec deriving Show
instance R.Name VToRec where
   name = VToRec
data Inisam = Inisam deriving Show
instance R.Name Inisam where
   name = Inisam

main = do
  ((fakedata::((R.X R.:& V R.::: (Double -> Double) R.:& S R.::: (Double -> Double)) (Id KindStar))))::((R.X R.:& V R.::: (Double -> Double) R.:& S R.::: (Double -> Double)) (Id KindStar)) <- sample (heston1::Prob ((R.X R.:& V R.::: (Double -> Double) R.:& S R.::: (Double -> Double)) (Id KindStar)))
  let prims :: ((R.X R.:& Inisam R.::: Prob ([Double]) R.:& VToRec R.::: ((Vector Double) -> ((R.X R.:& V R.::: (Double -> Double) R.:& V_0 R.::: Double R.:& Mu R.::: Double R.:& Eta R.::: Double R.:& Th R.::: Double R.:& K R.::: Double) (Id KindStar))) R.:& Postgrad R.::: ((Vector Double) -> ((Double,(Vector Double)))) R.:& Posterior R.::: (([Double]) -> Double)) (Id KindStar))
      prims = let {s::(Double -> Double) = (fakedata!!!S);
 posterior = runP1 (bindP (fmapP (\((_x)) -> exp _x) headP) (\(k) -> bindP (fmapP (\((_x)) -> exp _x) headP) (\(th) -> bindP (fmapP (\((_x)) -> exp _x) headP) (\(eta) -> bindP headP (\(mu) -> bindP (fmapP (\((_x)) -> exp _x) headP) (\(v_0) -> bindP (fmapP (\(s) -> v_0:(map (\(_x ) -> exp _x) s)) (takeP$((round (tmax/dt))-1))) (\(v__obsSig) -> bindP (returnP ((packL dt 0) v__obsSig)) (\(v) -> returnP (((gammaLogPdf 1 1) k)+(((gammaLogPdf 1 0.100) th)+(((gammaLogPdf 1 0.100) eta)+(((normalLogPdf 0 0.100) mu)+(((gammaLogPdf 1 0.100) v_0)+(((uniformLogPdf 0.000 2.000) (s 0))+(let {scanfun = \(i) -> \([]) -> \((v_last:(s_last:[]))) -> \((v_next:(s_next:[]))) -> let {t = (unround i)*dt;
 v = const v_last;
 s = const s_last;
 pthis = (0+((normalLogPdf (v_last+(dt*(k*(th-(v t))))) (dt*((eta*(sqrt (v t)))*(eta*(sqrt (v t)))))) v_next))+((normalLogPdf (s_last+(dt*(mu*(s t)))) (dt*(((sqrt (v t))*(s t))*((sqrt (v t))*(s t))))) s_next);
 } in (pthis,[]);
 (pdynsys,dynsysres) = ((scandyn scanfun []) ((v 0):((s 0):[]))) (transL ((tail v__obsSig):((sigTail s):[])));
 } in pdynsys+0)))))))))))))));
 postgrad = \(_v ) -> runST$((newSTRef 0)>>=(\(postref) -> (VSM.replicate (dim _v) 0)>>=(\(gradref) -> (newSTRef (0::Int))>>=(\(countref) -> let {post_incr = addSTRef postref;
 grad_incr = addMV gradref;
 count_incr = incrSTRef countref;
 } in (count_incr 1)>>=(\((_k_pos)) -> let {k = exp (_v@>_k_pos);
 } in (((post_incr ((\((x::Double)) -> ((((1-1)*(log x))+((0.000-x)/1))-(1*(log 1)))-(gammaln 1)) k))>>(grad_incr _k_pos (((\((_x)) -> (exp _x)*1) (_v@>_k_pos))*((((0+((1-1)*((1/k)*1)))+((0+(1*(0-1)))/(1*1)))-0)-0))))>>(return ()))>>((count_incr 1)>>=(\((_th_pos)) -> let {th = exp (_v@>_th_pos);
 } in (((post_incr ((\((x::Double)) -> ((((1-1)*(log x))+((0.000-x)/0.100))-(1*(log 0.100)))-(gammaln 1)) th))>>(grad_incr _th_pos (((\((_x)) -> (exp _x)*1) (_v@>_th_pos))*((((0+((1-1)*((1/th)*1)))+((0+(0.100*(0-1)))/(0.100*0.100)))-0)-0))))>>(return ()))>>((count_incr 1)>>=(\((_eta_pos)) -> let {eta = exp (_v@>_eta_pos);
 } in (((post_incr ((\((x::Double)) -> ((((1-1)*(log x))+((0.000-x)/0.100))-(1*(log 0.100)))-(gammaln 1)) eta))>>(grad_incr _eta_pos (((\((_x)) -> (exp _x)*1) (_v@>_eta_pos))*((((0+((1-1)*((1/eta)*1)))+((0+(0.100*(0-1)))/(0.100*0.100)))-0)-0))))>>(return ()))>>((count_incr 1)>>=(\((_mu_pos)) -> let {mu = _v@>_mu_pos;
 } in (((post_incr ((\((x::Double)) -> ((log 1)-(0.500*(log ((2.000*pi)*0.100))))-(((x-0)**2)/(2*0.100))) mu))>>(grad_incr _mu_pos (0-((0+((2*0.100)*((2*(mu-0))*(1-0))))/((2*0.100)*(2*0.100))))))>>(return ()))>>((count_incr 1)>>=(\((_v_0_pos)) -> let {v_0 = exp (_v@>_v_0_pos);
 } in (((post_incr ((\((x::Double)) -> ((((1-1)*(log x))+((0.000-x)/0.100))-(1*(log 0.100)))-(gammaln 1)) v_0))>>(grad_incr _v_0_pos (((\((_x)) -> (exp _x)*1) (_v@>_v_0_pos))*((((0+((1-1)*((1/v_0)*1)))+((0+(0.100*(0-1)))/(0.100*0.100)))-0)-0))))>>(return ()))>>(((post_incr ((\(_) -> (log 1)-(log (2.000-0.000))) (s 0.000)))>>(return ()))>>((count_incr ((round (tmax/dt))-1))>>=(\(v__startpos) -> let {v__obsSig = vcons v_0 (vmap (\((_x)) -> exp _x) ((slice v__startpos ((round (tmax/dt))-1)) _v));
 v = (pack dt 0) v__obsSig;
 } in (forM (fromTo 1 ((round (tmax/dt))-1)) (\((_timeix)) -> let {t = (unround _timeix)*dt;
 t__last = (unround (_timeix-1))*dt;
 v__last = v t__last;
 s__last = s t__last;
 } in ((return ())>>(let {v__next = v t;
 v__mean = v__last+(dt*(k*(th-v__last)));
 v__var = dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)));
 } in (post_incr (((log 1)-(0.500*(log ((2*pi)*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last))))))))-(((v__next-(v__last+(dt*(k*(th-v__last)))))**2)/(2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last))))))))>>((((((return ())>>(let {(_v__next_pos) = v__startpos+(_timeix-1);
 } in grad_incr _v__next_pos (((\((_x)) -> (exp _x)*1) (_v@>_v__next_pos))*(0-((0+((2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))*((2*(v__next-(v__last+(dt*(k*(th-v__last))))))*(1-0))))/((2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))*(2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))))))))>>(grad_incr _eta_pos (((\((_x)) -> (exp _x)*1) (_v@>_eta_pos))*((0-(0+(0.500*((1/((2*pi)*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last))))))*(0+((2*pi)*(0+(dt*((0+((eta*(sqrt v__last))*(0+((sqrt v__last)*1))))+((eta*(sqrt v__last))*(0+((sqrt v__last)*1))))))))))))-((0+(0-(((v__next-(v__last+(dt*(k*(th-v__last)))))**2)*(0+(2*(0+(dt*((0+((eta*(sqrt v__last))*(0+((sqrt v__last)*1))))+((eta*(sqrt v__last))*(0+((sqrt v__last)*1)))))))))))/((2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))*(2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))))))))>>(grad_incr _th_pos (((\((_x)) -> (exp _x)*1) (_v@>_th_pos))*(0-((0+((2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))*((2*(v__next-(v__last+(dt*(k*(th-v__last))))))*(0-(0+(0+(dt*(0+(k*(1-0))))))))))/((2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))*(2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))))))))>>(grad_incr _k_pos (((\((_x)) -> (exp _x)*1) (_v@>_k_pos))*(0-((0+((2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))*((2*(v__next-(v__last+(dt*(k*(th-v__last))))))*(0-(0+(0+(dt*(0+((th-v__last)*1)))))))))/((2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))*(2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))))))))>>(let {(_v__last_pos) = if (_timeix==1) then _v_0_pos else (v__startpos+(_timeix-2));
 } in grad_incr _v__last_pos (((\((_x)) -> (exp _x)*1) (_v@>_v__last_pos))*((0-(0+(0.500*((1/((2*pi)*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last))))))*(0+((2*pi)*(0+(dt*((0+((eta*(sqrt v__last))*(0+(eta*((0.500*(1/(sqrt v__last)))*1)))))+((eta*(sqrt v__last))*(0+(eta*((0.500*(1/(sqrt v__last)))*1)))))))))))))-(((0+((2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))*((2*(v__next-(v__last+(dt*(k*(th-v__last))))))*(0-(1+(0+(dt*(0+(k*(0-1))))))))))+(0-(((v__next-(v__last+(dt*(k*(th-v__last)))))**2)*(0+(2*(0+(dt*((0+((eta*(sqrt v__last))*(0+(eta*((0.500*(1/(sqrt v__last)))*1)))))+((eta*(sqrt v__last))*(0+(eta*((0.500*(1/(sqrt v__last)))*1))))))))))))/((2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))*(2*(dt*((eta*(sqrt v__last))*(eta*(sqrt v__last)))))))))))))>>(let {s__next = s t;
 s__mean = s__last+(dt*(mu*s__last));
 s__var = dt*(((sqrt v__last)*s__last)*((sqrt v__last)*s__last));
 } in (post_incr (((log 1)-(0.500*(log ((2*pi)*s__var))))-(((s__next-s__mean)**2)/(2*s__var))))>>(((return ())>>(grad_incr _mu_pos (1*(0-((0+((2*(dt*(((sqrt v__last)*s__last)*((sqrt v__last)*s__last))))*((2*(s__next-(s__last+(dt*(mu*s__last)))))*(0-(0+(0+(dt*(0+(s__last*1)))))))))/((2*(dt*(((sqrt v__last)*s__last)*((sqrt v__last)*s__last))))*(2*(dt*(((sqrt v__last)*s__last)*((sqrt v__last)*s__last))))))))))>>(let {(_v__last_pos) = if (_timeix==1) then _v_0_pos else (v__startpos+(_timeix-2));
 } in grad_incr _v__last_pos (((\((_x)) -> (exp _x)*1) (_v@>_v__last_pos))*((0-(0+(0.500*((1/((2*pi)*(dt*(((sqrt v__last)*s__last)*((sqrt v__last)*s__last)))))*(0+((2*pi)*(0+(dt*((0+(((sqrt v__last)*s__last)*(0+(s__last*((0.500*(1/(sqrt v__last)))*1)))))+(((sqrt v__last)*s__last)*(0+(s__last*((0.500*(1/(sqrt v__last)))*1)))))))))))))-((0+(0-(((s__next-(s__last+(dt*(mu*s__last))))**2)*(0+(2*(0+(dt*((0+(((sqrt v__last)*s__last)*(0+(s__last*((0.500*(1/(sqrt v__last)))*1)))))+(((sqrt v__last)*s__last)*(0+(s__last*((0.500*(1/(sqrt v__last)))*1))))))))))))/((2*(dt*(((sqrt v__last)*s__last)*((sqrt v__last)*s__last))))*(2*(dt*(((sqrt v__last)*s__last)*((sqrt v__last)*s__last)))))))))))))>>((readSTRef postref)>>=(\(postval) -> (unsafeFreeze gradref)>>=(\(gradval) -> return ((postval,gradval)))))))))))))))))))));
 vToRec = \(theta) -> runP (toList theta) (bindP (fmapP (\((_x)) -> exp _x) headP) (\(k) -> bindP (fmapP (\((_x)) -> exp _x) headP) (\(th) -> bindP (fmapP (\((_x)) -> exp _x) headP) (\(eta) -> bindP headP (\(mu) -> bindP (fmapP (\((_x)) -> exp _x) headP) (\(v_0) -> bindP (fmapP (\(s) -> v_0:(map (\(_x ) -> exp _x) s)) (takeP$((round (tmax/dt))-1))) (\(v__obsSig) -> bindP (returnP ((packL dt 0) v__obsSig)) (\(v) -> returnP (R.X R.:& V R.:= (v) R.:& V_0 R.:= (v_0) R.:& Mu R.:= (mu) R.:& Eta R.:= (eta) R.:& Th R.:= (th) R.:& K R.:= (k))))))))));
 inisam = (gamma 1 1)>>=(\(k) -> (gamma 1 0.100)>>=(\(th) -> (gamma 1 0.100)>>=(\(eta) -> (normal 0 0.100)>>=(\(mu) -> (gamma 1 0.100)>>=(\(v_0) -> (uniform 0.000 2.000)>>=(\(s_0) -> (wiener dt tmax)>>=(\(w2) -> (wiener dt tmax)>>=(\(w1) -> let {v = solveODE (\v-> \((t::Double)) -> (k*(th-v))+((eta*(sqrt v))*((d dt w1) t))) tmax dt v_0;
s = solveODE (\s-> \((t::Double)) -> (mu*s)+(((sqrt (v t))*s)*((d dt w2) t))) tmax dt s_0;
} in return (concat (((log k):[]):(((log th):[]):(((log eta):[]):((mu:[]):(((log v_0):[]):((sigTail ((\((_x)) -> log _x).v)):[])))))))))))))));
 } in R.X R.:& Inisam R.:= (inisam) R.:& VToRec R.:= (vToRec) R.:& Postgrad R.:= (postgrad) R.:& Posterior R.:= (posterior)
  let post :: (([Double]) -> Double)
      post = prims!!!Posterior
  let postgrad :: ((Vector Double) -> ((Double,(Vector Double))))
      postgrad = prims!!!Postgrad
  let vtorec :: ((Vector Double) -> ((R.X R.:& V R.::: (Double -> Double) R.:& V_0 R.::: Double R.:& Mu R.::: Double R.:& Eta R.::: Double R.:& Th R.::: Double R.:& K R.::: Double) (Id KindStar)))
      vtorec = prims!!!VToRec
  let inisam :: Prob ([Double])
      inisam = prims!!!Inisam
  return (post, postgrad,vtorec,inisam)
