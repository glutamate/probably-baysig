{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, ImplicitParams, ViewPatterns #-}
module Target.Regression where

import Target.Prelude
import qualified Data.Record as R
import Prelude hiding (scanl,repeat,replicate)
import Data.Record.Combinators ((!!!))
import Data.Kind
import Data.List (transpose)
import Data.TypeFun
import Numeric.LinearAlgebra hiding (diag, linspace, svd, )
import Math.Probably.Sampler hiding (uniform,primOneOf,logNormal,invGamma,binomial,gamma,oneOf,bernoulli, normal, unormal, unit)
import qualified Data.Text as T
import Foreign.Storable (Storable)
import Data.STRef
import Control.Monad.ST
import Control.Monad (forM, forM_)
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Storable as VS
import Foreign.Storable.Tuple

tmax :: Double
tmax = 1.000

dt :: Double
dt = 1.000e-2

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
plotStyle = \((opts::[(T.Text,T.Text)])) -> \(Plot (pos::[(T.Text,T.Text)]) (plr::Prob ([Radian]))) -> Plot (append opts pos) plr

axisLabels :: (T.Text -> ((T.Text -> ((Plot -> Plot)))))
axisLabels = \((xlab::T.Text)) -> \((ylab::T.Text)) -> \(Plot (popts::[(T.Text,T.Text)]) (plns::Prob ([Radian]))) -> Plot ((((T.pack "axis-x-label"),xlab)):((((T.pack "axis-y-label"),ylab)):popts)) plns

unPlotOpts :: (Plot -> ([(T.Text,T.Text)]))
unPlotOpts = \(Plot (os::[(T.Text,T.Text)]) (x::Prob ([Radian]))) -> os

sigLast :: (((Double -> Double)) -> Double)
sigLast = \((sig::(Double -> Double))) -> case observeSig sig of {ObservedSignal (dt::Double) (t0::Double) (pts::Vector Double) -> pts@>((dim pts)-1); ObservedXYSignal (pts::Vector ((Double,Double))) -> snd (pts@>((dim pts)-1))}

sigTail :: (((Double -> Double)) -> ([Double]))
sigTail = \((sig::(Double -> Double))) -> case observeSig sig of {ObservedSignal (dt::Double) (t0::Double) (pts::Vector Double) -> vecToList pts}

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
bernoulliLogPdf = \(((_arg0)::Double)) -> \(((_arg1)::Bool)) -> case (_arg0,_arg1) of {((p::Double),True ) -> log p; ((p::Double),False ) -> log (1-p)}

bernoulli01LogPdf :: (Double -> ((Int -> Double)))
bernoulli01LogPdf = \(((_arg0)::Double)) -> \(((_arg1)::Int)) -> case (_arg0,_arg1) of {((p::Double),1) -> log p; ((p::Double),0) -> log (1-p)}

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

multiNormal :: ((Vector Double) -> (((Matrix Double) -> (Prob (Vector Double)))))
multiNormal = \((vmean::Vector Double)) -> \((cov::Matrix Double)) -> (repeat (dim vmean) (normal 0 1))>>=(\((ns::[Double])) -> let {((u::Matrix Double):((s::Matrix Double):((v::Matrix Double):[]))) = svd cov;
 (j::Matrix Double) = mXm (mXm v (mmap sqrt s)) (transM v);
 } in return (vmean+(mXv j (listToVec ns))))

scandyn' :: (BayNum a) => (((Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes)))))))))) -> ((a -> ((Int -> ((sdes -> ((odes -> ((([odes]) -> ((([sdes]) -> ((a,([odes]))))))))))))))))
scandyn' = \(((_arg0)::(Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes))))))))))) -> \(((_arg1)::a)) -> \(((_arg2)::Int)) -> \((_arg3)) -> \((_arg4)) -> \(((_arg5)::[odes])) -> \(((_arg6)::[sdes])) -> case (_arg0,((_arg1,((_arg2,((_arg3,((_arg4,((_arg5,_arg6))))))))))) of {((f::(Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes)))))))))),((p::a),(_,(_,(_,((odeacc::[odes]),[])))))) -> (p,(reverse odeacc)); ((f::(Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes)))))))))),((p0::a),((i::Int),(sdelast,(odecurr,((odeacc::[odes]),(sde:(sdes::[sdes])))))))) -> let {((p1::a),odenext) = ((f i odecurr) sdelast) sde;
 } in (((((scandyn' f (p0+p1)) (i+1)) sde) odenext) (odenext:odeacc)) sdes}

scandyn :: (BayNum a) => (((Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes)))))))))) -> ((odes -> ((sdes -> ((([sdes]) -> ((a,([odes]))))))))))
scandyn = \((f::(Int -> ((odes -> ((sdes -> ((sdes -> ((a,odes))))))))))) -> \(initode) -> \(initsde) -> \((sdes::[sdes])) -> (((((scandyn' f 0) 0) initsde) initode) []) sdes

samples :: (BayNum a) => a
samples = 10000

regress :: Prob ([((R.X R.:& Y R.::: Double R.:& W R.::: Double) (Id KindStar))])
regress = (normal 0 1)>>=(\((offset::Double)) -> (gamma 1 1)>>=(\((sigma::Double)) -> (normal 0 1)>>=(\((slope::Double)) -> repeat 50 ((normal 0 1)>>=(\((w::Double)) -> (normal (offset+(slope*w)) sigma)>>=(\((y::Double)) -> return (R.X R.:& Y R.:= (y) R.:& W R.:= (w))))))))

regress1 :: Prob ([((R.X R.:& Y R.::: Double R.:& W R.::: Double) (Id KindStar))])
regress1 = repeat 50 ((uniform 0.000 1.000)>>=(\((w::Double)) -> (normal ((1*w)-0.500) 0.100)>>=(\((y::Double)) -> return (R.X R.:& Y R.:= (y) R.:& W R.:= (w)))))

data W = W deriving Show
instance R.Name W where
   name = W
data Y = Y deriving Show
instance R.Name Y where
   name = Y
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
data Offset = Offset deriving Show
instance R.Name Offset where
   name = Offset
data Sigma = Sigma deriving Show
instance R.Name Sigma where
   name = Sigma
data Slope = Slope deriving Show
instance R.Name Slope where
   name = Slope

get_ = do
  ((fakedata::[((R.X R.:& Y R.::: Double R.:& W R.::: Double) (Id KindStar))]))::[((R.X R.:& Y R.::: Double R.:& W R.::: Double) (Id KindStar))] <- sample (regress1::Prob ([((R.X R.:& Y R.::: Double R.:& W R.::: Double) (Id KindStar))]))
  let prims :: ((R.X R.:& Inisam R.::: Prob ([Double]) R.:& VToRec R.::: ((Vector Double) -> ((R.X R.:& Slope R.::: Double R.:& Sigma R.::: Double R.:& Offset R.::: Double) (Id KindStar))) R.:& Postgrad R.::: ((Vector Double) -> ((Double,(Vector Double)))) R.:& Posterior R.::: (([Double]) -> Double)) (Id KindStar))
      prims = let {(final0::[((R.X R.:& Y R.::: Double R.:& W R.::: Double) (Id KindStar))]) = fakedata;
 posterior = runP1 (bindP headP (\(offset) -> bindP (fmapP (\((_x)) -> exp _x) headP) (\(sigma) -> bindP headP (\(slope) -> bindP (forP final0 (\((R.X R.:& Y R.:= (y::Double) R.:& W R.:= (w::Double))) -> returnP ())) (\(final0_pars) -> returnP (((normalLogPdf 0 1) offset)+(((gammaLogPdf 1 1) sigma)+(((normalLogPdf 0 1) slope)+((sum (map (\(((R.X R.:& Y R.:= (y::Double) R.:& W R.:= (w::Double)),(()))) -> ((normalLogPdf 0 1) w)+(((normalLogPdf (offset+(slope*w)) sigma) y)+0)) (zip final0 final0_pars)))+0)))))))));
 postgrad = \((_v)) -> runST ((newSTRef 0)>>=(\(postref) -> (VSM.replicate (dim _v) 0)>>=(\(gradref) -> (newSTRef (0::Int))>>=(\(countref) -> let {post_incr = addSTRef postref;
 grad_incr = addMV gradref;
 count_incr = incrSTRef countref;
 } in (count_incr 1)>>=(\((_offset_pos)) -> let {offset = _v@>_offset_pos;
 } in (((post_incr ((\(x) -> ((log 1)-(0.500*(log ((2.000*pi)*1))))-(((x-0)**2)/(2*1))) offset))>>(grad_incr _offset_pos (0-((0+((2*1)*((2*(offset-0))*(1-0))))/((2*1)*(2*1))))))>>(return ()))>>((count_incr 1)>>=(\((_sigma_pos)) -> let {sigma = exp (_v@>_sigma_pos);
 } in (((post_incr ((\(x) -> ((((1-1)*(log x))+((0.000-x)/1))-(1*(log 1)))-(gammaln 1)) sigma))>>(grad_incr _sigma_pos (((\((_x)) -> (exp _x)*1) (_v@>_sigma_pos))*((((0+((1-1)*((1/sigma)*1)))+((0+(1*(0-1)))/(1*1)))-0)-0))))>>(return ()))>>((count_incr 1)>>=(\((_slope_pos)) -> let {slope = _v@>_slope_pos;
 } in (((post_incr ((\(x) -> ((log 1)-(0.500*(log ((2.000*pi)*1))))-(((x-0)**2)/(2*1))) slope))>>(grad_incr _slope_pos (0-((0+((2*1)*((2*(slope-0))*(1-0))))/((2*1)*(2*1))))))>>(return ()))>>((forM final0 (\((R.X R.:& Y R.:= (y::Double) R.:& W R.:= (w::Double))) -> ((post_incr ((\(x) -> ((log 1)-(0.500*(log ((2.000*pi)*1))))-(((x-0)**2)/(2*1))) w))>>(return ()))>>(((post_incr ((\(x) -> ((log 1)-(0.500*(log ((2.000*pi)*sigma))))-(((x-(offset+(slope*w)))**2)/(2*sigma))) y))>>((((return ())>>(grad_incr _slope_pos (1*(0-((0+((2*sigma)*((2*(y-(offset+(slope*w))))*(0-(0+(0+(w*1)))))))/((2*sigma)*(2*sigma)))))))>>(grad_incr _sigma_pos (((\((_x)) -> (exp _x)*1) (_v@>_sigma_pos))*((0-(0+(0.500*((1/((2.000*pi)*sigma))*(0+((2.000*pi)*1))))))-((0+(0-(((y-(offset+(slope*w)))**2)*(0+(2*1)))))/((2*sigma)*(2*sigma)))))))>>(grad_incr _offset_pos (1*(0-((0+((2*sigma)*((2*(y-(offset+(slope*w))))*(0-(1+0)))))/((2*sigma)*(2*sigma))))))))>>(return (R.X R.:& Y R.:= (y) R.:& W R.:= (w))))))>>=(\((final0::[((R.X R.:& Y R.::: Double R.:& W R.::: Double) (Id KindStar))])) -> (readSTRef postref)>>=(\(postval) -> (VS.unsafeFreeze gradref)>>=(\(gradval) -> return ((postval,gradval)))))))))))))));
 vToRec = \(theta) -> runP (toList theta) (bindP headP (\(offset) -> bindP (fmapP (\((_x)) -> exp _x) headP) (\(sigma) -> bindP headP (\(slope) -> bindP (forP final0 (\((R.X R.:& Y R.:= (y::Double) R.:& W R.:= (w::Double))) -> returnP ())) (\(final0_pars) -> returnP (R.X R.:& Slope R.:= (slope) R.:& Sigma R.:= (sigma) R.:& Offset R.:= (offset)))))));
 inisam = (normal 0 1)>>=(\(offset) -> (gamma 1 1)>>=(\(sigma) -> (normal 0 1)>>=(\(slope) -> (repeat (length final0) ((normal 0 1)>>=(\(w) -> (normal (offset+(slope*w)) sigma)>>=(\(y) -> return (((R.X R.:& Y R.:= (y) R.:& W R.:= (w)),(concat [])))))))>>=(\(final0_ret) -> let {(final0::[((R.X R.:& Y R.::: Double R.:& W R.::: Double) (Id KindStar))]) = map fst final0_ret;
 final0_iniret = map snd final0_ret;
 } in return (concat ((offset:[]):(((log sigma):[]):((slope:[]):((concat final0_iniret):[])))))))));
 } in R.X R.:& Inisam R.:= (inisam) R.:& VToRec R.:= (vToRec) R.:& Postgrad R.:= (postgrad) R.:& Posterior R.:= (posterior)
  let post :: (([Double]) -> Double)
      post = prims!!!Posterior
  let postgrad :: ((Vector Double) -> ((Double,(Vector Double))))
      postgrad = prims!!!Postgrad
  let vtorec :: ((Vector Double) -> ((R.X R.:& Slope R.::: Double R.:& Sigma R.::: Double R.:& Offset R.::: Double) (Id KindStar)))
      vtorec = prims!!!VToRec
  let inisam :: Prob ([Double])
      inisam = prims!!!Inisam
  return (post, postgrad,vtorec,inisam)
