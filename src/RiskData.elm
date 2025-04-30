module RiskData exposing (..)

import Dict exposing (Dict)

type alias RiskDataBin = 
    { totalTransmissions : Float
    , unstableTransmissions : Float
    , numFullMutations : Float
    , matRptChangeMin : Float
    , matRptChangeMax : Float
    , medianMatRptChange : Float }

riskDict : Dict (Int, Int) RiskDataBin
riskDict = Dict.fromList
    -- Formatted (floor of bin rounded to nearest 5 and num agg i.e. (45,0) is bin 45-49 and 0 AGG)
    [((45,0), {totalTransmissions=5.0, unstableTransmissions=4.0, numFullMutations=0.0, matRptChangeMin=1.0, matRptChangeMax=5.0, medianMatRptChange=1.5} )
    ,((45,1), {totalTransmissions=32.0, unstableTransmissions=6.0, numFullMutations=0.0, matRptChangeMin=1.0, matRptChangeMax=2.0, medianMatRptChange=1.0} )
    ,((45,2), {totalTransmissions=57.0, unstableTransmissions=3.0, numFullMutations=0.0, matRptChangeMin=-1.0, matRptChangeMax=2.0, medianMatRptChange=1.0} )
    ,((45,3), {totalTransmissions=3.0, unstableTransmissions=0.0, numFullMutations=0.0, matRptChangeMin=0.0, matRptChangeMax=0.0, medianMatRptChange=0.0} )
    ,((45,4), {totalTransmissions=1.0, unstableTransmissions=0.0, numFullMutations=0.0, matRptChangeMin=0.0, matRptChangeMax=0.0, medianMatRptChange=0.0} )
    ,((50,0), {totalTransmissions=9.0, unstableTransmissions=9.0, numFullMutations=0.0, matRptChangeMin=1.0, matRptChangeMax=31.0, medianMatRptChange=4.0} )
    ,((50,1), {totalTransmissions=49.0, unstableTransmissions=11.0, numFullMutations=0.0, matRptChangeMin=-20.0, matRptChangeMax=4.0, medianMatRptChange=1.0} )
    ,((50,2), {totalTransmissions=41.0, unstableTransmissions=5.0, numFullMutations=0.0, matRptChangeMin=1.0, matRptChangeMax=4.0, medianMatRptChange=2.0} )
    ,((50,3), {totalTransmissions=2.0, unstableTransmissions=0.0, numFullMutations=0.0, matRptChangeMin=0.0, matRptChangeMax=0.0, medianMatRptChange=0.0} )
    ,((50,4), {totalTransmissions=1.0, unstableTransmissions=0.0, numFullMutations=0.0, matRptChangeMin=0.0, matRptChangeMax=0.0, medianMatRptChange=0.0} )
    ,((55,0), {totalTransmissions=30.0, unstableTransmissions=29.0, numFullMutations=1.0, matRptChangeMin=1.0, matRptChangeMax=55.0, medianMatRptChange=9.5} )
    ,((55,1), {totalTransmissions=95.0, unstableTransmissions=50.0, numFullMutations=0.0, matRptChangeMin=-2.0, matRptChangeMax=8.0, medianMatRptChange=2.0} )
    ,((55,2), {totalTransmissions=64.0, unstableTransmissions=6.0, numFullMutations=0.0, matRptChangeMin=-2.0, matRptChangeMax=1.0, medianMatRptChange=1.0} )
    ,((55,3), {totalTransmissions=8.0, unstableTransmissions=0.0, numFullMutations=0.0, matRptChangeMin=0.0, matRptChangeMax=0.0, medianMatRptChange=0.0} )
    ,((60,0), {totalTransmissions=37.0, unstableTransmissions=36.0, numFullMutations=2.0, matRptChangeMin=4.0, matRptChangeMax=93.0, medianMatRptChange=16.0} )
    ,((60,1), {totalTransmissions=39.0, unstableTransmissions=33.0, numFullMutations=0.0, matRptChangeMin=-4.0, matRptChangeMax=10.0, medianMatRptChange=4.0} )
    ,((60,2), {totalTransmissions=38.0, unstableTransmissions=20.0, numFullMutations=0.0, matRptChangeMin=-17.0, matRptChangeMax=24.0, medianMatRptChange=1.0} )
    ,((60,3), {totalTransmissions=1.0, unstableTransmissions=1.0, numFullMutations=0.0, matRptChangeMin=2.0, matRptChangeMax=2.0, medianMatRptChange=2.0} )
    ,((65,0), {totalTransmissions=35.0, unstableTransmissions=35.0, numFullMutations=6.0, matRptChangeMin=-9.0, matRptChangeMax=68.0, medianMatRptChange=20.0} )
    ,((65,1), {totalTransmissions=28.0, unstableTransmissions=25.0, numFullMutations=0.0, matRptChangeMin=2.0, matRptChangeMax=48.0, medianMatRptChange=8.0} )
    ,((65,2), {totalTransmissions=20.0, unstableTransmissions=14.0, numFullMutations=0.0, matRptChangeMin=1.0, matRptChangeMax=4.0, medianMatRptChange=1.5} )
    ,((65,3), {totalTransmissions=2.0, unstableTransmissions=0.0, numFullMutations=0.0, matRptChangeMin=0.0, matRptChangeMax=0.0, medianMatRptChange=0.0} )
    ,((70,0), {totalTransmissions=29.0, unstableTransmissions=29.0, numFullMutations=15.0, matRptChangeMin=-11.0, matRptChangeMax=70.0, medianMatRptChange=35.5} )
    ,((70,1), {totalTransmissions=41.0, unstableTransmissions=40.0, numFullMutations=3.0, matRptChangeMin=-17.0, matRptChangeMax=63.0, medianMatRptChange=9.0} )
    ,((70,2), {totalTransmissions=14.0, unstableTransmissions=13.0, numFullMutations=0.0, matRptChangeMin=1.0, matRptChangeMax=14.0, medianMatRptChange=3.0} )
    ,((75,0), {totalTransmissions=43.0, unstableTransmissions=43.0, numFullMutations=32.0, matRptChangeMin=-44.0, matRptChangeMax=116.0, medianMatRptChange=37.0} )
    ,((75,1), {totalTransmissions=42.0, unstableTransmissions=42.0, numFullMutations=14.0, matRptChangeMin=-17.0, matRptChangeMax=93.0, medianMatRptChange=21.5} )
    ,((75,2), {totalTransmissions=14.0, unstableTransmissions=14.0, numFullMutations=1.0, matRptChangeMin=-2.0, matRptChangeMax=96.0, medianMatRptChange=7.0} )
    ,((80,0), {totalTransmissions=31.0, unstableTransmissions=31.0, numFullMutations=27.0, matRptChangeMin=-9.0, matRptChangeMax=96.0, medianMatRptChange=6.5} )
    ,((80,1), {totalTransmissions=45.0, unstableTransmissions=45.0, numFullMutations=30.0, matRptChangeMin=-10.0, matRptChangeMax=93.0, medianMatRptChange=36.0} )
    ,((80,2), {totalTransmissions=20.0, unstableTransmissions=20.0, numFullMutations=3.0, matRptChangeMin=4.0, matRptChangeMax=83.0, medianMatRptChange=15.5} )
    ,((85,0), {totalTransmissions=8.0, unstableTransmissions=8.0, numFullMutations=7.0, matRptChangeMin=59.0, matRptChangeMax=59.0, medianMatRptChange=59.0} )
    ,((85,1), {totalTransmissions=30.0, unstableTransmissions=30.0, numFullMutations=25.0, matRptChangeMin=30.0, matRptChangeMax=87.0, medianMatRptChange=74.0} )
    ,((85,2), {totalTransmissions=4.0, unstableTransmissions=4.0, numFullMutations=2.0, matRptChangeMin=10.0, matRptChangeMax=13.0, medianMatRptChange=11.5}) ]