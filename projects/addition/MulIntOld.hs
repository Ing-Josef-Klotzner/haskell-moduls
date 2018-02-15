module MulIntOld where
import Test.Hspec

-- version from 8_6_Exercises.hs

mulIntO' :: Integral a => a -> a -> a
mulIntO' 0 _ = 0
mulIntO' _ 0 = 0
mulIntO' x 1 = x
mulIntO' 1 y = y
mulIntO' x y = x + mulIntO' x (y - 1)

mulIntO :: (Integral a, Show a, Read a) => a -> a -> a
mulIntO x y
    | (y > 0 && x < 0) || (y < 0 && x > 0) && abs y < abs x = - (mulIntO'' (abs x) (abs y))
    | (y > 0 && x < 0) || (y < 0 && x > 0) && abs x < abs y = - (mulIntO'' (abs y) (abs x))
    | y < x = mulIntO'' (abs x) (abs y)
    | otherwise = mulIntO'' (abs y) (abs x)

--   876 * 234
--    3504    +     3504 = 876 * 4
--   2628 *10 +     2628 = 876 * 3
--  1752 *100 +     1752 = 876 * 2
--  204984      Result
-- version using above method for calculation
mulIntO'' :: (Integral a, Show a, Read a) => a -> a -> a
mulIntO'' x y = go x y 0
    where go a b res
            | b == 0 = res
            | otherwise = go (mulIntx10_n a 1) (div b 10) (res + mulIntO' a (mod b 10))

-- multiply Integer x by 10^n
mulIntx10_n :: (Read a, Show a, Integral a) => a -> a -> a
mulIntx10_n x n = read (show x ++ nTimesZero n)  --read (intToString x ++ nTimesZero n)

nTimesZero :: (Eq a, Num a) => a -> [Char]
nTimesZero n = go n ""
    where go ct str
            | ct == 0 = str
            | otherwise = go (ct - 1) (str ++ "0")

m1 :: Integer
m1 =  418865413135035184311351513108877941561332032032020320246845181975110515101564681554989856512288522222565884665564422221221154412158885188511238101564681554989856512288522222565884665564422221221154412158885188511238101564681554989856512288522222565884665564422221221154412158885188511238101564681554989856512288522222565884665564422221221154412158885188511238101564681554989856512288522222565884665564422221221154412158885188511238101564681554989856512288522222565884665564422221221154412158885188511238101564681554989856512288522222565884665564422221221154412158885188511238101564681554989856512288522222565884665564422221221154412158885188511238101564681554989856512288522222565884665564422221221154412158885188511238101564681554989856512288522222565884665564422221221154412158885188511238101564681554989856512288522222565884665564422221221154412158885188511238

m2 :: Integer
m2 =  175448234320783705510445245255050819736965667314182070476996680958373787748642367257531039457553578542512179209283443713967789852408275115675290524879091545118792038924269371221137061077330591511251593116889372686400601833369702422200658383571085313084751443571474789013863379910206620239678787647859725609277842872799405032441809812358066776133642931040554078755741926017029017897302174513496980132176053241344538403905951874487917832696204174332426516761476227588927822542294124622300674168972708421756909650482331635835136220777941680875512908535007900062944431993542355595986604760488939243755680079655772823203274775891177825214695014376289435063559038646242652375139381369864770893641016774455587484958035210223274140513316803546060994598683083956718584007257657733349755221056044157113217467594960849469614057984798048666274373498541011112025484076878090952209338008519234650329252786294635305870024341025976109534535769059914725836181675190970998036225865344488191818998154697271315066265955191631034759227397033667589416766563630396244128631913813993552796002934357697195682273118876364180797307261916304296438265672930715790525739913523763356605318840719060772177847960202212348747899432047438028255476892689829517528364562561757363558388658488120401057533191164160265985213871855895678451410284404453954938929356774028453367166950280882503714950851021961839374456006247150546319470055059936505676800709397604741444687830188028000297501728089847137700010753345844557986434468514326979174424809354094161220547449932543729080551451631752610296068227631049216904161788520160322143593171775240320461092149917660662605701986747771454633898767686226483066638893617936911841632848203568714915335745864493692363635746852292644


-- product = m1 * m2
product_ :: Integer
product_ = 73489197152587525950911239393504916219894331642766806004963014089380843432195243064650950725735481460509094611522295092055770560307387857603368879348291701229728966401279102049844289924300640684284366798153502354796896896789500754665177370547265657660921661348954697621591995329144558811859059610295069494174480001276465541714100797265562597603236581242855576960516431746151107058257278577140458016254966823740042307728662280978159201267253853999503828702379167682409828123857629229955705471572258926558581312076618614684485815303048091397149707469682032337796465311176700775100650900039996649029596050218367419981005802982147188580709879034300808759283724118145397460044620678510478322018857529574508058205275831726659557210548853060929777161014428520572172746633324314296228951501568978327494431374244743245980945231468397701533189130366366553092948556246055812517488168148665428737819961645830376900532915927644856011057363177745293943047124029890390319448042832931020046510356469033525122182841076500312770146567048119765328377936055337528694419941559705899197697390383624022786475802209926228978611576383420791661872963350795620350614914433481604524217036058199124855509570131769983609896542647260404117030780603687551983370268592938452959071917300385104538490679449100250496944988894241865163935499814144213289298617889266154447655373756315159490357539305144544065912729721816405846189671898413133337390625568905131097268431836395588689301958315955030629745079327826041207696434886870165317677666487994999034103185596560087639964840177504878058572801710210262151585243259942775100316411563631576876848954447449643890019726038925421598991162152238916887941636217169498340397422137660338702037207873015643659895423164883424409941132977359358881780045485551464536470065976004636048957424424550297103290258141019129326743319889053184167488269301320373110222688648114969124798202184897836679400225550162040407595799199365675946063912499960622285600146885543327918724954501137932157418050344723447475155965291632092585696092242955603044717506741980859416955022737393687697498847023129380025086038257134662057377882272690822186206716377376396613092557694452484603769984928080504956706081093267820465173131264671574221937097305601406321747932255793131742702103203711854253854267399279220524032465856576145179019880240272542415365948604022923552250124781199335519378663347376701175585828881518736974697617294201028360360703410431919207975689778003074311445778179531715420697109274246168678186003286976971589823545148455264586599728134311477753364424544158344505726024410834492664922229458733272

main :: IO ()
main = hspec $ do
    describe "Multiplication  ... this will take about a minute!" $ do
        it "-100 multiplied by 3 is -300" $ do" $ do
        it (show m1 ++ " ^ 10" ++ "\nmultiplied by\n" ++ show m1 ++ 
                " ^ 10" ++ "\nis\n" ++ show product_ ++ " ^ 10") $ do
            mulIntO (m1 ^ 10) (m2 ^ 10) `shouldBe` (product_ ^ 10)
        it "-100 multiplied by 3 is -300" $ do
            mulIntO (-100) 3 `shouldBe` (-300)
        it "100 multiplied by -3 is -300" $ do
            mulIntO 100 (-3) `shouldBe` (-300)
        it "-100 multiplied by -3 is 300" $ do
            mulIntO (-100) (-3) `shouldBe` 300

