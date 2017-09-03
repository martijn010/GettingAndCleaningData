# Codebook
## Data that is used
The original data comes from "Human Activity Recognition Using Smartphones Data Set". Information at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#. 
Download the data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.

## tidydata.txt
After running the run_analysis.R script the tidydata.txt dataset is acquired. This data contains a number of variables listed below. Information on the exact meaning of the variables can be found in the features_info.txt file within the original UCI HAR Dataset dataset.

rn |"column.names" | "class" | "range" | "mean"
|---|---			|---	  |---		|---   |
"1" | "Subject" | "numeric" | "1  /  30" | "15.5"
"2" | "Activity" | "character" | "LAYING / SITTING / STANDING / WALKING / WALKING_DOWNSTAIRS / WALKING_UPSTAIRS" | "Not available"
"3" | "TimeBodyAccelerometerMeanX" | "numeric" | "0.22159824394  /  0.3014610196" | "0.274302742245795"
"4" | "TimeBodyAccelerometerMeanY" | "numeric" | "-0.0405139534294  /  -0.00130828765170213" | "-0.0178755238674415"
"5" | "TimeBodyAccelerometerMeanZ" | "numeric" | "-0.152513899520833  /  -0.07537846886" | "-0.109163815804519"
"6" | "TimeBodyAccelerometerSDX" | "numeric" | "-0.996068635384615  /  0.626917070512821" | "-0.557690076404401"
"7" | "TimeBodyAccelerometerSDY" | "numeric" | "-0.990240946666667  /  0.616937015333333" | "-0.460462635378301"
"8" | "TimeBodyAccelerometerSDZ" | "numeric" | "-0.987658662307692  /  0.609017879074074" | "-0.575560246148636"
"9" | "TimeGravityAccelerometerMeanX" | "numeric" | "-0.680043155060241  /  0.974508732" | "0.697477505882702"
"10" | "TimeGravityAccelerometerMeanY" | "numeric" | "-0.479894842941176  /  0.956593814210526" | "-0.0162128361521394"
"11" | "TimeGravityAccelerometerMeanZ" | "numeric" | "-0.49508872037037  /  0.9578730416" | "0.0741278709325255"
"12" | "TimeGravityAccelerometerSDX" | "numeric" | "-0.996764227384615  /  -0.829554947808219" | "-0.96375253077172"
"13" | "TimeGravityAccelerometerSDY" | "numeric" | "-0.99424764884058  /  -0.643578361424658" | "-0.952429559765945"
"14" | "TimeGravityAccelerometerSDZ" | "numeric" | "-0.990957249538462  /  -0.610161166287671" | "-0.93640104156585"
"15" | "TimeBodyAccelerometerJerkMeanX" | "numeric" | "0.0426880986186441  /  0.130193043809524" | "0.0794735599203562"
"16" | "TimeBodyAccelerometerJerkMeanY" | "numeric" | "-0.0386872111282051  /  0.056818586275" | "0.00756520996888408"
"17" | "TimeBodyAccelerometerJerkMeanZ" | "numeric" | "-0.0674583919268293  /  0.0380533591627451" | "-0.00495340328183431"
"18" | "TimeBodyAccelerometerJerkSDX" | "numeric" | "-0.994604542264151  /  0.544273037307692" | "-0.594946699510964"
"19" | "TimeBodyAccelerometerJerkSDY" | "numeric" | "-0.989513565652174  /  0.355306716915385" | "-0.565414714340423"
"20" | "TimeBodyAccelerometerJerkSDZ" | "numeric" | "-0.993288313333333  /  0.0310157077775926" | "-0.735957689241115"
"21" | "TimeBodyGyroscopeMeanX" | "numeric" | "-0.205775427307692  /  0.19270447595122" | "-0.0324371599031218"
"22" | "TimeBodyGyroscopeMeanY" | "numeric" | "-0.204205356087805  /  0.0274707556666667" | "-0.0742595723452297"
"23" | "TimeBodyGyroscopeMeanZ" | "numeric" | "-0.0724546025804878  /  0.179102058245614" | "0.0874446468695526"
"24" | "TimeBodyGyroscopeSDX" | "numeric" | "-0.994276591304348  /  0.267657219333333" | "-0.691639902777431"
"25" | "TimeBodyGyroscopeSDY" | "numeric" | "-0.994210471914894  /  0.476518714444444" | "-0.653302029911363"
"26" | "TimeBodyGyroscopeSDZ" | "numeric" | "-0.985538363333333  /  0.564875818162963" | "-0.616435294332593"
"27" | "TimeBodyGyroscopeJerkMeanX" | "numeric" | "-0.157212539189362  /  -0.0220916265065217" | "-0.0960567959204382"
"28" | "TimeBodyGyroscopeJerkMeanY" | "numeric" | "-0.0768089915604167  /  -0.0132022768074468" | "-0.0426927819752453"
"29" | "TimeBodyGyroscopeJerkMeanZ" | "numeric" | "-0.0924998531372549  /  -0.00694066389361702" | "-0.0548018825799509"
"30" | "TimeBodyGyroscopeJerkSDX" | "numeric" | "-0.99654254057971  /  0.179148649684615" | "-0.703632714557601"
"31" | "TimeBodyGyroscopeJerkSDY" | "numeric" | "-0.997081575652174  /  0.295945926186441" | "-0.763551835158898"
"32" | "TimeBodyGyroscopeJerkSDZ" | "numeric" | "-0.995380794637681  /  0.193206498960417" | "-0.709559184010004"
"33" | "TimeBodyAccelerometerMagnitudeMean" | "numeric" | "-0.986493196666667  /  0.644604325128205" | "-0.49728966685894"
"34" | "TimeBodyAccelerometerMagnitudeSD" | "numeric" | "-0.986464542615385  /  0.428405922622222" | "-0.543908670845839"
"35" | "TimeGravityAccelerometerMagnitudeMean" | "numeric" | "-0.986493196666667  /  0.644604325128205" | "-0.49728966685894"
"36" | "TimeGravityAccelerometerMagnitudeSD" | "numeric" | "-0.986464542615385  /  0.428405922622222" | "-0.543908670845839"
"37" | "TimeBodyAccelerometerJerkMagnitudeMean" | "numeric" | "-0.99281471515625  /  0.434490400974359" | "-0.607929591545179"
"38" | "TimeBodyAccelerometerJerkMagnitudeSD" | "numeric" | "-0.994646916811594  /  0.450612065720513" | "-0.584175609709768"
"39" | "TimeBodyGyroscopeMagnitudeMean" | "numeric" | "-0.980740846769231  /  0.418004608615385" | "-0.565163077212988"
"40" | "TimeBodyGyroscopeMagnitudeSD" | "numeric" | "-0.981372675614035  /  0.299975979851852" | "-0.630394720315622"
"41" | "TimeBodyGyroscopeJerkMagnitudeMean" | "numeric" | "-0.997322526811594  /  0.0875816618205128" | "-0.736369300428253"
"42" | "TimeBodyGyroscopeJerkMagnitudeSD" | "numeric" | "-0.997666071594203  /  0.250173204117966" | "-0.755015188509002"
"43" | "FrequencyBodyAccelerometerMeanX" | "numeric" | "-0.995249932641509  /  0.537012022051282" | "-0.575799983503946"
"44" | "FrequencyBodyAccelerometerMeanY" | "numeric" | "-0.989034304057971  /  0.524187686888889" | "-0.488732713013952"
"45" | "FrequencyBodyAccelerometerMeanZ" | "numeric" | "-0.989473926666667  /  0.280735952206667" | "-0.62973875362598"
"46" | "FrequencyBodyAccelerometerSDX" | "numeric" | "-0.996604570307692  /  0.658506543333333" | "-0.552201112392524"
"47" | "FrequencyBodyAccelerometerSDY" | "numeric" | "-0.990680395362319  /  0.560191344" | "-0.481478729871355"
"48" | "FrequencyBodyAccelerometerSDZ" | "numeric" | "-0.987224804307692  /  0.687124163703704" | "-0.582361415029381"
"49" | "FrequencyBodyAccelerometerMeanFreqX" | "numeric" | "-0.635913046346154  /  0.159123629063636" | "-0.23226609715376"
"50" | "FrequencyBodyAccelerometerMeanFreqY" | "numeric" | "-0.379518455061538  /  0.466528231788462" | "0.0115288797872382"
"51" | "FrequencyBodyAccelerometerMeanFreqZ" | "numeric" | "-0.520114793584906  /  0.402532553395833" | "0.0437174260645842"
"52" | "FrequencyBodyAccelerometerJerkMeanX" | "numeric" | "-0.994630797358491  /  0.474317256051282" | "-0.613928222283428"
"53" | "FrequencyBodyAccelerometerJerkMeanY" | "numeric" | "-0.989398823913043  /  0.276716853307692" | "-0.588163069360073"
"54" | "FrequencyBodyAccelerometerJerkMeanZ" | "numeric" | "-0.992018447826087  /  0.157775692377778" | "-0.714358487490646"
"55" | "FrequencyBodyAccelerometerJerkSDX" | "numeric" | "-0.995073759245283  /  0.476803887476923" | "-0.612103283207987"
"56" | "FrequencyBodyAccelerometerJerkSDY" | "numeric" | "-0.990468082753623  /  0.349771285415897" | "-0.570730968650136"
"57" | "FrequencyBodyAccelerometerJerkSDZ" | "numeric" | "-0.993107759855072  /  -0.00623647528983051" | "-0.756489426411787"
"58" | "FrequencyBodyAccelerometerJerkMeanFreqX" | "numeric" | "-0.576044001875  /  0.331449281481482" | "-0.0691017912141093"
"59" | "FrequencyBodyAccelerometerJerkMeanFreqY" | "numeric" | "-0.601971415384615  /  0.195677336307692" | "-0.228102065671109"
"60" | "FrequencyBodyAccelerometerJerkMeanFreqZ" | "numeric" | "-0.62755547372549  /  0.230107945944444" | "-0.137602308791712"
"61" | "FrequencyBodyGyroscopeMeanX" | "numeric" | "-0.99312260884058  /  0.474962448333333" | "-0.636739605053057"
"62" | "FrequencyBodyGyroscopeMeanY" | "numeric" | "-0.994025488297872  /  0.328817010088889" | "-0.676686800745998"
"63" | "FrequencyBodyGyroscopeMeanZ" | "numeric" | "-0.985957788  /  0.492414379822222" | "-0.604391244378742"
"64" | "FrequencyBodyGyroscopeSDX" | "numeric" | "-0.994652185217391  /  0.196613286661538" | "-0.711035658050846"
"65" | "FrequencyBodyGyroscopeSDY" | "numeric" | "-0.994353086595745  /  0.646233637037037" | "-0.645433416234092"
"66" | "FrequencyBodyGyroscopeSDZ" | "numeric" | "-0.986725274871795  /  0.522454216314815" | "-0.657746585870822"
"67" | "FrequencyBodyGyroscopeMeanFreqX" | "numeric" | "-0.395770150677419  /  0.249209411510602" | "-0.104551025495773"
"68" | "FrequencyBodyGyroscopeMeanFreqY" | "numeric" | "-0.666814815306122  /  0.273141323315789" | "-0.167407475856434"
"69" | "FrequencyBodyGyroscopeMeanFreqZ" | "numeric" | "-0.507490866734694  /  0.3770740968" | "-0.0571809440547551"
"70" | "FrequencyBodyAccelerometerMagnitudeMean" | "numeric" | "-0.986800645362319  /  0.586637550769231" | "-0.536516692548498"
"71" | "FrequencyBodyAccelerometerMagnitudeSD" | "numeric" | "-0.987648484461539  /  0.178684580868889" | "-0.620963293005196"
"72" | "FrequencyBodyAccelerometerMagnitudeMeanFreq" | "numeric" | "-0.312338030213846  /  0.435846931652174" | "0.0761281754555899"
"73" | "FrequencyBodyBodyAccelerometerJerkMagnitudeMean" | "numeric" | "-0.993998275797101  /  0.538404846128205" | "-0.575617493234432"
"74" | "FrequencyBodyBodyAccelerometerJerkMagnitudeSD" | "numeric" | "-0.994366667681159  /  0.316346415348718" | "-0.599160868317743"
"75" | "FrequencyBodyBodyAccelerometerJerkMagnitudeMeanFreq" | "numeric" | "-0.125210388757581  /  0.488088499666667" | "0.162545885494571"
"76" | "FrequencyBodyBodyGyroscopeMagnitudeMean" | "numeric" | "-0.986535242105263  /  0.203979764835897" | "-0.667099099613148"
"77" | "FrequencyBodyBodyGyroscopeMagnitudeSD" | "numeric" | "-0.981468841692308  /  0.236659662496296" | "-0.672322349574843"
"78" | "FrequencyBodyBodyGyroscopeMagnitudeMeanFreq" | "numeric" | "-0.456638670923077  /  0.409521611525424" | "-0.0360322479939937"
"79" | "FrequencyBodyBodyGyroscopeJerkMagnitudeMean" | "numeric" | "-0.997617389275362  /  0.146618569064407" | "-0.756385271117363"
"80" | "FrequencyBodyBodyGyroscopeJerkMagnitudeSD" | "numeric" | "-0.99758523057971  /  0.287834616098305" | "-0.771517051737343"
"81" | "FrequencyBodyBodyGyroscopeJerkMagnitudeMeanFreq" | "numeric" | "-0.182923596577778  /  0.426301679855072" | "0.125922459004982"
"82" | "Angle(TimeBodyAccelerometerMean,Gravity)" | "numeric" | "-0.163042575021277  /  0.129153963587755" | "0.00655573583907996"
"83" | "Angle(TimeBodyAccelerometerJerkMean),GravityMean)" | "numeric" | "-0.120553975717391  /  0.203259965863014" | "0.000643885912133426"
"84" | "Angle(TimeBodyGyroscopeMean,GravityMean)" | "numeric" | "-0.389305120341463  /  0.444101172307692" | "0.0219317046182738"
"85" | "Angle(TimeBodyGyroscopeJerkMean,GravityMean)" | "numeric" | "-0.223672056052174  /  0.182384802705085" | "-0.0113731727087864"
"86" | "Angle(X,GravityMean)" | "numeric" | "-0.947116527659574  /  0.737784354819277" | "-0.524306549016646"
"87" | "Angle(Y,GravityMean)" | "numeric" | "-0.874567701929825  /  0.42476122745098" | "0.0786533502520241"
"88" | "Angle(Z,GravityMean)" | "numeric" | "-0.873649367  /  0.390444368518519" | "-0.0404362019002546"