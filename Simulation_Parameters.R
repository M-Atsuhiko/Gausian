# 2014/05/23
# 論文[Systematic mapping between dendritic function and structure]
# BENJAMIN TORBEN-NIELSEN et al 2009
# inverse approachの中の、確率的に神経細胞形態を生成する部分
# パラメータ設定ヘッダ

# これらの値は暫定的なもの

# [論文で不明な点と改善方法]
#
# 1、Stem以外のlengthの算出方法が載ってない -> 他の参考文献からすべてのセグメントの長さは一定にする
# 2、Diameterの計算方法がおかしい。論文の方法では負の値になる(d_(i) = d_(i - 1)*Taper_rate) -> d_(i) = d_(i - 1) + d_(i - 1)Taper_rateにする。
# 3、Synaptic zoneの定義の仕方があいまい。somaからy軸方向にy > UPPER_SYNAPTIC_ZONE_Y(正の値)となる範囲をred synapse zone、somaからy < LOWER_SYNAPTIC_ZONE_Y(負の値)となる範囲をblue synaptic zoneとする

# [改良できそうな点]
#
# 1、GAの遺伝子をより直接的に遺伝子としてあつかう
# 2、遺伝子の、コンダクタンスと形態の比率を変えてみる。より電気的特性を詳しくする。
# 3、確率分布の選び方、これによって作成される形態が変わりそう

# プログラム中の主なデータ構造
# 基本的にリスト形式で扱っている
#
# 1, Segment  : 三次元空間中に伸びる、樹状突起を構成する円柱状のパーツ。長さ、伸張角度、始点と終点の三次元座標などのパラメータをもつ
# 2, Stem     : somaから最初に伸びるSegment
# 3, Branch   : Stem以外のSegment
# 4, Dendrite : 一つのStemと、そこから伸びるBranchをあわせたもの
# 5, param    : StemとBranchは一組のパラメータ群から作成される、その元となるパラメータのひとまとまり
# 6, Dendrite : 作成されたStemとBranchの組、これを一つのDendriteとして扱う
# 7, TREE     : 複数のparamから作成された複数のDendriteを格納するlist
# 8, GENERATION : 1世代のparam(N_INDIVIDUAL個)を格納しているlist変数。最も外側の変数

# テスト実行かどうか、乱数のSEEDとΔtとコンダクタンスをコマンドライン引数から受け取れるようにする
# 引数は TRUE/FALSE SEED Δt[ms] コンダクタンス の順番にする
# TRUE/FALSEは関数として後で評価するので関数名としての"TRUE"、"FALSE"を与える
# is_testなので、TRUEの時はテスト実行をすることに注意

# 進化的アルゴリズムの評価方法的に、シミュレーションが行える個体が現れるまではΔtやALPHAは生成される形態に影響しない。よって
# シミュレーション不可の個体を評価する手法を変更しないかぎり、SEEDごとに初めてシミュレーションが行われる世代までを保存して、途中から探索を行うことができる
# 2014/05/23
# 論文[Systematic mapping between dendritic function and structure]
# BENJAMIN TORBEN-NIELSEN et al 2009
# inverse approachの中の、確率的に神経細胞形態を生成する部分
# パラメータ設定ヘッダ

# これらの値は暫定的なもの

# [論文で不明な点と改善方法]
#
# 1、Stem以外のlengthの算出方法が載ってない -> 他の参考文献からすべてのセグメントの長さは一定にする
# 2、Diameterの計算方法がおかしい。論文の方法では負の値になる(d_(i) = d_(i - 1)*Taper_rate) -> d_(i) = d_(i - 1) + d_(i - 1)Taper_rateにする。
# 3、Synaptic zoneの定義の仕方があいまい。somaからy軸方向にy > UPPER_SYNAPTIC_ZONE_Y(正の値)となる範囲をred synapse zone、somaからy < LOWER_SYNAPTIC_ZONE_Y(負の値)となる範囲をblue synaptic zoneとする

# [改良できそうな点]
#
# 1、GAの遺伝子をより直接的に遺伝子としてあつかう
# 2、遺伝子の、コンダクタンスと形態の比率を変えてみる。より電気的特性を詳しくする。
# 3、確率分布の選び方、これによって作成される形態が変わりそう

# プログラム中の主なデータ構造
# 基本的にリスト形式で扱っている
#
# 1, Segment  : 三次元空間中に伸びる、樹状突起を構成する円柱状のパーツ。長さ、伸張角度、始点と終点の三次元座標などのパラメータをもつ
# 2, Stem     : somaから最初に伸びるSegment
# 3, Branch   : Stem以外のSegment
# 4, Dendrite : 一つのStemと、そこから伸びるBranchをあわせたもの
# 5, param    : StemとBranchは一組のパラメータ群から作成される、その元となるパラメータのひとまとまり
# 6, Dendrite : 作成されたStemとBranchの組、これを一つのDendriteとして扱う
# 7, TREE     : 複数のparamから作成された複数のDendriteを格納するlist
# 8, GENERATION : 1世代のparam(N_INDIVIDUAL個)を格納しているlist変数。最も外側の変数

# テスト実行かどうか、乱数のSEEDとΔtとコンダクタンスをコマンドライン引数から受け取れるようにする
# 引数は TRUE/FALSE SEED Δt[ms] コンダクタンス の順番にする
# TRUE/FALSEは関数として後で評価するので関数名としての"TRUE"、"FALSE"を与える
# is_testなので、TRUEの時はテスト実行をすることに注意

# 進化的アルゴリズムの評価方法的に、シミュレーションが行える個体が現れるまではΔtやALPHAは生成される形態に影響しない。よって
# シミュレーション不可の個体を評価する手法を変更しないかぎり、SEEDごとに初めてシミュレーションが行われる世代までを保存して、途中から探索を行うことができる

args <- commandArgs()

is_test <- NA
args_SEED <- FALSE
args_delta_t <- FALSE
args_conductances <- NA
args_Function_ratio <- NA
args_Conductance_ratio <- NA
args_extra_prefix <- ""

if(!(is.na(args_start <- match("--args",args)))){
  ARGS <- args[(args_start + 1):length(args)]
  if(length(ARGS) < 2)
    stop("the number of arguments is less than 2")

  is_test <- eval(parse(text=ARGS[1]))
  args_SEED <- as.integer(ARGS[2])
  args_delta_t <- as.integer(ARGS[3])
  args_Function_ratio <- as.numeric(ARGS[4])
  args_Conductance_ratio <- as.numeric(ARGS[5])
  args_extra_prefix <- ARGS[6]
  
  if(length(ARGS) > 6){
    args_conductances <- ARGS[7:length(ARGS)]
  }
}

############### 乱数初期化 ###############
#RAND_SEED <- Sys.time()
RAND_SEED <- 1#デフォルト値
if(args_SEED){
  RAND_SEED <- args_SEED #もしSEED引数が与えられていればここで設定する
}

set.seed(RAND_SEED*10^(8))


DELTA_T                        <- 15 #デフォルト値
if(args_delta_t){
  DELTA_T <- args_delta_t
}

#導入するコンダクタンスを指定する "ca", "k"
Conductances <- c()
if(!(is.na(args_conductances[1]))) Conductances <- args_conductances

### 実行 Flag
THIS_IS_TEST <- TRUE #テストモードで実行
#THIS_IS_TEST <- FALSE
if(!(is.na(is_test))) THIS_IS_TEST <- is_test

WITH_K <- FALSE#デフォルト
WITH_Ca <- FALSE#デフォルト

if(!(is.na(match("k",Conductances)))) WITH_K <- TRUE
if(!(is.na(match("ca",Conductances)))) WITH_Ca <- TRUE

include_conductances <- c()
if(WITH_K) include_conductances <- c(include_conductances,"k")
if(WITH_Ca) include_conductances <- c(include_conductances,"ca")
if(WITH_K + WITH_Ca == 0) include_conductances <- "passive"

ALPHA                          <- 1                    # 評価値に対する形態の影響度。ここ結構結果に影響しそう(notice!)
#ALPHAを大きく(最大3くらい)すると、パフォーマンスよりも形態重視の結果になりそう

### マルチコアで走らせる
library("doParallel")
MAX_CORES <- detectCores()
USE_CORES <- MAX_CORES
registerDoParallel(cores=(USE_CORES))
library("compiler")

### GA用のパラメータ ###
MAX_GENERATION                 <- 500                    # GAのMAX世代数

N_INDIVIDUAL                   <- 200                    # 1世代の個体数
ES_RAMDA                       <- 10                     # 次世代生成に用いる優秀な個体の数

GA_CROSS_OVER                  <- 0.25                   # 一点交叉を起こす確率 一点交叉しなかった場合は世代n個体を上位からそのままコピーする

 # MUTATION PROBs#
GA_MUTATION_AR_G               <- 0.65                   # 各パラメータがmutationを起こす確率
MUTATION_SIGMA                 <- 0.05                   # mutationさせる際に用いる標準偏差値

#MUTATOINを起こす際のSDにパラメータによって差をつけてみる このへんも要調整か?
MUTATION_ANGLE_MIEW_SD          <- 2
MUTATION_ANGLE_SIGMA_SD         <- 0.5
MUTATION_LENGTH_MIEW_SD         <- 1
MUTATION_beta_SIGMA_SD          <- 3
MUTATION_alfa_taper_diam_SD     <- 0.1
MUTATION_K_peak                 <- 0.001
MUTATION_Ca_peak                <- 5*10^(-5)
MUTATION_conductance_Gaus_mean  <- 0.05
MUTATION_conductance_Gaus_sd    <- 0.05

MUTATION_taper_rate             <- 0.005

MUTATION_CONDUCTANCE_SIGMA     <- 0.05*10^(-3)            # 標準偏差0.05では大きすぎるため
GA_DELETE                      <- 0.15                   # 樹状突起を減らす確率
GA_INTRO                       <- 0.15                   # 樹状突起を増やす

set_select_prob <- function(N,prob){
  if(N > 1) return(c(prob/2,set_select_prob(N - 1,prob/2)))
  else return(prob)
}
SELECT_PROB <- set_select_prob(ES_RAMDA,1)

###   st_mol用のパラメータ  ###
N_DENDRITE                     <- 2
#MAX_N_DEND                     <- 4                      # 一つのNEURONから伸びるDENDRITEの最大本数
#MIN_N_DEND                     <- 2                      # 一つのNEURONから伸びるDENDRITEの最小本数
FOURCE_MAX_LENG                <- 2000                   # [μm] 強制的に終端させる長さ
FOURCE_MIN_DIAM                <- 0.15                   # [μm] 強制的に終端させる太さ この太さ以下のBranchは認めない

MAX_PEAK                       <- 0.8                    # (Scaled γのピークの値) 

SOMA_DIAM                      <- 25                     # 球形のsomaの直径
SOMA_LENGTH                    <- 25                     # NEURONで用いる時に、球の表面積と円柱の面積が等しくなるようにする

TAPER_RATE                     <- -0.125                 # 論文では d_(i) = d_(i - 1)*Taper_rate の計算になってるけど、これはおかしい
                                                         # Burke(1992)では d(i) = d(i - 1) + Taper_rate

AXIS_LENG                      <- 100                    #y'軸、z'軸を例示する際の長さ

SOMA_COLOR                     <- "green"                #Somaを図示する際の色
SOMA_TEXTURE                   <- "temp.png"          #Somaの球に貼る画像
STEM_COLOR                     <- "blue"                 #Stemを図示する際の色
BRANCH_COLOR                   <- "green"                #Branchを図示する際の色

OUTPUT_MOVIE                   <- "./movie/st_mol_output_movie" #gifデータ出力ファイル名

UPPER_SYNAPTIC_ZONE_COLOR      <- "red"
LOWER_SYNAPTIC_ZONE_COLOR      <- "blue"

UPPER_SYNAPTIC_ZONE_INDEX      <- 1
LOWER_SYNAPTIC_ZONE_INDEX      <- 0

UPPER_SYNAPTIC_ZONE_Y          <- 170.0                 # [μm] somaからの距離、red synapseを作る範囲を変更する
LOWER_SYNAPTIC_ZONE_Y          <- -170.0                # [μm] somaからの距離、blue synapseを作る範囲を変更する

SYNAPTIC_ZONE_DEPTH            <- 20.0                  # [μm] synaptic zoneの深さ

UPPER_SYNAPTIC_ZONE_BOUNDARY_Y <- UPPER_SYNAPTIC_ZONE_Y + SYNAPTIC_ZONE_DEPTH 
LOWER_SYNAPTIC_ZONE_BOUNDARY_Y <- LOWER_SYNAPTIC_ZONE_Y - SYNAPTIC_ZONE_DEPTH

SYNAPTIC_SHOW_SQUARE           <- 300.0                 # [μm] synaptic zoneを図示する際に用いるxy平面に平行な正方形の一辺の長さ
ALP                            <- 0.3                   # synaptic zoneを図示する際の透明度

SYNAPSE_RADIUS                 <- 2.5                   # [μm] synapseを球で表示する際の半径 DENDRITEが太い場合、シナプスは埋もれて見えなくなることがある

#コンダクタンス関係
K_MAX                          <- 0.22
Ca_MAX                         <- 0.0022

K_RANGE                        <- c(10^(-7),K_MAX)              #Kコンダクタンスの取りうる範囲 論文値では0が入っているが、Stemのコンダクタンスが0になると、それ以降の枝のコンダクタンスが全て0になるので、わずかに残しておくことにする
Ca_RANGE                       <- c(10^(-8),Ca_MAX)             #Caコンダクタンスの取りうる範囲


F_MAX                          <- 2 # Fのおおよその最大値 Fを正規化したほうがよさそう

GAUS_MEAN_MAX                  <- 1
GAUS_MEAN_MIN                  <- 0

#gamma分布のパラメータの範囲(11/26)
MIN_GAMMMA_ALFA                <- 1
MIN_GAMMMA_BETA                <- 1

### NEURON用のパラメータ ###

SIMULATION_SCRIPT              <- "./simulation.sh" #シミュレーションを実行するスクリプトファイル
SIM_TEMPLATE                   <- "./sim_template2.hoc" #(12/12)シミュレーションを実行するhocファイル
DEFINE_TEMP                    <- "./define_template.hoc"

     # ファイル出力先
ROOT_OUTPUT_DIR                <- "./Datas/" #最も大本のデータ出力先
Dir_MorphoSyn_Data             <- paste(ROOT_OUTPUT_DIR,"MorphoSynapseHoc/",sep="")
Dir_SimOut                     <- paste(ROOT_OUTPUT_DIR,"SimOut/",sep="")
Dir_SimHoc                     <- paste(ROOT_OUTPUT_DIR,"SimHoc/",sep="")
Dir_Test                       <- paste(ROOT_OUTPUT_DIR,"test/",sep="")

     # ファイルprefixes

OUTPUT_MORPHO_FILE             <- paste(Dir_MorphoSyn_Data,"morpho",sep="") #morphology用データ出力ファイル名prefix
OUTPUT_SYNAPSE_FILE            <- paste(Dir_MorphoSyn_Data,"synapse",sep="") #synapse用データ出力ファイル名prefix
OUTPUT_SIM_SOMA                <- paste(Dir_SimOut,"sim_soma",sep="")       #シミュレーション結果出力用ファイル名prefix

     # main.Rで使用するファイルの用意
MORPHO_FILE_NAME               <- paste(OUTPUT_MORPHO_FILE,1:N_INDIVIDUAL,".hoc",sep="")
SYNAPSE_FILE_NAME              <- paste(OUTPUT_SYNAPSE_FILE,1:N_INDIVIDUAL,".hoc",sep="")

SIMPLE_SYNAPSE_FILE_NAME              <- paste(OUTPUT_SYNAPSE_FILE,".hoc",sep="")

OUTPUT_UPPER_LOWER_FILE_NAME   <- paste(OUTPUT_SIM_SOMA,1:N_INDIVIDUAL,"ul.data",sep="")
OUTPUT_LOWER_UPPER_FILE_NAME   <- paste(OUTPUT_SIM_SOMA,1:N_INDIVIDUAL,"lu.data",sep="")
OUTPUT_UPPER_TEST_FILE_NAME    <- paste(OUTPUT_SIM_SOMA,1:N_INDIVIDUAL,"test_u.data",sep="")
OUTPUT_LOWER_TEST_FILE_NAME    <- paste(OUTPUT_SIM_SOMA,1:N_INDIVIDUAL,"test_l.data",sep="")

Round_DIGITS                   <- 9                         # 形態情報を出力する際の丸め桁
Morpho_Round_DIGITS            <- 4                         # 形態情報を出力する際の丸め桁
Conductance_Round_DIGITS       <- 9                         # コンダクタンスを出力する際の丸め桁

COMP_MAX_SIZE                  <- 5.0                   # マルチコンパートメントモデルを作る際の1コンパートメントの最大サイズ

SOMA                           <- "soma"                # hocファイル出力で使用する名前
DEND                           <- "dend"                # hocファイル出力で使用する名前
UPPER_SYNAPSE                  <- "upperSyn"            # hocファイルに出力する名前
LOWER_SYNAPSE                  <- "lowerSyn"            # hocファイルに出力する名前
#(12/12) シナプスの種類はsim_template2に埋め込みになっている
#SYN_TYPE_NAME                  <- "simulSyn"            # 自作した、テスト用のシナプスの名前
SYN_TYPE_NAME                  <- "Exp2Syn"              # シナプスを変更してみる

PAS                            <- "pas"                 # hocファイルで使用するleak電流の名前
G_PAS                          <- "g_pas"               # pasのコンダクタンス
E_PAS                          <- "e_pas"               # pasの反転電位

Ka                             <- "borgka"              # hocファイルで使用するpotasium電流の名前
G_K                            <- "gkabar_borgka"       # K電流のコンダクタンス

CaT                            <- "cat"                 # hocファイルで使用するcalcium電流の名前
G_Ca                           <- "gcatbar_cat"         # K電流のコンダクタンス

g_pas                          <- 2.5e-5                # S(cm^2) 論文値とは異なるが、コードレベルではこう書いてある。
e_pas                          <- -70.0                 # mV

Ra                             <- 100                   # Ωcm^2
cm                             <- 0.8                   # μFcm^2 <- μF/cm^2では?
V_INIT                         <- -70.0                 # [mV] 膜電位の初期値
V_REST                         <- -70.0                 # 静止膜電位
celsius                        <- 37                    # 温度 

cai                            <- 50*10^(-6)            # 細胞内caイオン濃度
cao                            <- 2                     # 細胞外caイオン濃度

ek                             <- -91

# synapse のパラメータ
tau1                           <- 0.5                   # rise time
tau2                           <- 2                     # decay time

G_SYN                          <- 0.5*10^(-3)


FIRST_ACTIVATE_TIME            <- 5 #最初にシナプス活性化をさせる時間
NOT_ACTIVATE_TIME              <- -100 #シナプスを活性化させない場合の時間

if(WITH_Ca && !(WITH_K)){
  SIM_TIME                     <- 100 #シミュレーションの長さ
}else{
  SIM_TIME                     <- 50 #シミュレーションの長さ
}

#今はSIMUL_PARAMETER_FILEの変数に関わらず、sim_templateファイルに埋め込みの値を使っているので注意
SIMUL_PARAMETER_FILE           <- "./sim_parameter.hoc" # シミュレーションで用いるパラメータ、(cai、cao、celsiusなど)をまとめたファイル

### 評価関数に関わる部分 ###
Function_ratio                 <- 50#デフォルト値
Conductance_ratio              <- 0
if(!(is.na(args_Function_ratio)) && !(is.na(args_Conductance_ratio))){
  Function_ratio                 <- args_Function_ratio
  Conductance_ratio                 <- args_Conductance_ratio
}
Morphology_ratio               <- 100 - (Function_ratio + Conductance_ratio*(WITH_K || WITH_Ca))

BAD_RATIO                      <- 0.9                    # Non-pref/Pref の比がこの値を上回ったら、「よくない結果」として負の値をとる評価式を用いる

L0                             <- abs(UPPER_SYNAPTIC_ZONE_Y) + abs(LOWER_SYNAPTIC_ZONE_Y) #両方のSYNAPTIC_ZONEにシナプスを形成するのに必要とする最小の枝の長さ

V0                             <- abs(LOWER_SYNAPTIC_ZONE_Y)*((FOURCE_MIN_DIAM/2)^2)*pi + abs(UPPER_SYNAPTIC_ZONE_Y)*((FOURCE_MIN_DIAM/2)^2)*pi

### 形態ヒューリスティック ###
N_MIN_BRANCHS                  <- 15                    # 一つのNeuronが持つべき最小のBranch(Segment)数

#ペナルティの値は結構調整が必要そう
MORPHO_PENALTY_MIEW            <- -100                   # 形態的異常 シミュレーションをする対象にならなかった場合はガウス分布に従う乱数で評価値を与える
MORPHO_PENALTY_SIGMA           <- 5

### シミュレーションヒューリスティックス ###
MIN_EPSP                       <- 2                     # [mV] upper lowerからのEPSPの最小値、これを超えないと、estimateはpenaltyになる
EPSP_ERROR_MESSAGE             <- "EPSP_ERROR!"         # EPSPエラーの時にファイルに出力するメッセージ
MAX_MEMBRANE                   <- -45                   # [mV] 最大の膜電位、発火を起こさないためにこれを超えてはいけないことにする
NOT_ACTIVATE_STEP              <- 5                     # 膜電位が静止電位に向かっているかを判定するために用いるシミュレーションの最終ステップ数
EPSP_PENALTY_MIEW              <- -50                   # EPSPの異常の場合に与えるpenalty評価値 ガウス関数から与える 
EPSP_PENALTY_SIGMA             <-   5

Mor_E                          <- "Morphology_E!"
EPSP_E                         <- "EPSP_E!"
Bad_Result                     <- "Bad_Result!"
Good_Result                    <- "Good_Result!"

Dend_Length <- 200

Segment_Labels <- c(
#     Stemデータ構造
#-----------------------
# segmentデータはリスト形式で表し、
# [[1]]:
  "No",
# [[2]]:
  "length",
# [[3]]:
  "elevation",
# [[4]]:
  "rotation",
# [[5]]:
  "diam",
# [[6]]:
  "path_leng",
# [[7]]:
  "parent",
# [[8]]:
  "connect",#(ベクトル)、結合しているBranchの番号
# [[9]]:
  "coordi", #そのsegmentの始点[1,]と終点[2,]を表す座標をもった2x3行列
# [[10]]:
  "nseg", #length/5.0 を奇数に繰り上げした値
# [[11]]:
  "synapse", #synapsを形成している位置をcoordi[1,]からcoordi[2,]を1とした比で表す。この比とUPPER_ZENE(UPPER_SYNAPTIC_ZONE_COLOR)かLOWER_ZONE(LOWER_SYNAPTIC_ZONE_COLOR)かの情報をもつ行列形式で格納する。シナプスがなければ -1(int)にしておく
# [[12]]
  "K_conductance",
# [[13]]  
  "Ca_conductance"
  )

Simple_Param_Labels <- c(
#[[1]]
  "Stem_diameter",#非負
#[[2]]
  "Taper",#非負
#[[2]]
  "K_Conductances",#非負
#[[3]]
  "Ca_Conductances"#非負
  )

Gausian_Param_Labels <- c(
### Stem parameter ###
#[[1]]:
  "Stem_elevation_MIEW",
#[[2]]  
  "Stem_rotation_MIEW",
#[[3]]    
  "Stem_diameter",#非負
#[[4]]    
  "Length_MIEW",#非負
#[[5]]    
  "Branch_rotation_MIEW",
#[[6]]    
  "Branch_rotation_SIGMA",#非負
#[[7]]    
  "Branch_elevation_MIEW",
#[[8]]    
  "Branch_elevation_SIGMA",#非負
#[[9]]    
  "Bif_alfa",#非負
#[[10]]    
  "Bif_beta",#非負
#[[11]]    
  "Trm_alfa",#非負
#[[12]]    
  "Trm_beta",#非負
#[[13]]
  "K_peak",#非負
#[[14]]
  "K_Gaus_mean",#[0,1]の値
#[[15]]
  "K_Gaus_sd",#非負
#[[16]]
  "Ca_peak",#非負
#[[17]]
  "Ca_Gaus_mean",#[0,1]の値
#[[18]]
  "Ca_Gaus_sd"#非負
  )

Tsuishi_Param_Labels <- c(
### Stem parameter ###
#[[1]]:
  "Stem_elevation_MIEW",
#[[2]]  
  "Stem_rotation_MIEW",
#[[3]]    
  "Stem_diameter",#非負
#[[4]]    
  "Length_MIEW",#非負
#[[5]]    
  "Branch_rotation_MIEW",
#[[6]]    
  "Branch_rotation_SIGMA",#非負
#[[7]]    
  "Branch_elevation_MIEW",
#[[8]]    
  "Branch_elevation_SIGMA",#非負
#[[9]]    
  "Bif_alfa",#非負
#[[10]]    
  "Bif_beta",#非負
#[[11]]    
  "Trm_alfa",#非負
#[[12]]    
  "Trm_beta",#非負
#[[13]]
  "K_Stem_conductance",#非負
#[[14]]
  "K_taper",#非負
#[[15]]
  "Ca_Stem_conductance",#非負
#[[16]]
  "Ca_taper"#非負
  )  

RERATIVE_MULTI_GENERATION_Labels <- c(
  #INDIVIDUALのラベル
  #[[1]]: INDIVIDUALの番号
  "ID_IND",
  #[[2]]: 形態情報出力hocファイル
  "Morpho_file_name",
  #[[3]]: シナプス情報出力hocファイル
  "Synapse_file_name",
  #[[3]]: シミュレーション結果(upper -> lower)出力ファイル
  "Output_upper_lower_file_name",
  #[[4]]: シミュレーション結果(lower -> upper)出力ファイル
  "Output_lower_upper_file_name",
  #[[5]]: テストシミュレーション(upper)結果出力ファイル名
  "Output_upper_test_file_name",
  #[[6]]: テストシミュレーション(lower)結果出力ファイル名
  "Output_lower_test_file_name",
  #[[7]]: TREE生成に用いるパラメータセット
  "Params",
  #[[8]]: TREE生成時に用いるseed 世代＊個体で一意に定まるようにする
  "Seed", 
  #[[9]]木の形態、座標、シナプス情報は未設定
  "TREE",
  #[[10]]木の大きさ
  "TREE_Volume",
  #[[11]]Caコンダクタンス量
  "Ca_Amount",
  #[[12]]Kコンダクタンス量
  "K_Amount",
  #[[13]]機能性
  "Ratio",
  #[[14]]評価値
  "Estimate",
  #[[15]]前世代で親になった個体の番号
  "Parent",
  #[[16]]順位
  "Rank",
  #[[17]]シミュレーションの結果
  "Result"
  )

MULTI_GENERATION_Labels <- c(
  #INDIVIDUALのラベル
  #[[1]]: INDIVIDUALの番号
  "ID_IND",
  #[[2]]: 形態情報出力hocファイル
  "Morpho_file_name",
  #[[3]]: シナプス情報出力hocファイル
  "Synapse_file_name",
  #[[3]]: シミュレーション結果(upper -> lower)出力ファイル
  "Output_upper_lower_file_name",
  #[[4]]: シミュレーション結果(lower -> upper)出力ファイル
  "Output_lower_upper_file_name",
  #[[5]]: テストシミュレーション(upper)結果出力ファイル名
  "Output_upper_test_file_name",
  #[[6]]: テストシミュレーション(lower)結果出力ファイル名
  "Output_lower_test_file_name",
  #[[7]]: TREE生成に用いるパラメータセット
  "Params",
  #[[8]]: TREE生成時に用いるseed 世代＊個体で一意に定まるようにする
  "Seed", 
  #[[9]]木の形態、座標、シナプス情報は未設定
  "TREE",
#  "Soma_Conductance",
  #[[10]]評価値
  "Estimate",
  #[[11]]機能性
  "Ratio",
  #[[12]]前世代で親になった個体の番号
  "Parent",
  #[[14]]順位
  "Rank"
  )

Soma_Conductance_Labels <- c(
  "K_conductance",
  "Ca_conductance"
  )

TREE_Labels <- c(
  "Upper_Dend",
  "Lower_Dend",
  "Other_Dend"
  )
