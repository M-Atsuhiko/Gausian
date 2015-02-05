#! /bin/bash
#author: Atsuhiko Murakami

IS_TEST="FALSE"

if [ -n "$1" ]
then
    IS_TEST="TRUE"
fi

# mmmmmm m    m mmmmmm   mmm  m    mmmmmmmm mmmmmm
# #       #  #  #      m"   " #    #   #    #     
# #mmmmm   ##   #mmmmm #      #    #   #    #mmmmm
# #       m""m  #      #      #    #   #    #     
# #mmmmm m"  "m #mmmmm  "mmm" "mmmm"   #    #mmmmm


# k_caはSEED = 2が妙に時間がかかる

# 実行中
# excuse time  | server| type     | seed   | dt     | suffix               |
# ------------------------------------------------------------------------------
# (2/3 11:00~) | hal1  | ca       | (1:10) | (5:30) | Rerative_Gaus_st50_75_0   | F:75 C:0 tstopを50msに
# (2/3 11:00~) | hal2  | ca       | (1:10) | (5:30) | Rerative_Gaus_st50_75_5  | F:75 C:5 tstopを50msに
# (2/3 11:00~) | hal3  | ca       | (10) | (5:30) | Rerative_Gaus_st50_75_0  | F:75 C:0 tstopを50msに
# (2/3 11:00~) | hal4  | ca       | (10) | (5:30) | Rerative_Gaus_st50_75_5  | F:75 C:5 tstopを50msに

#ほかのパターンもなるべく多く回す

SEED=(10)
DELTA_T=(30 25 20 15 10 5)
CONTAIN_CONDUCTANCES="ca"
Function_Ratio="75"                   #ここでFunction_Ratioを変更できる
Conductance_Ratio="5"
Spesific="Rerative_Gaus_st50_"${Function_Ratio}"_"${Conductance_Ratio}     #その他のprefix

for se in ${SEED[@]}
do
    for dt in ${DELTA_T[@]}
    do
	echo "executed date: "
	date
	time R --vanilla --slave --args ${IS_TEST} ${se} ${dt} ${Function_Ratio} ${Conductance_Ratio} ${Spesific} ${CONTAIN_CONDUCTANCES} < main.R

	if [ ${IS_TEST} != "FALSE" ]
	then
	    exit 0
	fi
    done
done
