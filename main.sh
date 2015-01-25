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
# excuse time   | server| type     | seed   | dt     | suffix               |
# ------------------------------------------------------------------------------
# (1/21 13:15~)  | hal1  | k ca    | (1:10) | (5:30) | Rerative_Gaus_75_0   | F:75 C:0
# (1/25 15:35~)  | hal2  | k       | (1:10) | (5:30) | Rerative_liner_75_5  | F:75 C:5
# (1/22 11:50~)  | hal3  | k ca    | (1:10) | (5:30) | Rerative_liner_75_0  | F:75 C:0
# (1/23 12:40~)  | hal4  | k ca    | (1:10) | (5:30) | Rerative_liner_75_5  | F:75 C:5

#あと回すやつ
# k_ca_Rerative_Gaus_75_5

# (12/17)  | iMac  | passive | ()           | F:75

SEED=(1 2 3 4 5 6 7 8 9 10)
DELTA_T=(5 10 15 20 25 30)
CONTAIN_CONDUCTANCES="k"
Function_Ratio="75"                   #ここでFunction_Ratioを変更できる
Conductance_Ratio="5"
Spesific="Rerative_liner_"${Function_Ratio}"_"${Conductance_Ratio}     #その他のprefix

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
