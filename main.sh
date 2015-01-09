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

# 実行中
# excuse time   | server| type    | seed   | dt     | suffix               |
# ------------------------------------------------------------------------------
# (1/9 13:40~)  | hal1  | ca      | (1:10) | (5:30) | Determine_Gaus_100_0 | F:100 C:0
# (1/7 16:30~)  | hal2  | ca      | (1:10) | (5:30) | Rerative_Gaus_75_10  | F:75 C:10
# (1/8 19:35~)  | hal3  | k       | (1:10) | (5:30) | Rerative_liner_75_0  | F:75 C:0 #追試だが、結構結果が違う
# (1/8 17:50~)  | hal4  | ca      | (1:10) | (5:30) | Rerative_Gaus_75_0   | F:75 C:0 #形態評価の部分が間違っていたのでもう一回まわす

# (12/17)  | iMac  | passive | ()           | F:75

SEED=(1 2 3 4 5 6 7 8 9 10)
DELTA_T=(5 10 15 20 25 30)
CONTAIN_CONDUCTANCES="ca"
Function_Ratio="100"                   #ここでFunction_Ratioを変更できる
Conductance_Ratio="0"
Spesific="Determine_Gaus_"${Function_Ratio}"_"${Conductance_Ratio}     #その他のprefix

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
