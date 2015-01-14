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
# excuse time   | server| type     | seed   | dt     | suffix               |
# ------------------------------------------------------------------------------
# (1/14 11:25~)  | hal1  | k       | (1:10) | (5 10) | Determine_Gaus_80_0  | F:80 C:0
# (1/14 13:50~)  | hal2  | passive | (1:10) | (5:30) | Tsuishi_alfa_05      | alfa:0.5
# (1/14 11:15~)  | hal3  | k       | (1:10) | (5:20) | Rerative_liner_80_0  | F:80 C:0
# (1/14 11:45~)  | hal4  | k       | (1:10) | (5 10) | Rerative_Gaus_80_0   | F:80 C:0

# (12/17)  | iMac  | passive | ()           | F:75

SEED=(1 2 3 4 5 6 7 8 9 10)
DELTA_T=(5 10 15 20 25 30)
CONTAIN_CONDUCTANCES=""
Function_Ratio="75"                   #ここでFunction_Ratioを変更できる
Conductance_Ratio="0"
Spesific="Tsuishi_alfa_05_"${Function_Ratio}"_"${Conductance_Ratio}     #その他のprefix

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
