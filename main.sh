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
# (1/5 15:20~)  | hal1  | ca      | (1:10) | (5:30) | Determine_Gaus_75_0  | F:75 C:0
# (1/7 16:30~)  | hal2  | c       | (1:10) | (5:30) | Rerative_Gaus_75_10  | F:75 C:10
# (1/6 14:15~)  | hal3  | ca      | (1:10) | (5:30) | Rerative_liner_75_0  | F:75 C:0 
# (1/5 13:30~)  | hal4  | ca      | (1:10) | (5:30) | Rerative_Gaus_75_5   | F:75 C:5 

# (12/17)  | iMac  | passive | ()           | F:75

SEED=(1 2 3 4 5 6 7 8 9 10)
DELTA_T=(5 10 15 20 25 30)
CONTAIN_CONDUCTANCES="ca"
Function_Ratio="75"                   #ここでFunction_Ratioを変更できる
Conductance_Ratio="10"
Spesific="Rerative_Gaus_"${Function_Ratio}"_"${Conductance_Ratio}     #その他のprefix

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
