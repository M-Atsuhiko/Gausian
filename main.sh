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
# (1/15 13:00~)  | hal1  | ca      | (1:10) | (5:30) | Rerative_liner_75_5  | F:75 C:5
# (1/15 20:40~)  | hal2  | k       | (1:10) | (5 10) | Rerative_Gaus_90_0   | F:90 C:0
# (1/15 15:05~)  | hal3  | k       | (1:10) | (5:30) | Rerative_liner_80_0  | F:80 C:0
# (1/15 13:05~)  | hal4  | k       | (1:10) | (5 10) | Rerative_Gaus_85_0   | F:85 C:0

# (12/17)  | iMac  | passive | ()           | F:75

# Linerの コンダクタンス制限バージョンもやってみるか

SEED=(1 2 3 4 5 6 7 8 9 10)
DELTA_T=(5 10)
CONTAIN_CONDUCTANCES="k"
Function_Ratio="90"                   #ここでFunction_Ratioを変更できる
Conductance_Ratio="0"
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
