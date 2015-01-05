#! /bin/bash
#author: Atsuhiko Murakami

IS_TEST="FALSE"

if [ -n "$1" ]
then
    IS_TEST="TRUE"
fi

#test

# 実行したseed
#---------------------------------------
# passive |F:75  || ()
#---------------------------------------
# k       |F:75  || ()
#---------------------------------------
# ca      |F:75  | times | (1 2 4 5)
#---------------------------------------
# k ca    |F:75  || ()

# 実行したseed
#---------------------------------------
# times          |F:75  | (1 2 4 5 6 7 8)
#---------------------------------------
# times_Ldet     |F:75  | (1 2 5 7 8 9 10 12 14)

# mmmmmm m    m mmmmmm   mmm  m    mmmmmmmm mmmmmm
# #       #  #  #      m"   " #    #   #    #     
# #mmmmm   ##   #mmmmm #      #    #   #    #mmmmm
# #       m""m  #      #      #    #   #    #     
# #mmmmm m"  "m #mmmmm  "mmm" "mmmm"   #    #mmmmm

#SEED = 2では回らない

# 実行中
# excuse time   | server| type    | seed   | dt     | suffix               |
# ------------------------------------------------------------------------------
# (1/5 15:20~)  | hal1  | ca      | (1:10) | (5:30) | Determine_75_0       | F:75 C:0
# (1/5 13:50~)  | hal2  | ca      | (1:3)  | (5:30) | Tsuishi_alfa_05       | alfa:0.5
# (1/5 14:00~)  | hal3  | ca      | (1:10) | (5:25) | Rerative_liner_75_0  | F:75 C:0
# (1/5 13:30~)  | hal4  | ca      | (1:10) | (5:30) | Rerative_Gaus_75_5   | F:75 C:5 #Bad Resultで結果を分けるのをやめてみる


# (12/17)  | iMac  | passive | ()           | F:75

SEED=(1 2 3 4 5 6 7 8 9 10)
DELTA_T=(5 10 15 20 25 30)
CONTAIN_CONDUCTANCES="ca"
Function_Ratio="75"                   #ここでFunction_Ratioを変更できる
Conductance_Ratio="0"
Spesific="Tsuishi_"${Function_Ratio}"_"${Conductance_Ratio}     #その他のprefix

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
