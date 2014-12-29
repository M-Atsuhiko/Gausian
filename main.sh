#! /bin/bash
#author: Atsuhiko Murakami

IS_TEST="FALSE"

if [ -n "$1" ]
then
    IS_TEST="TRUE"
fi

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
# excuse time     | server| type    | seed      | dt     | suffix           |
# ------------------------------------------------------------------------------
# (12/29 17:30~)  | hal1  | passive | (1:10) | (5:30) | Rerative_75_0       | F:75 C:0 相対的な評価手法をpassiveに試す。理論的な形態の解が本当なら、この方法でも同一の傾向を持つ形になるはず
# (12/29 17:30~)  | hal2  | ca      | (1:10) | (30)   | Tsuishi_75_0        | F:75 C:0 hal2-hal3-hal4の結果を比較する
# (12/29 17:35~)  | hal3  | ca      | (1:10) | (30)   | Rerative_liner_75_0 | F:75 C:0
# (12/29 17:35~)  | hal4  | ca      | (1:10) | (30)   | Rerative_Gaus_75_0  | F:75 C:0


# (12/17)  | iMac  | passive | ()           | F:75

SEED=(1 2 3 4 5 6 7 8 9 10)
DELTA_T=(30)
CONTAIN_CONDUCTANCES="ca"
Function_Ratio="75"                   #ここでFunction_Ratioを変更できる
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
