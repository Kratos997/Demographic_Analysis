#!/bin/sh

function show_usage (){
    printf "Usage: $0 [options] \n"
    printf "\n"
	printf "Options: \n"
	printf "	-h, --help : Prints Help \n"
    printf "	-tmwt, --tmannwhitneytest : Computes T test or Mann-Whitney test for normally or not normally distributed data respectively \n"
    printf "	-akwt, --anovakwtest : Computes One Way ANOVA or Kruskal Wallis test for normally or not normally distributed data respectively \n"
    printf "	-ptwt, --pairedtwiltest : Computes paired T test or Wilcoxon signed rank test for normally or not normally distributed data respectively \n"
    printf "	-psc, --pearsonspearmancorr : Computes Pearson's or Spearman's correlation for normally or not normally distributed data respectively \n"
    printf "	-cqt, --chisquaretest : Computes Chi sqaured test for categorical variables \n"
	
return 0
}

if [ -z "$1" ];then 
	show_usage
	exit
fi

for opt in "$@";do
    case "$opt" in
    -h|--help)
        show_usage
        ;;
    -tmwt|--tmannwhitneytest)
        echo "Calculating Student's T (normal data) or Mann-Whitney (not normal data) test ..."
        ;;
    -akwt|--anovakwtest)
        echo "Calculating One Way ANOVA (normal data) or Kruskal-Wallis (not normal data) test ..."
        ;;
    -ptwt|--pairedtwiltest)
        echo "Calculating Paired Student's (normal data) or Wilcoxon signed rank (not normal data) test ..."
        ;;
    -psc|--pearsonspearmancorr)
        echo "Calculating Pearson's (normal data) or Spearman's (not normal data) correlation ..."
        ;;
    -cqt|--chisquaretest)
        echo "Calculating Chi squared test ..."
        ;;
    *)
        echo "Invalid Option."
        show_usage
        ;; 
    esac
done