
# generate coco c interface file from original CocoJNI
ls ../../src/main/scala
PREFIX=`ls ../../src/main/scala | grep "CocoJNI" | awk -F"_" '{$NF=""; print $0}' | sed -r 's/[" "]+/_/g'`
echo $PREFIX

# create the source
rm build/"$PREFIX"CocoJNI.c
#cp src/CocoJNI.c src/"$PREFIX"CocoJNI.c
cat src/CocoJNI.c | awk -F "_" '{if ($0=="#include \"CocoJNI.h\"") print "#include \"'$PREFIX'CocoJNI.h\""; else if ($1=="JNIEXPORT void JNICALL Java"||$1=="JNIEXPORT jlong JNICALL Java"||$1=="JNIEXPORT jint JNICALL Java"||$1=="JNIEXPORT jdoubleArray JNICALL Java"||$1=="JNIEXPORT jstring JNICALL Java") print $1"_'$PREFIX'"$2"_"$3; else print $0}' > build/"$PREFIX"CocoJNI.c

# move the header (needed for compilation)
mv ../../src/main/scala/"$PREFIX"CocoJNI.h build
