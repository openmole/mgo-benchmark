/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class mgobench_utils_CocoJNI */

#ifndef _Included_mgobench_utils_CocoJNI
#define _Included_mgobench_utils_CocoJNI
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoSetLogLevel
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_mgobench_utils_CocoJNI_cocoSetLogLevel
  (JNIEnv *, jobject, jstring);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoGetObserver
 * Signature: (Ljava/lang/String;Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL Java_mgobench_utils_CocoJNI_cocoGetObserver
  (JNIEnv *, jobject, jstring, jstring);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoFinalizeObserver
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_mgobench_utils_CocoJNI_cocoFinalizeObserver
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemAddObserver
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL Java_mgobench_utils_CocoJNI_cocoProblemAddObserver
  (JNIEnv *, jobject, jlong, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemRemoveObserver
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL Java_mgobench_utils_CocoJNI_cocoProblemRemoveObserver
  (JNIEnv *, jobject, jlong, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoGetSuite
 * Signature: (Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL Java_mgobench_utils_CocoJNI_cocoGetSuite
  (JNIEnv *, jobject, jstring, jstring, jstring);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoFinalizeSuite
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_mgobench_utils_CocoJNI_cocoFinalizeSuite
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoSuiteGetNextProblem
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL Java_mgobench_utils_CocoJNI_cocoSuiteGetNextProblem
  (JNIEnv *, jobject, jlong, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoSuiteGetProblem
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL Java_mgobench_utils_CocoJNI_cocoSuiteGetProblem
  (JNIEnv *, jobject, jlong, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoEvaluateFunction
 * Signature: (J[D)[D
 */
JNIEXPORT jdoubleArray JNICALL Java_mgobench_utils_CocoJNI_cocoEvaluateFunction
  (JNIEnv *, jobject, jlong, jdoubleArray);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoEvaluateConstraint
 * Signature: (J[D)[D
 */
JNIEXPORT jdoubleArray JNICALL Java_mgobench_utils_CocoJNI_cocoEvaluateConstraint
  (JNIEnv *, jobject, jlong, jdoubleArray);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetDimension
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetDimension
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetNumberOfObjectives
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetNumberOfObjectives
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetNumberOfConstraints
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetNumberOfConstraints
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetSmallestValuesOfInterest
 * Signature: (J)[D
 */
JNIEXPORT jdoubleArray JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetSmallestValuesOfInterest
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetLargestValuesOfInterest
 * Signature: (J)[D
 */
JNIEXPORT jdoubleArray JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetLargestValuesOfInterest
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetLargestFValuesOfInterest
 * Signature: (J)[D
 */
JNIEXPORT jdoubleArray JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetLargestFValuesOfInterest
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetId
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetId
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetName
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetName
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetEvaluations
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetEvaluations
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetEvaluationsConstraints
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetEvaluationsConstraints
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemGetIndex
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_mgobench_utils_CocoJNI_cocoProblemGetIndex
  (JNIEnv *, jobject, jlong);

/*
 * Class:     mgobench_utils_CocoJNI
 * Method:    cocoProblemIsFinalTargetHit
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_mgobench_utils_CocoJNI_cocoProblemIsFinalTargetHit
  (JNIEnv *, jobject, jlong);

#ifdef __cplusplus
}
#endif
#endif
