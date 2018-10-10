import os,sys

from subprocess import STDOUT

from amalgamate import amalgamate
from cocoutils import make, run, python, check_output, expand_file



COCOJNI_CLASS='mgobench/utils/CocoJNI.scala'
SOURCE_ROOT = '../../src/main/scala'
BUILD_DIR = 'build'

CORE_FILES = ['src/coco_random.c',
              'src/coco_suite.c',
              'src/coco_observer.c',
              'src/coco_archive.c',
              'src/coco_runtime_c.c'
             ]

RELEASE = os.getenv('COCO_RELEASE', 'false') == 'true'

# amalgamate coco source
amalgamate(CORE_FILES,BUILD_DIR+'/coco.c', RELEASE,{})

# expand header
expand_file('src/coco.h', BUILD_DIR+'/coco.h',{})

# compile cocojni class and generate c header
run(SOURCE_ROOT, ['scalac', COCOJNI_CLASS])
# header
run(SOURCE_ROOT, ['javah', COCOJNI_CLASS.replace("/",".").replace(".scala","")])

# change CocoJNI.c to have the right class names
#os.system('./generateInterface.sh '+BUILD_DIR)
jheadername = [f for f in os.listdir("../../src/main/scala") if "CocoJNI" in f][0].split('_')
prefix = "_".join(jheadername[0:(len(jheadername)-1)])+"_"

interface = open('src/CocoJNI.c','r')
newfile = open('src/'+prefix+'CocoJNI.c','w')
for line in interface.readlines():
    currentline = line
    if '#include "CocoJNI.h"' in line :
        currentline = '#include "'+prefix+'CocoJNI.h"'
    if 'JNIEXPORT void JNICALL Java' in line or 'JNIEXPORT jlong JNICALL Java' in line or 'JNIEXPORT jint JNICALL Java' in line or 'JNIEXPORT jdoubleArray JNICALL Java' in line or 'JNIEXPORT jstring JNICALL Java' in line :
        split = line.split('_')
        currentline = split[0]+'_'+prefix+split[1]+'_'+split[2]
    newfile.write(currentline+'\n')


## compile library
if 'linux' in sys.platform:
    jdkpath = check_output(['locate', 'jni.h'],env=os.environ, universal_newlines=True)
    jdkpath1 = jdkpath.split("jni.h")[0]
    jdkpath2 = jdkpath1 + '/linux'
    run('BUILD_DIR',['gcc', '-I', jdkpath1, '-I', jdkpath2, '-c', COCOJNI_CLASS.replace(".scala",".c").replace("/","_")])
    run('BUILD_DIR',['gcc', '-I', jdkpath1, '-I', jdkpath2, '-o','lib'+COCOJNI_CLASS.replace(".scala",".so").replace("/","_"), '-fPIC', '-shared',COCOJNI_CLASS.replace(".scala",".c").replace("/","_")])
elif 'darwin' in sys.platform:
    jdkversion = check_output(['javac', '-version'],  stderr=STDOUT, env=os.environ, universal_newlines=True)
    print(jdkversion)
    jdkversion = jdkversion.split(" ")[1]
    jdkpath = '/System/Library/Frameworks/JavaVM.framework/Headers'
    jdkpath1 = ('/Library/Java/JavaVirtualMachines/jdk' +jdkversion + '.jdk/Contents/Home/include')
    jdkpath2 = jdkpath1 + '/darwin'
    run(BUILD_DIR,['gcc', '-I', jdkpath, '-I', jdkpath1, '-I', jdkpath2, '-c', COCOJNI_CLASS.replace(".scala",".c").replace("/","_")])
    run(BUILD_DIR,['gcc', '-dynamiclib', '-o', 'lib'+COCOJNI_CLASS.replace(".scala",".jnilib").replace("/","_"), COCOJNI_CLASS.replace(".scala",".o").replace("/","_")])


# recompile scala classes : at runtime ?
