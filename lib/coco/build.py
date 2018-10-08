import os

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
#run('../../src/main/scala', ['javac', 'mgobench/utils/CocoJNI.scala']
run(SOURCE_ROOT, ['scalac', COCOJNI_CLASS])
# header
run(SOURCE_ROOT, ['javah', COCOJNI_CLASS.replace("/",".").replace(".scala","")])

# change CocoJNI.c
os.system('./generateInterface.sh')

# compile library
jdkpath = check_output(['locate', 'jni.h'],env=os.environ, universal_newlines=True)
jdkpath1 = jdkpath.split("jni.h")[0]
jdkpath2 = jdkpath1 + '/linux'
run('build',
    ['gcc', '-I', jdkpath1, '-I', jdkpath2, '-c', COCOJNI_CLASS.replace(".scala",".c").replace("/","_")])
run('build',
    ['gcc', '-I', jdkpath1, '-I', jdkpath2, '-o',
     'lib'+COCOJNI_CLASS.replace(".scala",".so").replace("/","_"), '-fPIC', '-shared',COCOJNI_CLASS.replace(".scala",".c").replace("/","_")])


# recompile scala classes : at runtime ?
