import os

from amalgamate import amalgamate
from cocoutils import make, run, python, check_output, expand_file


CORE_FILES = ['src/coco_random.c',
              'src/coco_suite.c',
              'src/coco_observer.c',
              'src/coco_archive.c',
              'src/coco_runtime_c.c'
             ]

RELEASE = os.getenv('COCO_RELEASE', 'false') == 'true'

# amalgamate coco source
amalgamate(CORE_FILES,'build/coco.c', RELEASE,{})

# expand header
expand_file('src/coco.h', 'build/coco.h',{})

# compile cocojni class and generate c header
#run('../../src/main/scala', ['javac', 'mgobench/utils/CocoJNI.scala']
run('../../src/main/scala', ['scalac', 'mgobench/utils/CocoJNI.scala'])
# header
run('../../src/main/scala', ['javah', 'mgobench.utils.CocoJNI'])

# change CocoJNI.c
os.system('./generateInterface.sh')

# compile library
jdkpath = check_output(['locate', 'jni.h'],env=os.environ, universal_newlines=True)
jdkpath1 = jdkpath.split("jni.h")[0]
jdkpath2 = jdkpath1 + '/linux'
run('build',
    ['gcc', '-I', jdkpath1, '-I', jdkpath2, '-c', 'mgobench_utils_CocoJNI.c'])
run('build',
    ['gcc', '-I', jdkpath1, '-I', jdkpath2, '-o',
     'libmgobench_utils_CocoJNI.so', '-fPIC', '-shared', 'mgobench_utils_CocoJNI.c'])


# recompile scala classes : at runtime ?
