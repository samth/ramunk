
all:
	affc chipmunk2d/include/chipmunk/chipmunk.h --include `find chipmunk2d/include/chipmunk/ -type f ! -name chipmunk_private.h` --exclude > bin/chipmunk.json
	cd chipmunk2d/ && mkdir -p build/Release && cd build/Release/ && cmake ../../ -DFUNCTION_POINTERS=OFF -DCMAKE_BUILD_TYPE=Release && make
	cp chipmunk2d/build/Release/src/libchipmunk.dylib bin/chipmunk.dylib
