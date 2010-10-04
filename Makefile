CFLAGS = -O3 -ffast-math -march=core2 -fomit-frame-pointer -funroll-loops
CFLAGS += -std=c++0x 
#CFLAGS += -fprofile-generate
#CFLAGS += -fprofile-use
#CFLAGS += -D_DEBUG_PRINT

#LIBS = -ltbb

CXX = g++ 
#CXX = /opt/intel/cc/10.1.012/bin/icc
#CXX = icc

TARGETS = gibbsPOScpp
SRCS = random.cpp gibbsPOS.cpp 
OBJS = random.o gibbsPOS.o

.SUFFIXES: .cpp

all: $(TARGETS)

gibbsPOScpp: $(OBJS)
	$(CXX) $(CFLAGS) -o gibbsPOS $(OBJS) $(LIBS)

#$(TARGET): $(OBJS)
#	$(CXX) $(CFLAGS) -o $(TARGET) $(OBJS) $(LIBS)

clean:
	rm -f *~ $(OBJS) $(TARGET)

.cpp.o:
	$(CXX) $(CFLAGS) $(INC) $(SDL_INC) -c $*.cpp
