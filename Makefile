CFLAGS = -O2 -march=core2
CFLAGS += -std=gnu++0x -g -pg
#CFLAGS += -D_DEBUG_PRINT

CXX = g++ 

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
