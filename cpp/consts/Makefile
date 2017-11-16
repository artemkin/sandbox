
SRC := $(wildcard *.cpp)
OBJS := $(SRC:%.cpp=%.o)
BIN := consts


all: $(BIN)

$(BIN): $(OBJS)
	$(CXX) -o $@ $(OBJS)

$(OBJS): consts.h

clean:
	@rm -rf *.o
	@rm -f $(BIN)

.PHONY: all clean
