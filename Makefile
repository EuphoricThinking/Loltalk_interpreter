TARGET = interpreter

all: $(TARGET).hs
	ghc $(TARGET).hs

clean:
	$(RM) *.o *.hi $(TARGET) Loltalk/*.o Loltalk/*.hi
