TARGET=json

.PHONY: build
build:
	mmc --make lib$(TARGET)

.PHONY: depend
depend:
	mmc --make lib$(TARGET).depend

.PHONY: install
install:
	mmc --make lib$(TARGET).install

.PHONY: clean
	mmc --make $(TARGET).clean

.PHONY: realclean
	mmc --make $(TARGET).realclean
	/usr/bin/env rm -rf Mercury