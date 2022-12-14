SRC := ultimate_tag.asm
OUT := ultimate_tag.bin

all: $(OUT)

$(OUT): $(SRC)
	dasm $(SRC) -f3 -o$(OUT) -Iheaders

run:
	stella $(OUT)

clean:
	rm $(OUT)

.PHONY: all run clean
