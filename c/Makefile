CC = gcc
CFLAGS = -Wall -Wextra -std=c11
LIBS = -lcurl
TARGET = checker
SOURCE = main.c

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(CC) $(CFLAGS) -o $(TARGET) $(SOURCE) $(LIBS)

clean:
	rm -f $(TARGET)

install-deps:
	@echo "Installing dependencies..."
	@if command -v apt-get > /dev/null; then \
		sudo apt-get update && sudo apt-get install -y libcurl4-openssl-dev; \
	elif command -v yum > /dev/null; then \
		sudo yum install -y libcurl-devel; \
	elif command -v brew > /dev/null; then \
		brew install curl; \
	else \
		echo "Please install libcurl development libraries manually"; \
	fi

.PHONY: all clean install-deps
