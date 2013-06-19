#
#
#
#       $@  Targetファイル名 
#       $%  Archiveファイルのメンバファイル名
#       $<  :の右の最左のファイル名
#       $?  :の右のファイルの中でtargetファイルよりも新しいもの
#       $*  Targetファイル名からsuffixをとったもの 

# COMPILER = gcc
COMPILER = g++
TARGET = 
INCLUDE_PATH = 
OPTIONS = 

all:

%.o: %.c
	$(COMPILER) -c $(OPTIONS) $(INCLUDE_PATH) $<

clean:
	rm -f  $(TARGET) *.o *~