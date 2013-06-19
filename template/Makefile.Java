ARCHIVE=
CLASSES=classes
MANIFEST=
SOURCES=
SOURCE_FILES=$(SOURCES) $(MANIFEST)

$(CLASSES)/$(ARCHIVE): $(SOURCE_FILES) 
	[ -e $(CLASSES) ] || mkdir $(CLASSES)
	rm -f $@
	javac -d $(CLASSES) $(SOURCES)
	cp $(SOURCE_FILES) $(CLASSES)
	(cd $(CLASSES) ; \
	if [ -z "$(MANIFEST)" ]; then \
		jar cvf $(ARCHIVE) * ; \
	else \
		jar cvmf $(MANIFEST) $(ARCHIVE) * ; \
	fi )

############################################################
# sign
#
KEYSTORE=/home/tak/.keystore/mamewo
KEY=mykey

$(KEYSTORE):
	keytool -genkey -keystore $(KEYSTORE) -alias $(KEY)
	keytool -selfcert -alias $(KEY) -keystore $(KEYSTORE)

sign:
	jarsigner -keystore $(KEYSTORE) $(CLASSES)/$(ARCHIVE) $(KEY)

################################################
# JavaCC
JAVACC_DEFAULT_SOURCES=TokenMgrError.java ParseException.java Token.java JavaCharStream.java
JAVACC_RESULT_SOURCE=
## Hoge.jj
JAVACC_SOURCE=
JAVACC_COMPILER=javacc

$(JAVACC_RESULT_JAVA_SOURCE): $(JAVACC_SOURCE)
	rm -f $(JAVACC_DEFAULT_SOURCES)
	$(JAVACC_COMPILER) $(JAVACC_SOURCE)


clean:
	rm $(CLASSES) *~
