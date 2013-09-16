STOGDIR=..
STOG=stog
DEST_DIR=`pwd`/../output
BASE_URL_OPTION=
STOG_OPTIONS= -d $(DEST_DIR) -v -v -v --depcut $(BASE_URL_OPTION) --def stogdir:`pwd` $(MORE_OPTS)
LESSC=lessc

.PHONY: build site test style

style:
	mkdir -p $(DEST_DIR)
	$(LESSC) less/style.less > $(DEST_DIR)/style.css

test:
	$(MAKE) BASE_URL_OPTION="--site-url file://$(DEST_DIR)" site

