GSC=gsc
LDFLAGS=-lX11

cosmic-sweep.o1: cosmic-sweep.scm cosmic-sweep-backend.c
	rm -f $@
	$(GSC) -ld-options "$(LDFLAGS)" $<

cosmic-sweep: cosmic-sweep.scm cosmic-sweep-backend.c
	$(GSC) -ld-options "$(LDFLAGS)" -postlude "(init-window) (main)" -exe -nopreload $<
