ERLC=erlc
SRCDIR=src
BEAMDIR=./ebin
INCDIR=./include
ERLCFLAGS=-o $(BEAMDIR) -I $(INCDIR) -Wall


all: 
	@mkdir -p $(BEAMDIR)
	@$(ERLC) $(ERLCFLAGS) $(SRCDIR)/*.erl

clean: 
	@rm -rf $(BEAMDIR)
	@rm -rf erl_crush.dump
