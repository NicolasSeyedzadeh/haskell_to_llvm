all: run clean

run:
	@cargo run
	@llc -relocation-model=pic out.ll -o out.s
	@gcc out.s -o out_executable
	./out_executable
clean:
	rm out.ll
	rm out.s
clean_eval:
	cd evaluation/Sample1  && \
	rm -f *.cmi  && \
	rm -f *.cmx && \
	rm -f *.hi && \
	rm -f *.o
	cd evaluation/Sample2 && \
	rm -f *.cmi && \
	rm -f *.cmx && \
	rm -f *.hi && \
	rm -f *.o
	cd evaluation/Sample3 && \
	rm -f *.cmi && \
	rm -f *.cmx && \
	rm -f *.hi && \
	rm -f *.o
	cd evaluation/Sample4 && \
	rm -f *.cmi && \
	rm -f *.cmx && \
	rm -f *.hi && \
	rm -f *.o
