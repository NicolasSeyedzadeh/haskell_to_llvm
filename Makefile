all: run clean

run:
	@cargo run
	@llc -relocation-model=pic out.ll -o out.s
	@gcc out.s -o out_executable
	./out_executable
clean:
	rm out.ll
	rm out.s
