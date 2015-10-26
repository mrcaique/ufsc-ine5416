# Compile the program
compile:
	swipl -c db2svg.pl && ./a.out > desenhos.svg

# Compile the files and run the interpreter
prolog:
	swipl -s desenhos.pl programa.pl db2svg.pl

# Run the program, showing the image SVG generated
run:
	display desenhos.svg

# Remove the desenhos.svg file and a.out executable
remove:
	rm desenhos.svg
	rm a.out
