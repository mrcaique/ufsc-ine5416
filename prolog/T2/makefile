# Compile the program
compile:
	swipl -f db2svg.pl > desenhos.svg  2> stderr.txt

# Compile the files and run the interpreter
prolog:
	swipl -s desenhos.pl programa.pl db2svg.pl

# Run the program, showing the image SVG generated
run:
	display desenhos.svg

# Remove the desenhos.svg and stderr.txt files
remove:
	rm desenhos.svg 
	rm stderr.txt
