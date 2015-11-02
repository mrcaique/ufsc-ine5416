FILES = desenhos.pl programa.pl
A_DIR = T2A/
B_DIR = T2B/
SPL_C = swipl

#########################
#	  T2A		#
#########################

# Compile the program in the A_DIR folder
dcompile:
	cd $(A_DIR); $(SPL_C) -c db2svg.pl && ./a.out > desenhos.svg

# Run the program, showing the image SVG generated
drun:
	cd $(A_DIR); gnome-open desenhos.svg

# Compile the files and run the interpreter
# in the A_DIR folder
dprolog:
	cd $(A_DIR); $(SPL_C) -s $(FILES)

# Removes the desenhos.svg file and a.out
# executable
dclean:
	cd $(A_DIR); rm desenhos.svg && rm a.out

#########################
#	  T2B		#
#########################

# Compile the program in the A_DIR folder
tcompile:
	cd $(B_DIR); $(SPL_C) -c db2svg.pl && ./a.out > desenhos.svg

# Run the program, showing the image SVG generated
trun:
	cd $(B_DIR); gnome-open desenhos.svg

# Compile the files and run the interpreter
# in the B_DIR folder
tprolog:
	cd $(B_DIR); $(SPL_C) -s $(FILES)

# Removes the desenhos.svg file and a.out
# executable
tclean:
	cd $(B_DIR); rm desenhos.svg && rm a.out
