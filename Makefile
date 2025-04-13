MPL=mpl

main: *.sml *.mlb
	$(MPL) -default-type int64 -default-type word64 main.mlb