for(f in lsf.str()){
	srcfile <- attr(attr(eval(parse(text=f)),"srcref"),"srcfile")$filename
	if(grepl("calculate_clone_polygons", srcfile)){		
		print(srcfile)
		dump(f, file="_calc_clone_polygons.R", append=TRUE)
	}
}


