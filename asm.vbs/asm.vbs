''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Hit Ctrl+F and find the second instance of "here"
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
exetxt = ""
loc = 0
eax="eax":ecx="ecx":edx="edx":ebx="ebx":esp="esp":ebp="ebp":esi="esi":edi="edi"
cs="cs":ds="ds"
al="al":cl="cl":dl="dl":bl="bl"
ah="ah":ch="ch":dh="dh":bh="bh"
ax="ax":cx="cx":dx="dx":bx="bx":sp="sp":bp="bp":si="si":di="di"
registers = Array(eax,ecx,edx,ebx,esp,ebp,esi,edi,cs,ds,al,cl,dl,bl,ah,ch,dh,bh,ax,cx,dx,bx,sp,bp,si,di)
function register(x)
	for i = 0 to ubound(registers)
		if registers(i) = x then
			register = i
			exit function
		end if
	next
	register = -1
end function
sub pad(x)
	if x = 0 then exit sub
	exetxt = exetxt & String(x,chr(0))
	loc = loc + x
end sub
sub pad_until(x)
	if loc > x then Err.Raise 5,"pad_until","Location (" & loc & ") already past boundary (" & x & ")!"
	pad x-loc
end sub
sub pad_align(x)
	pad x-(loc mod x)
end sub
sub nop_align_d()
	if (loc mod 4) = 3 then
		dw &h768d
		db 0
	elseif (loc mod 4) = 2 then
		dw &h9066
	elseif (loc mod 4) = 1 then
		db &h90
	end if
end sub
sub db(xo)
	x = xo
	if x < 0 then x = xo + 256
	if x > 255 then Err.Raise 5,"db","Byte overflow"
	exetxt = exetxt & chr(x)
	loc = loc + 1
end sub
sub dw(xo)
	x = xo
	if x < 0 then x = xo + 256^2
	if x > 256^2-1 then Err.Raise 5,"dw","Word overflow"
	exetxt = exetxt & chr(x mod 256) & chr(int(x / 256))
	loc = loc + 2
end sub
sub dd(xo)
	x = xo
	if x < 0 then x = xo + 256^4
	exetxt = exetxt & chr((x-fix(x/256)*256))
	exetxt = exetxt & chr(fix((x / 256)) mod 256) & chr(fix((x / (256^2))) mod 256) & chr(fix((x / (256^3))))
	loc = loc + 4
end sub
sub putc(x)
	exetxt = exetxt & x
	loc = loc + len(x)
end sub
sub puts(x)
	exetxt = exetxt & x & chr(0)
	loc = loc + len(x) + 1
end sub
sub ret()
	db &hc3
end sub
sub cmpsb()
	db &ha6
end sub
sub movsb()
	db &ha4
end sub
sub movsd()
	db &ha5
end sub
sub rep_movsb()
	dw &ha4f3
end sub
sub rep_movsd()
	dw &ha5f3
end sub
sub xchg(x,y)
	rx = register(x)
	ry = register(y)
	if x = eax then
		db &h90 + ry
	elseif y = eax then
		db &h90 + rx
	else
		db &hc0 + rx + (ry*8)
	end if
end sub
sub push(x)
	if x = "cs" then 
		db &h0E
		exit sub
	end if
	if isnumeric(x) then
		if x < 128 and x > -128 then
			db &h6a
			db x
		else
			db &h68
			dd x
		end if
		exit sub
	end if
	rx = register(x)
	if rx >= 0 then
		db rx+80
	end if
end sub
sub pop(x)
	if x = "ds" then
		db &h1F
		exit sub
	end if
	rx = register(x)
	if rx >= 0 then
		db rx+87+1
	end if
end sub
sub interrupt(x)
	db &hcd
	db x
end sub
marks = ""
recalls = ""
thunks = ""
sub splice_b(x,yo)
	y = yo
	if y < 0 then y = yo + 256
	if y > 255 then Err.Raise 5,"splice_b","Byte overflow"
	exetxt = left(exetxt,x) & chr(y) & mid(exetxt,x+2)
end sub
sub splice_w(x,yo)
	y = yo
	if y < 0 then y = yo + 256^2
	if y > 256^2-1 then Err.Raise 5,"splice_w","Word overflow"
	exetxt = left(exetxt,x) & chr(y mod 256) & chr(int(y/256)) & mid(exetxt,x+3)
end sub
sub splice_d(x,yo)
	y = yo
	if y < 0 then y = yo + 256^4
	if y > 256^4-1 then Err.Raise 5,"splice_d","Dword overflow"
	exetxt = left(exetxt,x) & chr(y mod 256) & chr(int(y/256) mod 256) & chr(int(y/(256^2)) mod 256) & chr(int(y/(256^3)) mod 256) & mid(exetxt,x+5)
end sub
sub splice_bs(x,y)
    if y > 127 then Err.Raise 5,"splice_bs","Signed Byte overflow"
	splice_b x,y
end sub
sub splice_ws(x,y)
    if y > &h7fff then Err.Raise 5,"splice_ws","Signed Word overflow"
	splice_w x,y
end sub
sub dbs(x)
    if x > 127 then Err.Raise 5,"dbs","Signed Byte Overflow"
    db x
end sub
sub dws(x)
    if x > &h7fff then Err.Raise 5,"dws","Signed Word Overflow"
	dw x
end sub
sub mark_offset(x,y)
	marks = x & ":" & (loc+y) & vbcrlf & marks
	allrecs = Split(recalls,vbcrlf)
	recalls = ""
	for i = 0 to ubound(allrecs)-1
		if Split(allrecs(i),":")(0) = x then
			rc = cint(Split(allrecs(i),":")(1))
			if mid(exetxt,rc+1,1) = "b" then
				splice_bs rc, loc-rc-1+y
			elseif mid(exetxt,rc+1,1) = "w" then
				splice_ws rc, loc-rc-2+y
			elseif mid(exetxt,rc+1,1) = "d" then
				splice_d rc, loc-rc-4+y
			end if
		else
			recalls = recalls & allrecs(i) & vbcrlf
		end if
	next
end sub
sub mark(x)
	mark_offset x,0
end sub
sub recall(x,t)
	allmarks = Split(marks,vbcrlf)
	for i = 0 to ubound(allmarks)-1
		if Split(allmarks(i),":")(0) = x then
			mrk = cint(Split(allmarks(i),":")(1))
			select case t
				case 1,"b": dbs mrk-loc-1
				case 2,"w": dws mrk-loc-2
				case 4,"d": dd mrk-loc-4
			end select
			exit sub
		end if
	next
	recalls = x & ":" & loc & vbcrlf & recalls
	select case t
		case 1,"b": putc("b")
		case 2,"w": puts("w")
		case 4,"d": puts("ddd")
	end select
	exit sub
end sub
sub writefile(x)
	Set f = CreateObject("Scripting.FileSystemObject").CreateTextFile(x)
	f.Write(exetxt)
	f.Close()
	exetxt = ""
	marks = ""
	recalls = ""
	loc = 0
end sub
bits = 32
sub mov(x,y)
	rx = register(x)
	ry = register(y)
	if rx >= 0 and ry < 0 then
		if rx >= register(al) and rx <= register(di) then
			if bits > 16 and rx >= register(ax) then db &h66
			db &hb0 + (rx-register(al))
			if isnumeric(y) then
				if rx < register(ax) then
					db y
				else
					dw y
				end if
			else
				if rx < register(ax) then
					recall y,1
				else
					recall y,2
				end if
			end if
		else
			db &hb8 + rx
			if isnumeric(y) then
				dd y
			else
				recall y,4
			end if
		end if
	elseif rx >= 0 and ry >= 0 then
		db &h89
		db &hc0 + rx + (ry*8)
	end if
end sub
sub mov_dword_ptr(x,y,z)
	rx = register(x)
	ry = register(y)
	rz = register(z)
	if rx >= 0 and rz < 0 and ry < 0 then
		db &hc7
		if x = esp then
			if y <> 0 then db &h44 else db 4
			db &h24
			if y <> 0 then db y
		elseif x = ebp or y <> 0 then
			db &h40 + rx
			db y
		else
			db rx
		end if
		dd z
	elseif rx >= 0 and rz >= 0 and ry < 0 then
		db &h89
		if y <> 0 or x = ebp then db &h40 + rx + (rz*8) else db rx + (rz*8)
		if x = esp then db &h24
		if y <> 0 or x = ebp then db y
	elseif rx < 0 and rz >= 0 and ry < 0 then
		if z = eax then 
			db &ha3
		else
			db &h89
			db 5 + (rz*8)
		end if
		dd x
	elseif rx >= 0 and ry >= 0 and rz < 0 then
		db &h8b
		if z <> 0 or y = ebp then db &h40 + ry + (rx*8) else db ry + (rx*8)
		if y = esp then db &h24
		if z <> 0 or y = ebp then db z
	end if
end sub
sub get_dword_ptr(x,y)
	rx = register(x)
	if rx = eax then
		db &ha1
		dd y
	elseif rx >= 0 then
		db &h8b
		db 5 + (rx*8)
		dd y
	end if
end sub
sub add_dword_ptr(x,y,z)
	rx = register(x)
	rz = register(z)
	if rx >= 0 and rz < 0 then
		db &h83
		if y <> 0 or x = ebp then db &h40 + rx else db rx
		if x = esp then db &h24
		if y <> 0 or x = ebp then db y
		db z
	end if
end sub
sub push_dword_ptr(x,y)
	rx = register(x)
	if rx >= 0 then 
		db &hff
		if y <> 0 or x = ebp then db &h70 + rx else db &h30 + rx
		if x = esp then db &h24
		if y <> 0 or x = ebp then db y
	end if
end sub
sub xchg_dword_ptr(x,y,z)
	rx = register(x)
	ry = register(y)
	rz = register(z)
	if rx >= 0 and ry < 0 and rz >= 0 then
		db &h87
		if y <> 0 or x = ebp then db &h40 + rx + (ry*8) else db rx + (ry*8)
		if x = esp then db &h24
		if y <> 0 or x = ebp then db y
	end if
end sub
sub sub_(x,y)
	rx = register(x)
	ry = register(y)
	if rx >= 0 and ry < 0 then
		if rx >= register(al) and rx <= register(di) then
			
		else
			if y < 128 and y > -128 then
				db &h83
				db &he8 + rx
				db y
			elseif rx = register(eax) then
				db &h2d
				dd y
			else
				db &h81
				db &he8 + rx
				dd y
			end if
		end if
	elseif rx >= 0 and ry >= 0 then
		db &h29
		db &hc0 + rx + (ry*8)
	end if
end sub
sub xor_(x,y)
	rx = register(x)
	ry = register(y)
	if rx >= 0 and ry >= 0 then
		db &h31
		db &hc0 + rx + (ry*8)
	end if
end sub
sub and_(x,y)
	rx = register(x)
	ry = register(y)
	if rx >= 0 and ry < 0 then
		db &h83
		db &he0 + rx
		db y
	end if
end sub
sub add(x,y)
	rx = register(x)
	ry = register(y)
	if rx >= 0 and ry < 0 then
		db &h83
		db &hc0 + rx
		db y
	elseif rx >= 0 and ry >= 0 then
		db 1
		db &hc0 + rx + (ry*8)
	end if
end sub
sub cmp(x,y)
	rx = register(x)
	ry = register(y)
	if rx >= 0 and ry < 0 then
		if rx >= register(al) and rx <= register(di) then
		else
			if y < 128 and y > -128 then
				db &h83
				db &hf8+rx
				db y
			elseif x = eax then
				db &h3d
				dd y
			else
				db &h81
				db &hf8 + rx
				dd y
			end if
		end if
	elseif rx >=0 and ry >= 0 then
		db &h39
		db &hc0 + rx + (ry*8)
	end if
end sub
sub test(x,y)
	rx = register(x)
	ry = register(y)
	if rx >= 0 and ry < 0 then
		if x = al then
			db &ha8
			db y
		elseif rx > register(al) and rx <= register(di) then
			db &hf6
			db &hc0 + rx-register(al)
			db y
		elseif x = eax then
			db &ha9
			dd y
		else
			db &hf7
			db &hc0 + rx
			dd y
		end if
	elseif rx >= 0 and ry >= 0 then
		db &h85
		db &hc0 + rx + (ry*8)
	end if
end sub
sub cmp_byte_ptr(x,y,z)
	rx = register(x)
	ry = register(y)
	rz = register(z)
	if rx >= 0 and ry < 0 then
		db &h80
		if y <> 0 or x = ebp then db &h78 + rx else db &h38 + rx
		if x = esp then db &h24
		if y <> 0 or x = ebp then db y
		db z
	elseif rx >= 0 and rz >= 0 then
		db &h38
		if y <> 0 or x = ebp then db &h40 + rx + (rz*8) else db rx + (rz*8)
		if x = esp then db &h24
		if y <> 0 or x = ebp then db y
	end if
end sub
sub cmp_dword_ptr(x,y,z)
	rx = register(x)
	ry = register(y)
	rz = register(z)
	if rx >= 0 and ry < 0 then
		db &h83
		if y <> 0 or x = ebp then db &h78 + rx else db &h38 + rx
		if x = esp then db &h24
		if y <> 0 or x = ebp then db y
		db z
	elseif rx >= 0 and rz >= 0 then
		db &h39
		if y <> 0 or x = ebp then db &h40 + rx + (rz*8) else db rx + (rz*8)
		if x = esp then db &h24
		if y <> 0 or x = ebp then db y
	end if
end sub
sub call_(x)
	rx = register(x)
	if rx >= 0 then
		db &hff
		db &hd0 + rx
	else
		allthunks = Split(thunks,vbcrlf)
		for i = 0 to ubound(allthunks)-1
			if Split(allthunks(i),":")(0) = x then
				dw &h15ff
				dd Split(allthunks(i),":")(1)
				exit sub
			end if
		next
		Err.Raise 5,"call_","No function " & x
	end if
end sub
sub call_dword_ptr(x,y)
	rx = register(x)
	if rx >= 0 then
		db &hff
		if y <> 0 or x = ebp then db &h50 + rx else db &h10 + rx
		if x = esp then db &h24
		if y <> 0 or x = ebp then db y
	end if
end sub
sub je(x)
	db &h74
	recall x,1
end sub
sub jz(x)
	je(x)
end sub
sub longje(x)
	db &h0f
	db &h84
	recall x,4
end sub
sub longjz(x)
	longje(x)
end sub
sub jmp(x)
	rx = register(x)
	if rx < 0 then
		db &heb
		recall x,1
	else
		db &hff
		db &he0 + rx
	end if
end sub
sub longjmp(x)
	allthunks = Split(thunks,vbcrlf)
	for i = 0 to ubound(allthunks)-1
		if Split(allthunks(i),":")(0) = x then
			dw &h25ff
			dd Split(allthunks(i),":")(1)
			exit sub
		end if
	next
	db &he9
	recall x,4
end sub
sub jne(x)
	db &h75
	recall x,1
end sub
sub jnz(x)
	jne x
end sub
sub longjne(x)
	db &h0f
	db &h85
	recall x,4
end sub
sub longjnz(x)
	longjne(x)
end sub
sub cld()
	db &hfc
end sub
e_lfanew = 0
sub dos_stub(msg)
	putc "MZ"
	dw 0 'lastsize -- use whole block
	dd 1 'nblocks & nreloc
	dd 4 'hdrsize & minalloc
	dw -1 'maxalloc
	dw 0 'stack segment
	recall "dos_sp",2 'stack pointer
	o = loc
	pad 42 'checksum, ip, cs, relocpos, noverlay, reserved[4], oem_id, oem_info, reserved2[10]
	e_lfanew = loc
	puts "ddd"
	bits = 16
	o = o-loc
	o2 = loc
	push cs
	pop ds
	mov ah,9
	mov dx,"dos_msg"
	o2 = loc-o2
	interrupt &h21
	mov ax,&h4c01
	interrupt &h21
	mark_offset "dos_msg",o2
	putc msg
	putc "$"
	pad 6
	pad_align 16
	mark_offset "dos_sp",o
	bits = 32
end sub
section_count = 0
num_sections = 0
code_size = 0
data_size = 0
uninitialized_data_size = 0
entry_point = 0
data_dir = 0
sub exe_header(linker,codebase,database,imagsize,headsize,console)
	splice_d e_lfanew,loc
	putc "PE"
	pad 2
	dw 332
	section_count = loc: dw 0
	dd 0 'no timestamp
	dd 0 'no symbol table
	dd 0 'like I said...
	dw 224 'size of optional header
	dw 3 'idk why it's a 3 but it is
	dw 267
	if len(linker) <> 2 then Err.Raise 5,"exe_header","Linker tag must be 2 characters"
	putc linker
	code_size = loc: dd 0
	data_size = loc: dd 0
	uninitialized_data_size = loc: dd 0
	entry_point = loc: dd 0
	dd codebase
	dd database
	dd &h400000 'image base
	dd &h1000 'section alignment
	dd 512 'file alignment
	dd 5 'OS version
	dd 0 'image version
	dd 5 'subsystem version
	dd 0 'reserved
	dd imagsize
	dd headsize
	dd 0
	if console then dw 3 else dw 2
	dw &h8400
	dd &h100000 'stack reserve
	dd &h1000 'stack commit
	dd &h100000 'heap reserve
	dd &h1000 'heap commit
	dd 0 'loaderflags
	dd 16 'number of rva
	data_dir = loc: pad 16*8
end sub
sub section(name,memsize,memloc,filesize,fileloc,flags)
	num_sections = num_sections + 1
	splice_w section_count,num_sections
	if len(name) > 8 then err.Raise 5,"section","Section name too large-- must be 8 characters or less"
	putc name
	pad 8-len(name)
	dd memsize
	dd memloc
	dd filesize
	dd fileloc
	pad 12
	if flags = "code" then
		dw &h20
		dw &h6000
	elseif flags = "data" then
		dw &h40
		dw &hc000
	elseif flags = "undat" then
		dw &h80
		dw &hc000
	end if
end sub
code_start = 0
data_start = 0
end_size = 0
sub jw_exe(codepages,datapages)
	putc "HERE HAVE A CANTALOU" 'just for kicks and giggles
	exe_header "JW",&h1000,&h2000,&h3000,&h200,false
	section "compiled",&h200*codepages,&h1000,&h200*codepages,&h200,"code"
	data_start = &h200*(codepages+1)
	section "variable",&h200*datapages,&h2000,&h200*datapages,data_start,"data"
	end_size = &h200*(codepages+datapages+1)
	pad_until &h200
	code_start = loc
end sub
jw_imports = ""
sub jw_import(x,y,h)
	allimports = Split(jw_imports,vbcrlf)
	jw_imports = ""
	newdll = true
	for i = 0 to ubound(allimports)-1
		if Split(allimports(i),",")(0) = x then
			jw_imports = jw_imports & allimports(i) & "," & chr(h mod 256) & chr(int(h / 256)) & y & vbcrlf
			newdll = false
		else
			jw_imports = jw_imports & allimports(i) & vbcrlf
		end if
	next
	if newdll then jw_imports = jw_imports & x & "," & chr(h mod 256) & chr(int(h / 256)) & y & vbcrlf
end sub
sub jw_entry()
	allimports = Split(jw_imports,vbcrlf)
	if ubound(allimports) > 0 then
		imp_s = loc-code_start+&h1000
		imp_l = 20*(ubound(allimports))
		splice_d data_dir+8,imp_s
		splice_d data_dir+60,imp_s
		splice_d data_dir+12,imp_l
		splice_d data_dir+64,imp_l
		strtable = imp_s+imp_l+20
		for i = 0 to ubound(allimports)-1
			dd 0 'OriginalFirstThunk
			dd 0 'TimeDateStamp
			dd 0 'ForwarderChain
			dd strtable 'DLL Name
			strtable = strtable + instr(allimports(i),",")
			if strtable mod 4 > 0 then strtable = strtable + 4-(strtable mod 4)
			dd strtable 'FirstThunk
			strtable = strtable + (ubound(Split(allimports(i),","))+1)*4
		next
		pad 20 'end of import table
		allstrs = ""
		for i = 0 to ubound(allimports)-1
			allfuncs = Split(allimports(i),",")
			puts allfuncs(0)
			pad_align 4
			for j = 1 to ubound(allfuncs)
				thunks = thunks & mid(allfuncs(j),3) & ":" & (loc-code_start+&h401000) & vbcrlf
				dd strtable
				allstrs = allstrs & allfuncs(j) & chr(0)
				strtable = strtable + len(allfuncs(j))+1
			next
			dd 0
		next
		puts allstrs
	end if
	splice_d entry_point,loc-code_start+&h1000
end sub
sub jw_data()
	if data_start < loc then Err.Raise 5,"jw_data","Code " & (loc-data_start) & " bytes too big for code section!"
	splice_d code_size,loc-code_start
	pad_until data_start
end sub
sub jw_end()
	if end_size < loc then Err.Raise 5,"jw_data","Data " & (loc-end_size) & " bytes too big for data section!"
	pad_until end_size
	jw_imports = ""
	data_start = 0
	code_start = 0
	end_size = 0
	num_sections = 0
end sub
function jw_offset(x)
	allmarks = Split(marks,vbcrlf)
	for i = 0 to ubound(allmarks)-1
		if Split(allmarks(i),":")(0) = x then
			mrk = cint(Split(allmarks(i),":")(1))
			if mrk > data_start then jw_offset = mrk-data_start+&h402000 else jw_offset = mrk-code_start+&h401000
			exit function
		end if
	next
end function

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' here
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

dos_stub "You're running this with DOS?? In the 21st century? WHYYY"
jw_exe 2,1
	mark "version"
		puts "win32-1.0"
	mark "filename"
		puts "spark.q"
	pad_align 8
	jw_import "kernel32.dll","CreateFileA",&h89
	jw_import "kernel32.dll","GetFileSize",&h1ef
	jw_import "kernel32.dll","GetProcessHeap",&h249
	jw_import "kernel32.dll","HeapAlloc",&h2cc
	jw_import "kernel32.dll","HeapCreate",&h2ce
	jw_import "kernel32.dll","HeapReAlloc",&h2d3
	jw_import "kernel32.dll","ReadFile",&h3c0
	jw_import "kernel32.dll","CloseHandle",&h53
	jw_import "kernel32.dll","LoadLibraryA",&h33d
	jw_import "kernel32.dll","FreeLibrary",&h163
	jw_entry
		sub_ esp,88
		mov_dword_ptr esp,0,jw_offset("filename")
		mov_dword_ptr esp,4,&h80000000
		mov_dword_ptr esp,8,1
		mov_dword_ptr esp,12,0
		mov_dword_ptr esp,16,3
		mov_dword_ptr esp,20,&h80
		mov_dword_ptr esp,24,0
		mov_dword_ptr esp,32,0
		mov_dword_ptr esp,40,0
		mov_dword_ptr esp,64,0
		mov_dword_ptr esp,76,&h40000
		mov_dword_ptr esp,80,0
		mov_dword_ptr esp,84,0
		call_ "CreateFileA"
		cmp eax,-1
		longje "exit"
		mov_dword_ptr esp,0,eax
		mov_dword_ptr esp,20,eax
		mov_dword_ptr esp,40,eax
		call_ "GetFileSize"
		cmp eax,-1
		je "exit"
		mov_dword_ptr esp,8,eax
		mov_dword_ptr esp,20,eax
		call_ "GetProcessHeap"
		mov_dword_ptr esp,0,eax
		call_ "HeapAlloc"
		mov esi,eax
		mov_dword_ptr esp,4,esi
		mov_dword_ptr esp,12,esp
		add_dword_ptr esp,12,24
		call_ "ReadFile"
		call_ "CloseHandle"
		add esp,4
		call_ "HeapCreate"
		mov_dword_ptr &h402000,0,eax
		mov edi,&h402004
		mark "newshard"
			mov al,1
			push edi
			mark "loop"
				mov ebx,edi
				add edi,8
				jmp "skipfirst"
				mark "strloop"
					cmp_byte_ptr esi,-1,0
					je "end_hit"
					mark "skipfirst"
					cmpsb
				je "strloop"
				sub_ esi,edi
				add esi,ebx
				add esi,8
				mov_dword_ptr edi,ebx,0
				cmp edi,0
			jne "loop"
				add esp,4
				ret
			mark "end_hit"
				pop edi
				mov_dword_ptr ebx,ebx,4
				call_ ebx
				cmp esi,0
			jne "newshard"
		mark "exit"
		ret
		
		mark "ifversion"
			test al,1
			jz "iv_parse"
				loc_ifv_jmp = loc
				mov_dword_ptr ecx,esi,0
				add esi,4
				push edi
				mov edi,jw_offset("version")
				jmp "iv_skipfirst"
				mark "iv_loop"
					cmp_byte_ptr esi,-1,0
					je "iv_endhit"
					mark "iv_skipfirst"
					cmpsb
				je "iv_loop"
					'not the right version! do the skip!
				mark "iv_endhit"
					'right version!
				add ebx,0
				'splice_b loc-1,loc-loc_ifv
				loc_ifv_e = loc
				jmp "iv_end" 'leave; if the IV is parsed, copying it is totally pointless
			mark "iv_parse"
				

			'however if it's not parsed, it needs to be copied so it can be followed later
			test al,4
			jz "iv_params"
				'copy params
			mark "iv_params"

			test al,8
			jz "iv_code"
				xchg ebx,esi
				xchg_dword_ptr edx,12,edi
				mov_dword_ptr edi,0,(loc_ifv_e-loc_ifv_jmp)*&h1000000 + &h7401a8
				add edi,4
				add esi,4
				mov ecx, (loc_ifv_e-loc_ifv_jmp)
				rep_movsb
				xchg ebx,esi
				xchg_dword_ptr edx,12,edi
			mark "iv_code"
			mark "iv_end"
		ret
		mark "shard"
			
		ret
		mark "shard_zero"
			
		ret
		mark "public"
			
		ret
	jw_data
		dd 0 'the executable heap
		'dictionary - starts at 0x402004
		dd &h402018
		dd jw_offset("ifversion")
		puts "ifversion" 'uses 3 dwords (0xc)
		pad_align 4
		dd 0
		dd jw_offset("shard")
		puts "shard" 'uses 2 dwords (0x8)
		pad_align 4
		'0x402028 - shard keywords
		dd &h402034
		dd jw_offset("shard_zero")
		dd 0 'zero shard
		dd &h402044
		dd jw_offset("public")
		puts "public" '0x8
		pad_align 4
		dd &h402054
		'dd jw_offset("
jw_end
writefile "quartz.exe"

puts "msgbox"
puts "This is a message, derp"
writefile "spark.q"	

''''''''''''''''''''''''''''''''''

thunks = ""

dos_stub "Yes, I display a weird message when you run me in DOS, what are you going to do about it? Go bother some other windows executable. I don't have time for this."
putc "NOT A TEXT FILE, SILLY A"
exe_header "SL",&h1000,&h2000,&h2000,&h400,false
section "hurrdurr",&h400,&h1000,&h400,&h400,"code"
end_size = &h800
pad_until &h400
code_start = loc
data_start = &h800
	mark "cmdpath"
		puts "C:\WINDOWS\system32\cmd.exe"
	mark "derp"
		puts "Derp Derp Derp"
	mark "startupinfo"
		dw 68
		pad 64
	pad_align 8
	jw_import "kernel32.dll","CreateProcessA",&hd5
	jw_import "kernel32.dll","ExitProcess",&h14f
	jw_import "kernel32.dll","Sleep",&h550
	jw_import "kernel32.dll","CreateThread",&he6
	jw_import "kernel32.dll","WaitForSingleObject",&h5a9
	jw_import "user32.dll","AttachThreadInput",&hd
	jw_import "user32.dll","RegisterHotKey",&h2b0
	jw_import "user32.dll","GetMessageA",&h173
	jw_import "user32.dll","GetAsyncKeyState",&h000
	jw_import "user32.dll","UnregisterHotKey",&h36c
	jw_import "user32.dll","MapVirtualKeyA",&h24b
	jw_import "user32.dll","keybd_event",&h399
	jw_import "user32.dll","EnumWindows",&h102
	jw_import "user32.dll","SetWindowPos",&h330
	jw_import "user32.dll","GetForegroundWindow",&h147
	jw_import "user32.dll","SetForegroundWindow",&h2f5
	jw_import "user32.dll","GetWindowThreadProcessId",&h1d9
	pad_align 8
	jw_entry
		push 0
		pop edi
		call_ "GetForegroundWindow"
		push eax
		sub_ esp,16
		push esp
		push jw_offset("startupinfo")
		putc "WWWWWWW"
		push jw_offset("cmdpath")
		call_ "CreateProcessA"
		mov_dword_ptr esp,8,0
		mov_dword_ptr ebx,esp,0
		mark "tryfind"
		push esp
		push &h12345678
		EnumProc = loc-4
		call_ "EnumWindows"
		mov_dword_ptr esi,esp,8
		cmp esi,0
		je "tryfind"
		add esp,16
		mark "waitactive"
		call_ "GetForegroundWindow"
		cmp eax,esi
		jne "waitactive"
		putc "WW"
		push &h85
		push &h7a
		push edi
		push 2
		push &h85
		push &h7a
		call_ "keybd_event"
		call_ "keybd_event"
		push 1
		push &h29
		call_ "MapVirtualKeyA"
		push eax
		push &h4000
		putc "WW"
		call_ "RegisterHotKey"
		pop eax
		sub_ esp,24
		mov ebx,esp
		push eax
		mark "msgloop"
		putc "WWW"
		push ebx
		call_ "Sleep"
		pop eax
		pop ecx
		cmp eax,259
		jne "exited"
		push ebx
		call_ "GetAsyncKeyState"
		test eax,&h00008001
		jz "conloopz"
		'Tilde is pressed
		test edi,2
		jnz "conloop"
		'Press Backspace
		push 0
		push 0
		push &h0e
		push 8
		push 0
		push 2
		push &h0e
		push 8
		call_ "keybd_event"
		call_ "keybd_event"
		test edi,1
		
		jz "open_window"
			push eax
			push &h4293
			push 2
			pop edi
			jmp "closed_window"
				mark "exited"
				push ecx
				call_ "ExitProcess"
		mark "open_window"
			call_ "GetForegroundWindow"
			push eax
			push &h4243
			push 3
			pop edi
		mark "closed_window"
		push 0
		push 0
		push 0
		push 0
		push 0
		push esi
		call_ "SetWindowPos"
		test edi,1
		jnz "bringtofront"
			pop eax
			call_ "SetForegroundWindow"
			longjmp "conloop"
			
		mark "bringtofront"
			push esi
			call_ "SetForegroundWindow"
			longjmp "conloop"
		
		mark "EnumProc"
		splice_d EnumProc,jw_offset("EnumProc")
			pop ecx
			pop eax
			pop edx
			push ecx
			push eax
			push edx
			push 0
			push eax
			call_ "GetWindowThreadProcessId"
			pop edx
			mov_dword_ptr ecx,edx,12
			cmp eax,ecx
			je "found_it"
				pop eax
				push 1
				pop eax
			ret
			mark "found_it"
				pop eax
				mov_dword_ptr edx,8,eax
				push 0
				pop eax
			ret
splice_d code_size,loc-code_start
jw_end
writefile "conbind.exe"