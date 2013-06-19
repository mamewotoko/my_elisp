#!/usr/local/bin/ruby

# $B%G!<%?%Y!<%9$,%a%b%jCf$KE83+$5$l$?;~$NBg$-$5$r35;;$9$k(B
# malloc $B$N%X%C%@$J$I$O9MN8$7$F$$$J$$$N$G!":G0-$3$NG\$0$i$$$r9M$($k$H$$$$$+$b(B

FileName = ARGV[0];  # last_record1_ $B$r;XDj$9$k(B

xstrs = Array[];
keys = Array[];
nSection = 0;
nColumns = 0;
nValues = 0;
nKeys = 0;
nKeysLen = 0;
nIntern = 0;
nInternLen = 0;
nUnintern = 0;
nUninternLen = 0;

f = File.new(FileName, "r");
f.each() { |l|
	if (l =~ /^---/) then
		# section $B$N@hF,(B
		nSection = nSection + 1;
		next;
	end
	nColumns = nColumns + 1;
	if (l =~ /^[+-]/) then
		# LRU$B$N%U%i%0(B
		l = l[1 .. -1];
	end
	a = l.split(/\s+/);
	keys.push a.shift;
	a.each { |s|
		nValues = nValues + 1;
		if (s =~ /^\"([^"]+)\"$/) then
			xstrs.push $1;
		end
	}
}
nUnintern = xstrs.length;
nUninternLen = xstrs.join().length;
xstrs.uniq!;
nIntern = xstrs.length;
nInternLen = xstrs.join().length;
nKeys = keys.length;
nKeysLen = keys.join().length;

print "section: ", nSection, "\n";
print "column: ", nColumns, "\n";
print "keys: ", nKeys, "\n";
print "keys length: ", nKeysLen, "\n";
print "values: ", nValues, "\n";
print "interned xstr: ", nIntern, "\n";
print "interned xstr total length: ", nInternLen, "\n";
print "(no intern simulation) xstr: ", nUnintern, "\n";
print "(no intern simulation) total length: ", nUninternLen, "\n";
print "total size: ", (nColumns + nIntern) * 48 + nInternLen * 2 + nValues * 8 + + nKeysLen * 2, "\n";
