var rng = function(initialSeed) {
	var seed = initialSeed >>> 0
	return function() {
		// linear congruent bla bla bla
		var bits = (seed & 0x7FFF0000) >>> 16
		seed = (multiply_uint32(seed, 1103515245) + 12345) & 0xFFFFFFFF
		return bits
	}
}

// like... jeez, can you multiply two numbers!
function multiply_uint32(a, b) {
    var ah = (a >> 16) & 0xffff, al = a & 0xffff;
    var bh = (b >> 16) & 0xffff, bl = b & 0xffff;
    var high = ((ah * bl) + (al * bh)) & 0xffff;
    return ((high << 16)>>>0) + (al * bl);
    // PS: thanks SO ~:]
}

// test: rng(17) should produce 0,24107,16552,12125,9427,13152,21440,3383,6873,16117...
