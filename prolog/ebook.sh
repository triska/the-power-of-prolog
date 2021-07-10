#!/bin/bash

ebook_dir="./ebook"
content="$ebook_dir/content.opf"
manifest="$ebook_dir/manifest"
spine="$ebook_dir/spine"
toc="$ebook_dir/toc.ncx"

echo "dir: $ebook_dir"

if [ -d "$ebook_dir" ]; then
	rm -rf "$ebook_dir"
fi
mkdir "$ebook_dir"

cp "kindle.css" "$ebook_dir/kindle.css"

for chapter in *.html; do
	echo "chapter: $chapter"
	chapter_file="$ebook_dir/$chapter"
	echo "    <item id=\"$chapter\" href=\"$chapter\" media-type=\"application/html\"/>" >> "$manifest"
	echo "    <itemref idref=\"$chapter\"/>" >> "$spine"
	awk '{\
gsub(" style=\"[^\"]+?\"","");\
gsub("<script src=\".+\"></script>","");\
sub("<link rel=\"stylesheet\" type=\"text/css\" href=\".*toc.css\">","");\
sub("<link rel=\"stylesheet\" type=\"text/css\" href=\".*prolog.css\">","<link rel=\"stylesheet\" type=\"text/css\" href=\"kindle.css\">");\
print\
}' "$chapter" > "$chapter_file"
done

echo '<?xml version="1.0" encoding="utf-8"?>
<package version="2.0" unique-identifier="BookId" xmlns="http://www.idpf.org/2007/opf">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">
    <dc:identifier opf:scheme="UUID" id="BookId">urn:uuid:ca006611-474d-43a3-9a76-c06b86ac020b</dc:identifier>
    <dc:creator opf:role="aut">Markus Triska</dc:creator>
    <dc:title>The Power of Prolog</dc:title>
    <dc:language>en</dc:language>
    <dc:date opf:event="modification" xmlns:opf="http://www.idpf.org/2007/opf">2018-05-28</dc:date>
  </metadata>
  <manifest>' >> "$content"
echo '<item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml"/>' >> "$content"
cat "$manifest" >> "$content"
echo '  </manifest>
  <spine toc="ncx">' >> "$content"
cat "$spine" >> "$content"
echo '  </spine>
  <guide>
  </guide>
</package>' >> "$content"

rm "$manifest" "$spine"

echo '<?xml version="1.0" encoding="UTF-8" standalone="no" ?>
<!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN" "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd">
<ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1" xml:lang="en">
    <!-- Metadata Section -->
    <!-- Title and Author Section -->
    <!-- Navigation Map Section -->
</ncx>' >> "$toc"

echo "prepared ebook data, zip files in ebook directory and rename file to .epub"