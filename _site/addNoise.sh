# https://www.imagemagick.org/discourse-server/viewtopic.php?t=21381#p87308
convert ./out/test.png -set colorspace RGB -separate -seed 1000 -attenuate 0.5 +noise gaussian -combine ./out/testMonoG.png
