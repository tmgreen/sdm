version := 0.1.0
srcname := sdm-$(version)-src

srcdist:
	rm -rf ./$(srcname)
	mkdir -p $(srcname)
	svn export src $(srcname)/src
	svn export project $(srcname)/project
	svn export build.sbt $(srcname)/build.sbt
	jar cMvf $(srcname).zip $(srcname)

bindist:
	sbt assembly
