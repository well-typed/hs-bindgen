
default: libconstructor_import_issue.so

.PHONY: clean
clean:
	rm -f *.so *.dll *.o

libconstructor_import_issue.so: constructor_import_issue.o
	gcc  -Wall -shared -o libconstructor_import_issue.so constructor_import_issue.o

constructor_import_issue.o: constructor_import_issue.c constructor_import_issue.h
	gcc -Wall -o constructor_import_issue.o -c -fPIC constructor_import_issue.c

