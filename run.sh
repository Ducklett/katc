tcc -run src/main.c $@ > test.c && tcc -run test.c
