@echo off
cls
tcc.exe -run src/main.c %* > test.c && tcc -run test.c
