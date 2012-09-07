.class public Template
.super java/lang/Object

.method public <init>()V
    aload_0
    invokenonvirtual java/lang/Object/<init>()V
    return
.end method

.method public static main([Ljava/lang/String;)V
    .limit stack 10
    .limit locals 3

    ; Pointer
    iconst_0
    istore_1
    
    ; Create inital tape
    bipush 100
    newarray int 
    astore_2

    ; >
    ; Increment the pointer
    iinc 1 1

    ; <
    ; Decrement the pointer
    iinc 1 -1

    ; +
    ; Increment the byte at the pointer
    aload_2
    iload_1
    dup2
    iaload
    bipush 1
    iadd
    iastore 

    ; -
    ; Decrement the byte at the pointer
    aload_2
    iload_1
    dup2
    iaload
    bipush -1
    iadd
    iastore 

    ; ,
    ; Input a byte and store it in the byte at the pointer
    aload_2
    iload_1
    getstatic java/lang/System/in Ljava/io/InputStream;
    invokevirtual java/io/InputStream/read()I
    iastore 

    ; .
    ; Output the byte at the pointer
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload_2
    iload_1
    iaload
    i2c
    invokevirtual java/io/PrintStream/print(C)V

    return 
.end method

;;; Instructions ;;;
;    ; [
;    ; Start loop: Execute delimited code until the byte at the pointer equals zero
;    aload_2
;    iload_1
;    iaload
;    ifeq 2
;
;    ; ]
;    ; End loop: Jump back to the matching [
;
;    ; #
;    ; Dump the values of a[0] thru a[9] to the console. 
;
